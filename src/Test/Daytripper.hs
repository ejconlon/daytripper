module Test.Daytripper
  ( MonadExpect (..)
  , Expect
  , expectBefore
  , expectDuring
  , expectAfter
  , mkExpect
  , RT
  , mkPropRT
  , mkFileRT
  , mkUnitRT
  , testRT
  , DaytripperWriteMissing (..)
  , daytripperIngredients
  , daytripperMain
  )
where

import Control.Monad (unless, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged, untag)
import Options.Applicative (flag', help, long)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Test.Falsify.Generator (Gen)
import Test.Falsify.Predicate qualified as FR
import Test.Falsify.Property (Property)
import Test.Falsify.Property qualified as FP
import Test.Tasty (TestName, TestTree, askOption, defaultIngredients, defaultMainWithIngredients, includingOptions)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), safeRead)

-- | Interface for asserting and performing IO in tests.
-- TODO Migrate to 'MonadIO' superclass when Falsify supports it.
class MonadFail m => MonadExpect m where
  expectLiftIO :: IO a -> m a
  expectAssertEq :: (Eq a, Show a) => a -> a -> m ()
  expectAssertFailure :: String -> m ()
  expectAssertBool :: String -> Bool -> m ()
  expectAssertBool s b = unless b (expectAssertFailure s)

instance MonadExpect IO where
  expectLiftIO = id
  expectAssertEq = (@?=)
  expectAssertFailure = assertFailure
  expectAssertBool = assertBool

instance MonadExpect Property where
  expectLiftIO = pure . unsafePerformIO
  expectAssertEq x y = FP.assert (FR.eq FR..$ ("LHS", x) FR..$ ("RHS", y))
  expectAssertFailure = FP.testFailed

-- | A general type of test expectation. Captures two stages of processing an input,
-- first encoding, then decoding. The monad is typically something implementing
-- 'MonadExpect', with assertions performed before returning values for further processing.
-- The input is possibly missing, in which case we test decoding only.
type Expect m a b c = Either b a -> m (b, m c)

eitherMay :: Either b a -> Maybe a
eitherMay = either (const Nothing) Just

-- | Assert something before processing (before encoding and before decoding)
expectBefore :: Monad m => (Maybe a -> m ()) -> Expect m a b c -> Expect m a b c
expectBefore f ex i = f (eitherMay i) >> ex i

-- | Assert something during processing (after encoding and before decoding)
expectDuring :: Monad m => (Maybe a -> b -> m ()) -> Expect m a b c -> Expect m a b c
expectDuring f ex i = ex i >>= \p@(b, _) -> p <$ f (eitherMay i) b

-- | Asserting something after processing (after encoding and after decoding)
expectAfter :: Monad m => (Maybe a -> b -> c -> m ()) -> Expect m a b c -> Expect m a b c
expectAfter f ex i = ex i >>= \(b, end) -> end >>= \c -> (b, pure c) <$ f (eitherMay i) b c

-- | A way of definining expectations from a pair of encode/decode functions and
-- a comparison function.
mkExpect
  :: MonadExpect m
  => (a -> m b)
  -> (b -> m c)
  -> (Maybe a -> c -> m ())
  -> Expect m a b c
mkExpect f g h i = do
  b <- either pure f i
  pure . (b,) $ do
    c <- g b
    h (either (const Nothing) Just i) c
    pure c

-- | Simple way to run an expectation, ignoring the intermediate value.
runExpect :: Monad m => Expect m a b c -> a -> m c
runExpect f a = f (Right a) >>= snd

data PropRT where
  PropRT :: Show a => TestName -> Expect Property a b c -> Gen a -> PropRT

-- | Create a property-based roundtrip test
mkPropRT :: Show a => TestName -> Expect Property a b c -> Gen a -> RT
mkPropRT name expec gen = RTProp (PropRT name expec gen)

testPropRT :: PropRT -> TestTree
testPropRT (PropRT name expec gen) =
  testProperty name (FP.gen gen >>= void . runExpect expec)

data FileRT where
  FileRT
    :: TestName
    -> Expect IO a ByteString c
    -> FilePath
    -> Maybe a
    -> FileRT

-- | Create a file-based ("golden") roundtrip test
mkFileRT
  :: TestName
  -> Expect IO a ByteString c
  -> FilePath
  -> Maybe a
  -> RT
mkFileRT name expec fn mval = RTFile (FileRT name expec fn mval)

testFileRT :: FileRT -> TestTree
testFileRT (FileRT name expec fn mval) = askOption $ \dwm ->
  testCase name $ do
    exists <- doesFileExist fn
    (mcon, eval) <-
      if exists
        then do
          con <- BS.readFile fn
          pure (Just con, maybe (Left con) Right mval)
        else case (dwm, mval) of
          (DaytripperWriteMissing True, Just val) -> pure (Nothing, Right val)
          _ -> fail ("File missing: " ++ fn)
    (bs, end) <- expec eval
    for_ mcon (bs @?=)
    _ <- end
    case mcon of
      Nothing -> BS.writeFile fn bs
      Just _ -> pure ()

data UnitRT where
  UnitRT :: TestName -> Expect IO a b c -> a -> UnitRT

-- | Create a unit roundtrip test
mkUnitRT :: TestName -> Expect IO a b c -> a -> RT
mkUnitRT name expec val = RTUnit (UnitRT name expec val)

testUnitRT :: UnitRT -> TestTree
testUnitRT (UnitRT name expec val) =
  testCase name (void (runExpect expec val))

data RT
  = RTProp !PropRT
  | RTFile !FileRT
  | RTUnit !UnitRT

-- | Run a roundtrip test
testRT :: RT -> TestTree
testRT = \case
  RTProp x -> testPropRT x
  RTFile x -> testFileRT x
  RTUnit x -> testUnitRT x

-- | By passing the appropriate arguments to Tasty (`--daytripper-write-missing` or
-- `TASTY_DAYTRIPPER_WRITE_MISSING=True`) we can fill in the contents of missing files
-- with the results of running tests.
newtype DaytripperWriteMissing = DaytripperWriteMissing {unDaytripperWriteMissing :: Bool}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance IsOption DaytripperWriteMissing where
  defaultValue = DaytripperWriteMissing False
  parseValue = fmap DaytripperWriteMissing . safeRead
  optionName = return "daytripper-write-missing"
  optionHelp = return "Write missing test files"
  optionCLParser =
    DaytripperWriteMissing
      <$> flag'
        True
        ( long (untag (optionName :: Tagged DaytripperWriteMissing String))
            <> help (untag (optionHelp :: Tagged DaytripperWriteMissing String))
        )

-- | Tasty ingredients with write-missing support
daytripperIngredients :: [Ingredient]
daytripperIngredients =
  includingOptions [Option (Proxy :: Proxy DaytripperWriteMissing)]
    : defaultIngredients

-- | Tasty main with write-missing support
daytripperMain :: TestTree -> IO ()
daytripperMain = defaultMainWithIngredients daytripperIngredients
