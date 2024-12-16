module Test.Daytripper
  ( Expect
  , expectBefore
  , expectDuring
  , expectAfter
  , mkExpect
  , runExpect
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

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged, untag)
import Options.Applicative (flag', help, long)
import PropUnit (Gen, PropertyT, TestLimit, forAll, setupTests, testProp, testUnit, (===))
import System.Directory (doesFileExist)
import Test.Tasty (TestName, TestTree, askOption, defaultIngredients, defaultMainWithIngredients, includingOptions)
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), safeRead)

-- | A general type of test expectation. Captures two stages of processing an input,
-- first encoding, then decoding. The monad is typically something implementing
-- 'MonadExpect', with assertions performed before returning values for further processing.
-- The input is possibly missing, in which case we test decoding only.
type Expect m a b c = Either b a -> m (b, m c)

eitherMay :: Either b a -> Maybe a
eitherMay = either (const Nothing) Just

-- | Assert something before processing (before encoding and before decoding)
expectBefore :: (Monad m) => (Maybe a -> m ()) -> Expect m a b c -> Expect m a b c
expectBefore f ex i = f (eitherMay i) >> ex i

-- | Assert something during processing (after encoding and before decoding)
expectDuring :: (Monad m) => (Maybe a -> b -> m ()) -> Expect m a b c -> Expect m a b c
expectDuring f ex i = ex i >>= \p@(b, _) -> p <$ f (eitherMay i) b

-- | Asserting something after processing (after encoding and after decoding)
expectAfter :: (Monad m) => (Maybe a -> b -> c -> m ()) -> Expect m a b c -> Expect m a b c
expectAfter f ex i = ex i >>= \(b, end) -> end >>= \c -> (b, pure c) <$ f (eitherMay i) b c

-- | A way of definining expectations from a pair of encode/decode functions and
-- a comparison function.
mkExpect
  :: (Monad m)
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
runExpect :: (Monad m) => Expect m a b c -> a -> m c
runExpect f a = f (Right a) >>= snd

data PropRT where
  PropRT :: (Show a) => TestName -> Expect (PropertyT IO) a b c -> Gen a -> PropRT

-- | Create a property-based roundtrip test
mkPropRT :: (Show a) => TestName -> Expect (PropertyT IO) a b c -> Gen a -> RT
mkPropRT name expec gen = RTProp (PropRT name expec gen)

testPropRT :: TestLimit -> PropRT -> TestTree
testPropRT lim (PropRT name expec gen) =
  testProp name lim (forAll gen >>= void . runExpect expec)

data FileRT where
  FileRT
    :: TestName
    -> Expect (PropertyT IO) a ByteString c
    -> FilePath
    -> Maybe a
    -> FileRT

-- | Create a file-based ("golden") roundtrip test
mkFileRT
  :: TestName
  -> Expect (PropertyT IO) a ByteString c
  -> FilePath
  -> Maybe a
  -> RT
mkFileRT name expec fn mval = RTFile (FileRT name expec fn mval)

testFileRT :: FileRT -> TestTree
testFileRT (FileRT name expec fn mval) = askOption $ \dwm ->
  testUnit name $ do
    exists <- liftIO (doesFileExist fn)
    (mcon, eval) <-
      if exists
        then do
          con <- liftIO (BS.readFile fn)
          pure (Just con, maybe (Left con) Right mval)
        else case (dwm, mval) of
          (DaytripperWriteMissing True, Just val) -> pure (Nothing, Right val)
          _ -> fail ("File missing: " ++ fn)
    (bs, end) <- expec eval
    for_ mcon (bs ===)
    _ <- end
    case mcon of
      Nothing -> liftIO (BS.writeFile fn bs)
      Just _ -> pure ()

data UnitRT where
  UnitRT :: TestName -> Expect (PropertyT IO) a b c -> a -> UnitRT

-- | Create a unit roundtrip test
mkUnitRT :: TestName -> Expect (PropertyT IO) a b c -> a -> RT
mkUnitRT name expec val = RTUnit (UnitRT name expec val)

testUnitRT :: UnitRT -> TestTree
testUnitRT (UnitRT name expec val) =
  testUnit name (void (runExpect expec val))

data RT
  = RTProp !PropRT
  | RTFile !FileRT
  | RTUnit !UnitRT

-- | Run a roundtrip test
testRT :: TestLimit -> RT -> TestTree
testRT lim = \case
  RTProp x -> testPropRT lim x
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
daytripperMain :: (TestLimit -> TestTree) -> IO ()
daytripperMain f = do
  lim <- setupTests
  defaultMainWithIngredients daytripperIngredients (f lim)
