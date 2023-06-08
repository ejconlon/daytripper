module Test.Daytripper
  ( MonadExpect (..)
  , Expect
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

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
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
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), safeRead)

-- | Interface for asserting and performing IO in tests.
-- TODO Migrate to 'MonadIO' superclass when Falsify supports it.
class MonadFail m => MonadExpect m where
  expectLiftIO :: IO a -> m a
  expectAssertEq :: (Eq a, Show a) => a -> a -> m ()

instance MonadExpect IO where
  expectLiftIO = id
  expectAssertEq = (@?=)

instance MonadExpect Property where
  expectLiftIO = pure . unsafePerformIO
  expectAssertEq x y = FP.assert (FR.eq FR..$ ("LHS", x) FR..$ ("RHS", y))

-- | A general type of test expectation. Captures two stages of processing an input,
-- first encoding, then decoding. The monad is typically something implementing
-- 'MonadExpect', with assertions performed before returning values for further processing.
type Expect m a b c = a -> m (b, m c)

-- | One way of definining expectations from a pair of encode/decode functions.
-- Generalizes decoding in 'Maybe' or 'Either'.
mkExpect
  :: (MonadExpect m, Eq (f a), Show (f a), Applicative f)
  => (a -> m b)
  -> (b -> m (f a))
  -> Expect m a b (f a)
mkExpect f g a = do
  b <- f a
  pure . (b,) $ do
    fa <- g b
    expectAssertEq fa (pure a)
    pure fa

-- | Simple way to run an expectation, ignoring the intermediate value.
runExpect :: Monad m => Expect m a b c -> a -> m c
runExpect f a = f a >>= snd

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
    -> a
    -> FileRT

-- | Create a file-based ("golden") roundtrip test
mkFileRT
  :: TestName
  -> Expect IO a ByteString c
  -> FilePath
  -> a
  -> RT
mkFileRT name expec fn val = RTFile (FileRT name expec fn val)

mkFileExpect
  :: DaytripperWriteMissing
  -> Expect IO a ByteString c
  -> FilePath
  -> Expect IO a ByteString c
mkFileExpect (DaytripperWriteMissing wm) expec fn val = do
  exists <- doesFileExist fn
  mcon <-
    if exists
      then fmap Just (BS.readFile fn)
      else
        if wm
          then pure Nothing
          else fail ("File missing: " ++ fn)
  (bs, end) <- expec val
  pure . (bs,) $ do
    c <- end
    case mcon of
      Nothing -> BS.writeFile fn bs
      Just con -> bs @?= con
    pure c

testFileRT :: FileRT -> TestTree
testFileRT (FileRT name fp expec val) = askOption $ \dwm ->
  testCase name (void (runExpect (mkFileExpect dwm fp expec) val))

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
