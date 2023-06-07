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

class MonadFail m => MonadExpect m where
  expectLiftIO :: IO a -> m a
  expectAssertEq :: (Eq a, Show a) => a -> a -> m ()

instance MonadExpect IO where
  expectLiftIO = id
  expectAssertEq = (@?=)

instance MonadExpect Property where
  expectLiftIO = pure . unsafePerformIO
  expectAssertEq x y = FP.assert (FR.eq FR..$ ("LHS", x) FR..$ ("RHS", y))

type Expect m a b = a -> m (b, m ())

mkExpect :: (MonadExpect m, Eq a, Show a) => (a -> m b) -> (b -> m (Maybe a)) -> Expect m a b
mkExpect f g a = do
  b <- f a
  pure . (b,) $ do
    ma' <- g b
    case ma' of
      Nothing -> fail "Failed roundtrip"
      Just a' -> expectAssertEq a' a

runExpect :: Monad m => Expect m a b -> a -> m ()
runExpect f a = f a >>= snd

data PropRT where
  PropRT :: Show a => TestName -> Expect Property a b -> Gen a -> PropRT

mkPropRT :: Show a => TestName -> Expect Property a b -> Gen a -> RT
mkPropRT name expec gen = RTProp (PropRT name expec gen)

testPropRT :: PropRT -> TestTree
testPropRT (PropRT name expec gen) =
  testProperty name (FP.gen gen >>= runExpect expec)

data FileRT where
  FileRT
    :: TestName
    -> Expect IO a ByteString
    -> FilePath
    -> a
    -> FileRT

mkFileRT
  :: TestName
  -> Expect IO a ByteString
  -> FilePath
  -> a
  -> RT
mkFileRT name expec fn val = RTFile (FileRT name expec fn val)

mkFileExpect
  :: DaytripperWriteMissing
  -> Expect IO a ByteString
  -> FilePath
  -> Expect IO a ByteString
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
    end
    case mcon of
      Nothing -> BS.writeFile fn bs
      Just con -> bs @?= con

testFileRT :: FileRT -> TestTree
testFileRT (FileRT name fp expec val) = askOption $ \dwm ->
  testCase name (runExpect (mkFileExpect dwm fp expec) val)

data UnitRT where
  UnitRT :: TestName -> Expect IO a b -> a -> UnitRT

mkUnitRT :: TestName -> Expect IO a b -> a -> RT
mkUnitRT name expec val = RTUnit (UnitRT name expec val)

testUnitRT :: UnitRT -> TestTree
testUnitRT (UnitRT name expec val) =
  testCase name (runExpect expec val)

data RT
  = RTProp !PropRT
  | RTFile !FileRT
  | RTUnit !UnitRT

testRT :: RT -> TestTree
testRT = \case
  RTProp x -> testPropRT x
  RTFile x -> testFileRT x
  RTUnit x -> testUnitRT x

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

daytripperIngredients :: [Ingredient]
daytripperIngredients =
  includingOptions [Option (Proxy :: Proxy DaytripperWriteMissing)]
    : defaultIngredients

daytripperMain :: TestTree -> IO ()
daytripperMain = defaultMainWithIngredients daytripperIngredients
