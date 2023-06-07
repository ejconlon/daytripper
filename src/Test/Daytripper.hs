module Test.Daytripper
  ( LiftIO
  , AssertEq
  , Ctx (..)
  , unitCtx
  , propCtx
  , Expect
  , expect
  , RT
  , propRT
  , fileRT
  , unitRT
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

type LiftIO m = forall a. IO a -> m a

type AssertEq m = forall a. (Eq a, Show a) => a -> a -> m ()

data Ctx m = Ctx
  { ctxLiftIO :: !(LiftIO m)
  , ctxAssertEq :: !(AssertEq m)
  }

unitCtx :: Ctx IO
unitCtx = Ctx id (@?=)

propAssertEq :: AssertEq Property
propAssertEq x y = FP.assert (FR.eq FR..$ ("LHS", x) FR..$ ("RHS", y))

propCtx :: Ctx Property
propCtx = Ctx (pure . unsafePerformIO) propAssertEq

type Expect m a b = a -> m (b, m ())

expect :: (MonadFail m, Eq a, Show a) => Ctx m -> (a -> m b) -> (b -> m (Maybe a)) -> Expect m a b
expect ctx f g a = do
  b <- f a
  pure . (b,) $ do
    ma' <- g b
    case ma' of
      Nothing -> fail "Failed roundtrip"
      Just a' -> ctxAssertEq ctx a' a

runExpect :: Monad m => Expect m a () -> a -> m ()
runExpect f a = f a >>= snd

data PropRT where
  PropRT :: Show a => TestName -> Expect Property a () -> Gen a -> PropRT

propRT :: Show a => TestName -> Expect Property a () -> Gen a -> RT
propRT name expec gen = RTProp (PropRT name expec gen)

testPropRT :: PropRT -> TestTree
testPropRT (PropRT name expec gen) =
  testProperty name (FP.gen gen >>= runExpect expec)

data FileRT where
  FileRT
    :: TestName
    -> FilePath
    -> Expect IO a ByteString
    -> a
    -> FileRT

fileRT
  :: TestName
  -> FilePath
  -> Expect IO a ByteString
  -> a
  -> RT
fileRT name fn expec val = RTFile (FileRT name fn expec val)

mkFileTripper
  :: DaytripperWriteMissing
  -> FilePath
  -> Expect IO a ByteString
  -> Expect IO a ()
mkFileTripper (DaytripperWriteMissing wm) fp expec val = do
  exists <- doesFileExist fp
  mcon <-
    if exists
      then fmap Just (BS.readFile fp)
      else
        if wm
          then pure Nothing
          else fail ("File missing: " <> fp)
  (bs, end) <- expec val
  pure . ((),) $ do
    end
    case mcon of
      Nothing -> BS.writeFile fp bs
      Just con -> bs @?= con

testFileRT :: FileRT -> TestTree
testFileRT (FileRT name fp expec val) = askOption $ \dwm ->
  testCase name (runExpect (mkFileTripper dwm fp expec) val)

data UnitRT where
  UnitRT :: TestName -> Expect IO a () -> a -> UnitRT

unitRT :: TestName -> Expect IO a () -> a -> RT
unitRT name expec val = RTUnit (UnitRT name expec val)

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
