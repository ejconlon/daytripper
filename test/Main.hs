{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (isJust, isNothing)
import Test.Daytripper (Expect, MonadExpect (..), daytripperMain, mkExpect, mkFileRT, mkPropRT, mkUnitRT, testRT)
import Test.Falsify.Generator qualified as Gen
import Test.Tasty (testGroup)

type Cmp m = Maybe ByteString -> Maybe ByteString -> m ()

expec :: (MonadExpect m) => Cmp m -> Expect m ByteString ByteString (Maybe ByteString)
expec = mkExpect enc dec
 where
  enc a = pure (a <> a)
  dec b =
    pure $
      let a = BS.take (div (BS.length b) 2) b
      in  if b == a <> a
            then Just a
            else Nothing

expecOk, expecFail :: (MonadExpect m) => Expect m ByteString ByteString (Maybe ByteString)
expecOk =
  expec $
    maybe
      (expectAssertBool "expected Just" . isJust)
      (\a mc -> expectAssertEq mc (Just a))
expecFail = expec (const (expectAssertBool "expected Nothing" . isNothing))

main :: IO ()
main =
  daytripperMain $
    testGroup "Daytripper" $
      fmap
        testRT
        [ mkPropRT "prop" expecOk (Gen.choose (pure "a") (pure "b"))
        , mkUnitRT "unit" expecOk "a"
        , mkFileRT "file just" expecOk "testdata/b.txt" (Just "b")
        , mkFileRT "file nothing" expecOk "testdata/c.txt" Nothing
        , mkFileRT "file fail" expecFail "testdata/x.txt" Nothing
        ]
