{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (isJust, isNothing)
import Hedgehog.Gen qualified as Gen
import PropUnit (MonadTest, assert, (===))
import Test.Daytripper (Expect, daytripperMain, mkExpect, mkFileRT, mkPropRT, mkUnitRT, testRT)
import Test.Tasty (testGroup)

type Cmp m = Maybe ByteString -> Maybe ByteString -> m ()

expec :: (Monad m) => Cmp m -> Expect m ByteString ByteString (Maybe ByteString)
expec = mkExpect enc dec
 where
  enc a = pure (a <> a)
  dec b =
    pure $
      let a = BS.take (div (BS.length b) 2) b
      in  if b == a <> a
            then Just a
            else Nothing

expecOk, expecFail :: (MonadTest m) => Expect m ByteString ByteString (Maybe ByteString)
expecOk =
  expec $
    maybe
      (assert . isJust)
      (\a mc -> mc === Just a)
expecFail = expec (const (assert . isNothing))

main :: IO ()
main =
  daytripperMain $ \lim ->
    testGroup "Daytripper" $
      fmap
        (testRT lim)
        [ mkPropRT "prop" expecOk (Gen.element ["a", "b"])
        , mkUnitRT "unit" expecOk "a"
        , mkFileRT "file just" expecOk "testdata/b.txt" (Just "b")
        , mkFileRT "file nothing" expecOk "testdata/c.txt" Nothing
        , mkFileRT "file fail" expecFail "testdata/x.txt" Nothing
        ]
