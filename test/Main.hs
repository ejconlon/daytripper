{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Test.Daytripper (Expect, MonadExpect (..), daytripperMain, mkExpect, mkFileRT, mkPropRT, mkUnitRT, testRT)
import Test.Falsify.Generator qualified as Gen
import Test.Tasty (testGroup)

expec :: MonadExpect m => Expect m ByteString ByteString (Maybe ByteString)
expec = mkExpect enc dec
 where
  enc a = pure (a <> a)
  dec b =
    pure $
      let a = BS.take (div (BS.length b) 2) b
      in  if b == a <> a
            then Just a
            else Nothing

main :: IO ()
main =
  daytripperMain $
    testGroup "Daytripper" $
      fmap
        testRT
        [ mkPropRT "prop" expec (Gen.choose (pure "a") (pure "b"))
        , mkUnitRT "unit" expec "a"
        , mkFileRT "file" expec "testdata/b.txt" "b"
        ]
