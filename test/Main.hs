{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  )
where

import Data.ByteString (ByteString)
import Test.Daytripper (Expect, MonadExpect (..), daytripperMain, fileRT, propRT, testRT, unitRT)
import Test.Falsify.Generator qualified as Gen
import Test.Tasty (testGroup)

expec :: MonadExpect m => Expect m ByteString ByteString
expec = error "TODO"

main :: IO ()
main =
  daytripperMain $
    testGroup "Daytripper" $
      fmap
        testRT
        [ propRT "prop" expec (Gen.choose (pure "a") (pure "b"))
        , unitRT "unit" expec "a"
        , fileRT "file" expec "testdata/b.txt" "b"
        ]
