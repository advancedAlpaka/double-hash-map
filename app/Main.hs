
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.HashMap
import Data.HashMap.Internal.Base
import Data.HashMap.Internal.Class
import Data.HashMap.Internal.Instance



main :: IO ()
main = do
  let m = set "c" "d" $ singleton "a" "b"
  print m
