module Main where

import Data.HashMap

main :: IO ()
main = do
  let m = insert "c" "d" $ singleton "a" "b"
  print m
