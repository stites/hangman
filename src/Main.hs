module Main where

import Hello

main :: IO ()
main = do
  name <- getLine -- getLine :: IO String
  sayHello name

