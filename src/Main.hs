module Main where

import System.IO

import Hello

main :: IO ()
main = do
  {- This way the next line will not be buffered/deferred. Otherwise, instead of:
   - $ Please input your name: <input>
   - >>> Hello <input>!
   -
   - We will get something like:
   - $ <input>
   - >>> Please input your name: Hello <input>!
   -}
  hSetBuffering stdout NoBuffering
  putStr "Please input your name: "
  name <- getLine -- getLine :: IO String
  sayHello name

