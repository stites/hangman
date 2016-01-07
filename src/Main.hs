module Main where

import Control.Monad (forever)     -- infinite loop, not necessary
import Data.Char     (toLower)
import Data.Maybe    (isJust)
import Data.List     (intersperse)
import System.Exit   (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  all <- allWords
  return $ filter gameLength all
    where gameLength w =
            let l = length (w::String)
            in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIdx <- randomRIO (0, length wl - 1)
  return $ wl !! randomIdx


randomWord' :: IO String
randomWord' = gameWords >>= randomWord


