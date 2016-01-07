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
  randomIdx <- randomRIO (0, length wl)
  return $ wl !! (randomIdx - 1)


randomWord' :: IO String
randomWord' = gameWords >>= randomWord

type Solution = String
type Found = [Maybe Char]
type Guessed = [Char]

data Puzzle = Puzzle Solution Found Guessed
instance Show Puzzle where
  show (Puzzle a found guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar found)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: Solution -> Puzzle
freshPuzzle word = Puzzle word (fmap (const Nothing) word) []

charInSolution :: Puzzle -> Char -> Bool
charInSolution (Puzzle solution _ _) guess = guess `elem` solution

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) guess = guess `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just m) = m
renderPuzzleChar _ = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar guessed) guess =
  Puzzle word newFilledInSoFar (guess:guessed)
  where newFilledInSoFar = zipWith (zipper guess) word filledInSoFar
        zipper guess wordChar guessedChar = if wordChar == guess
                                            then Just wordChar
                                            else guessedChar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInSolution puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> putStrLn "You already guessed that,\
                          \ pick something else!" >> return puzzle
    (True, _) -> do
      putStrLn "Nice job! Filling in your guess now"
      return $ fillInCharacter puzzle guess
    (False, _) -> do
      putStrLn "Nope -- try again"
      return $ fillInCharacter puzzle guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle solution _ guessed) =
  if (length guessed) > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ solution
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ found _) =
  if all isJust found
  then putStrLn "You win!" >> exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- randomWord'
  runGame $ freshPuzzle (fmap toLower word)

