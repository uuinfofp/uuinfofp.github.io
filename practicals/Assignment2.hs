{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

{-# LANGUAGE TupleSections #-} {- A handy syntax extension. See:
  https://downloads.haskell.org/ghc/9.8.1/docs/users_guide/exts/tuple_sections.html
-}

-- Rename to "Main" if you want to compile the game.
-- Don't forget to rename it back when submitting!
module Assignment2 where

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import System.IO

-- | Rose trees

data Rose a = Node a [Rose a]
  deriving (Eq, Show)

-- Exercise 1

root :: Rose a -> a
root = undefined

children :: Rose a -> [Rose a]
children = undefined

-- Exercise 2

size :: Rose a -> Int
size = undefined

leaves :: Rose a -> Int
leaves = undefined

-- | State representation

-- * Players

data Player = P1 | P2
  deriving (Eq, Ord)

instance Show Player where
  show P1 = "Player 1"
  show P2 = "Player 2"

-- Exercise 3

nextPlayer :: Player -> Player
nextPlayer = undefined

-- * Board

data Field = X | O | B
  deriving (Eq, Ord)

instance Show Field where
  show X = "X"
  show O = "O"
  show B = " "

-- Exercise 4

symbol :: Player -> Field
symbol = undefined

type Row   = (Field, Field, Field)
type Board = (Row, Row, Row)

-- Exercise 5

verticals :: Board -> (Row, Row, Row)
verticals = undefined

diagonals :: Board -> (Row, Row)
diagonals = undefined

-- Exercise 6

emptyBoard :: Board
emptyBoard = undefined

-- Exercise 7

printBoard :: Board -> String
printBoard = undefined

-- | Move generation

-- Exercise 8

moves :: Player -> Board -> [Board]
moves = undefined

-- | Gametree generation

-- Exercise 9

hasWinner :: Board -> Maybe Player
hasWinner = undefined

-- Exercise 10

gameTree :: Player -> Board -> Rose Board
gameTree = undefined

-- | Game complexity

-- Exercise 11

gameTreeComplexity :: Int
gameTreeComplexity = undefined

-- | Minimax

-- * Scores

data Score = Win | Draw | Lose
  deriving (Eq, Ord)

instance Show Score where
  show Win  = "+1"
  show Draw =  "0"
  show Lose = "-1"

-- Exercise 12

minimax :: Player -> Rose Board -> Rose Score
minimax = undefined

-- * Lazier minimum and maximums

-- Exercise 13

minimum' :: [Score] -> Score
minimum' = undefined

maximum' :: [Score] -> Score
maximum' = undefined

-- | Gameplay

-- Exercise 14

makeMove :: Player -> Board -> Maybe Board
makeMove = undefined

-- | Main

data PlayerType = Human | Computer

instance Show PlayerType where
  show Human    = "H"
  show Computer = "C"

main :: IO ()
main = do
  typeOfP1 <- askFor "Should Player 1 be a (H)uman or a (C)omputer player?"
    [Human, Computer]
  typeOfP2 <- askFor "Should Player 2 be a (H)uman or a (C)omputer player?"
    [Human, Computer]

  let
    playerType :: Player -> PlayerType
    playerType P1 = typeOfP1
    playerType P2 = typeOfP2

    gameLoop :: Player -> Board -> IO ()
    gameLoop p b = do
      putStrLn ("\n" ++ printBoard b)
      case hasWinner b of
        Just p  -> putStrLn (show p ++ " has won!")
        Nothing -> do
          putStr   ("It's " ++ show p ++ "'s turn. ")
          mb' <- case playerType p of
            Human    -> humanMove    p b
            Computer -> computerMove p b
          case mb' of
            Nothing -> putStrLn "No more moves are possible. It's a draw."
            Just b' -> gameLoop (nextPlayer p) b'

    humanMove :: Player -> Board -> IO (Maybe Board)
    humanMove p b = case moves p b of
      [] -> return Nothing
      possibleMoves -> do
        putStrLn "Possible moves are:"
        putStrLn (listMoves possibleMoves)
        i <- askFor "Make your choice:" [1..length possibleMoves]
        return (Just (possibleMoves !! (i - 1)))

    computerMove :: Player -> Board -> IO (Maybe Board)
    computerMove p b = do
      putStrLn "Thinking..."
      return (makeMove p b)

    listMoves :: [Board] -> String
    listMoves = intercalate "\n"
      . map (intercalate "    ")
      . transpose
      . map lines
      . zipWith printMove [1..]

    printMove :: Int -> Board -> String
    printMove i b = "(" ++ show i ++ "): \n" ++ printBoard b

  gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
  putStr (m ++ " ")
  hFlush stdout
  i <- getLine
  let similar a b = map toLower a == map toLower b
  case find (similar i . show) xs of
    Nothing -> do
      putStrLn $ "I didn't understand you. Enter one of: "
        ++ intercalate ", " (map show xs) ++ "."
      askFor m xs
    Just y -> return y