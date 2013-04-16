module Sim.Types where

import Data.Text
import Text.Show.Functions

data Board = Board { playerA :: Player
                   , playerB :: Player
                   }
           deriving (Show)

data Player = Player { playerName :: Text
                     , curMoves   :: [Move]
                     , validMoves :: [Move]
                     , makeMove   :: Move -> Player
                     }
            deriving (Show)

data Move = Line Vertex Vertex
          deriving (Show)

instance Eq Move where
  (==) (Line p q) (Line r s) | (p == r) && (q == s) = True
                             | (p == s) && (q == r) = True
                             | otherwise = False

data Vertex = One
            | Two
            | Three
            | Four
            | Five
            | Six
            deriving (Enum, Eq, Show)
