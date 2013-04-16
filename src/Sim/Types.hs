module Sim.Types
       ( Board(..)
       , Player(..)
       , Move(..)
       , PlayerId(..)
       , Dim
       ) where

import Data.Text
import Text.Show.Functions

data Board = Board { playerA :: Player
                   , playerB :: Player
                   , status  :: Status
                   }
           deriving (Show)

type Dim = (Double, Double)

data Status = MoveA
            | HalfMoveA
            | MoveB
            | HalfMoveB
            | WinA
            | WinB
            deriving (Enum, Eq, Show)

data Player = Player { playerName :: Text
                     , playerId   :: PlayerId
                     , curMoves   :: [Move]
                     , validMoves :: [Move]
                     , makeMove   :: Move -> Player
                     }
            deriving (Show)

data PlayerId = A | B
              deriving (Enum, Eq, Show)

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
