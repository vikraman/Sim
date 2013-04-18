module Main where

import Data.List
import Test.HUnit

import Sim.Game  (initMoves, updateValidMoves)
import Sim.Types (Move (..), Vertex (..))

main = runTestTT tests

tests = TestList [TestLabel "test1" test1]
test1 = TestCase (assertEqual "compute invalid moves" validMoves' (updateValidMoves curMoves validMoves)
                 )

curMoves = [(Line One Two), (Line One Five)]
validMoves = delete (Line One Two) $
             delete (Line One Five) initMoves
validMoves' = delete (Line Two Five) validMoves
