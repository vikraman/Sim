module Sim.Game
       ( mkPlayer
       , gameLoop
       , initMoves
       , updateValidMoves
       , startVertices
       , verticesToFrom
       ) where

import           Control.Monad.Trans (liftIO)
import           Data.List
import qualified Data.Text           as T
import           Graphics.UI.Gtk

import Sim.AI
import Sim.Board
import Sim.Types

initMoves :: [Move]
initMoves = [ Line x y
            | x <- enumFrom One
            , y <- enumFrom x
            , x /= y
            ]

toMove :: Vertex -> Vertex -> Move
toMove v v' = if v < v'
              then Line v v'
              else Line v' v

mkPlayer :: T.Text -> PlayerId -> PlayerType -> Player
mkPlayer name pid pt = Player name pid pt [] initMoves

startVertex :: Move -> Vertex
startVertex (Line x _) = x

endVertex :: Move -> Vertex
endVertex (Line _ y) = y

endVertices :: [Move] -> [Vertex]
endVertices = nub . map endVertex

startVertices :: [Move] -> [Vertex]
startVertices ms = nub $ map startVertex ms `union` map endVertex ms

vertexFrom :: Vertex -> Move -> Bool
vertexFrom v (Line x y) | v == x = True
                        | otherwise = False

verticesToFrom :: Vertex -> [Move] -> [Vertex]
verticesToFrom v ms = delete v $ nub $ union p q
  where p = startVertices moves
        q = endVertices moves
        moves = filter (\m -> startVertex m == v ||
                              endVertex m == v
                       ) ms

endVerticesFrom :: Vertex -> [Move] -> [Vertex]
endVerticesFrom v ms = nub . map endVertex $ filter (vertexFrom v) ms

isValidMove :: Move -> Player -> Bool
isValidMove = (. validMoves) . elem

isLosing :: Player -> Bool
isLosing = null . validMoves

deleteMove :: Move -> [Move] -> [Move]
deleteMove = delete

updateValidMoves :: [Move] -> [Move] -> [Move]
updateValidMoves cms vms = foldr (\m vms ->
                                   if triangle m cms
                                   then delete m vms
                                   else vms
                                 ) vms vms

mkMove :: Board -> Player -> Move -> Board
mkMove b p m@(Line x y) =  b { playerA = pA'
                             , playerB = pB'
                             }
  where pA  = playerA b
        pB  = playerB b
        pA' =
          case playerId p of
            A -> pA { curMoves = curMoves'
                    , validMoves = updateValidMoves curMoves' validMoves'
                    }
            B -> pA { validMoves = validMoves'
                    }
          where curMoves' = m : curMoves pA
                validMoves' = deleteMove m $ validMoves pA
        pB' =
          case playerId p of
            A -> pB { validMoves = validMoves'
                    }
            B -> pB { curMoves = curMoves'
                    , validMoves = updateValidMoves curMoves' validMoves'
                    }
          where curMoves' = m : curMoves pB
                validMoves' = deleteMove m $ validMoves pB

renderMove :: DrawingArea -> Player -> Move -> IO ()
renderMove canvas p m  = do (w, h) <- widgetGetSize canvas
                            drawing <- widgetGetDrawWindow canvas
                            renderWithDrawable drawing $
                              drawMove (fromIntegral w, fromIntegral h) p m

gameLoop :: DrawingArea -> Vertex -> Board -> IO Board
gameLoop canvas v b =
  case status b of
    MoveA -> do
      let pA = playerA b
      let pB = playerB b
      case playerType pA of
        Human -> return b { status = HalfMoveA v }
        Computer -> do let m = play pA pB MoveA
                       renderMove canvas pA m
                       let b' = mkMove b pA m
                       return $ if isLosing $ playerA b'
                                then b' { status = WinB }
                                else b' { status = MoveB }
    MoveB -> do
      let pA = playerA b
      let pB = playerB b
      case playerType pB of
        Human -> return b { status = HalfMoveB v }
        Computer -> do let m = play pB pA MoveB
                       renderMove canvas pB m
                       let b' = mkMove b pB m
                       return $ if isLosing $ playerB b'
                                then b' { status = WinA }
                                else b' { status = MoveA }
    HalfMoveA v' -> do let p = playerA b
                       let m = toMove v v'
                       if isValidMove m p
                         then do renderMove canvas p m
                                 let b' = mkMove b p m
                                 return $ if isLosing $ playerA b'
                                          then b' { status = WinB }
                                          else b' { status = MoveB }
                         else return b { status = MoveA }
    HalfMoveB v' -> do let p = playerB b
                       let m = toMove v v'
                       if isValidMove m p
                         then do renderMove canvas p m
                                 let b' = mkMove b p m
                                 return $ if isLosing $ playerB b'
                                          then b' { status = WinA }
                                          else b' { status = MoveA }
                         else return b { status = MoveB }
    _ -> return b
