module Sim.Game
       ( mkPlayer
       , handleInput
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

mkPlayer :: T.Text -> PlayerId -> Player
mkPlayer name pid = Player name pid [] initMoves mkMove

isValidMove :: Move -> Player -> Bool
isValidMove = (. validMoves) . elem

isWinning :: Player -> Bool
isWinning = null . validMoves

deleteMove :: Move -> [Move] -> [Move]
deleteMove = delete

updateValidMoves :: Move -> [Move] -> [Move]
updateValidMoves m vms = foldr deleteMove vms $ m : curMove m vms

mkMove :: Board -> Player -> Move -> Board
mkMove b p m@(Line x y) =  b { playerA = pA'
                             , playerB = pB'
                             }
  where pA  = playerA b
        pB  = playerB b
        pA' =
          case playerId p of
            A -> pA { curMoves = m : curMoves pA
                    , validMoves = updateValidMoves m $ validMoves pA
                    }
            B -> pA { validMoves = deleteMove m $ validMoves pA
                    }
        pB' =
          case playerId p of
            A -> pB { validMoves = deleteMove m $ validMoves pB
                    }
            B -> pB { curMoves = m : curMoves pB
                    , validMoves = updateValidMoves m $ validMoves pB
                    }

renderMove :: DrawingArea -> Player -> Move -> IO ()
renderMove canvas p m  = do (w, h) <- widgetGetSize canvas
                            drawing <- widgetGetDrawWindow canvas
                            renderWithDrawable drawing $
                              drawMove (fromIntegral w, fromIntegral h) p m

handleInput :: DrawingArea -> Vertex -> Board -> IO Board
handleInput canvas v b =
  case status b of
    MoveA -> return b { status = HalfMoveA v }
    MoveB -> return b { status = HalfMoveB v }
    HalfMoveA v' -> do let p = playerA b
                       let m = Line v v'
                       if isValidMove m p
                         then do renderMove canvas p m
                                 let b' = mkMove b p m
                                 return $ if isWinning $ playerA b'
                                          then b' { status = WinB }
                                          else b' { status = MoveB }
                         else return b { status = MoveA }
    HalfMoveB v' -> do let p = playerB b
                       let m = Line v v'
                       if isValidMove m p
                         then do renderMove canvas p m
                                 let b' = mkMove b p m
                                 return $ if isWinning $ playerB b'
                                          then b' { status = WinA }
                                          else b' { status = MoveA }
                         else return b { status = MoveB }
    _ -> return b
