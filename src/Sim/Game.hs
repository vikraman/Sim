module Sim.Game
       ( mkPlayer
       , handleInput
       , gameLoop
       ) where

import Control.Monad.Trans (liftIO)
import Data.List
import Data.Text
import Graphics.UI.Gtk

import Sim.Board
import Sim.Types

initMoves :: [Move]
initMoves = [ Line x y
            | x <- enumFrom One
            , y <- enumFrom x
            , x /= y
            ]

mkPlayer :: Text -> PlayerId -> Player
mkPlayer name pid = Player name pid [] initMoves mkMove

mkMove :: Player -> Move -> Player
mkMove p m@(Line x y) =
  p { validMoves = delete m $ validMoves p
    , curMoves = m : curMoves p
    }

handleInput :: DrawingArea -> Vertex -> Board -> IO Board
handleInput canvas v b =
  case status b of
    MoveA -> return b { status = HalfMoveA v }
    MoveB -> return b { status = HalfMoveB v }
    HalfMoveA v' -> do let m = Line v v'
                       let pA' = mkMove (playerA b) m
                       d@(w, h) <- widgetGetSize canvas
                       drawing <- widgetGetDrawWindow canvas
                       renderWithDrawable drawing $
                         drawMove (fromIntegral w, fromIntegral h) pA' m
                       return b { status = MoveB }
    HalfMoveB v' -> do let m = Line v v'
                       let pB' = mkMove (playerB b) m
                       d@(w, h) <- widgetGetSize canvas
                       drawing <- widgetGetDrawWindow canvas
                       renderWithDrawable drawing $
                         drawMove (fromIntegral w, fromIntegral h) pB' m
                       return b { status = MoveA }
    _ -> return b

gameLoop :: IO ()
gameLoop = return ()
