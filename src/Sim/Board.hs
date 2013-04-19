module Sim.Board
       ( drawBoard
       , drawMove
       ) where

import Control.Monad            (forM_)
import Graphics.Rendering.Cairo

import Sim.Types

radius :: Double
radius = 150

theta :: Double
theta = (2*pi)/n
  where n = fromIntegral $ length $ enumFrom One

vertex :: Dim -> Int -> Dim
vertex (w, h) x = (w/2 + radius*sin (x'*theta), h/2 + radius*cos (x'*theta))
  where x' = fromIntegral x

drawBoard :: Dim -> Render ()
drawBoard d@(w, h) =
  forM_ (map fromEnum $ enumFrom One) $ \x -> do
    let (p, q) = vertex d x
    arc p q 15 0 (2*pi)
    stroke
    moveTo (p - 3) (q + 3)
    showText $ show $ x + 1
    newPath

drawMove :: Dim -> Player -> Move -> Render ()
drawMove d@(w, h) p (Line v1 v2) = do
  let (x1, y1) = vertex d $ fromEnum v1
  let (x2, y2) = vertex d $ fromEnum v2
  moveTo x1 y1
  case playerId p of
    A -> setSourceRGB 1 0 0
    B -> setSourceRGB 0 0 1
  lineTo x2 y2
  stroke
