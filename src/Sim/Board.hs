module Sim.Board
       ( drawBoard
       ) where

import Control.Monad (forM_)

import Graphics.Rendering.Cairo

drawBoard :: Double -> Double -> Render ()
drawBoard w h = do
  let r = 150
  let t = pi/3
  forM_ [1..6] $ \x -> do
    arc (w/2 + r*sin (x*t)) (h/2 + r*cos (x*t)) 15 0 (2*pi)
    stroke
    moveTo (w/2 + r*sin(x*t) - 3) (h/2 + r*cos (x*t) + 3)
    showText $ show $ floor x
    newPath
