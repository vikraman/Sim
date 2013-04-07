module Main where

import Control.Monad       (forM_, void)
import Control.Monad.Trans (liftIO)

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Paths_Sim

main :: IO ()
main = do
  initGUI

  builder <- builderNew
  getDataFileName "src/sim.glade" >>= builderAddFromFile builder

  simWindow <- builderGetObject builder castToWindow "simWindow"
  simWindow `on` deleteEvent $ liftIO mainQuit >> return False

  simNewDialog <- builderGetObject builder castToDialog "simNewDialog"
  newGameImageMenuItem <- builderGetObject builder castToMenuItem "newGameImageMenuItem"
  newGameImageMenuItem `on` menuItemActivate $ liftIO $ void (dialogRun simNewDialog)

  quitGameImageMenuItem <- builderGetObject builder castToMenuItem "quitGameImageMenuItem"
  quitGameImageMenuItem `on` menuItemActivate $ liftIO mainQuit

  simAboutDialog <- builderGetObject builder castToDialog "simAboutDialog"
  aboutHelpImageMenuItem <- builderGetObject builder castToMenuItem "aboutHelpImageMenuItem"
  aboutHelpImageMenuItem `on` menuItemActivate $ liftIO $ void (dialogRun simAboutDialog)

  simFrame <- builderGetObject builder castToFrame "simFrame"
  simCanvas <- drawingAreaNew
  containerAdd simFrame simCanvas
  simCanvas `on` exposeEvent $ liftIO $ do (w, h) <- widgetGetSize simCanvas
                                           simDrawing <- widgetGetDrawWindow simCanvas
                                           renderWithDrawable simDrawing $
                                             drawBoard (fromIntegral w) (fromIntegral h)
                                           return False

  widgetShowAll simWindow
  mainGUI

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
