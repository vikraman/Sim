module Main where

import Control.Monad       (forM_, void)
import Control.Monad.Trans (liftIO)

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Paths_Sim

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk

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
  nameEntry  <- builderGetObject builder castToEntry "nameEntry"
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

  simWindow `on` keyPressEvent $ do keyv1 <- eventKeyName
                                    liftIO $ do simWindow `on` keyPressEvent $ do keyv2 <- eventKeyName
                                                                                  liftIO $ do (w, h) <- widgetGetSize simCanvas
                                                                                              simDrawing <- widgetGetDrawWindow simCanvas
                                                                                              renderWithDrawable simDrawing $
                                                                                                drawL keyv1 keyv2 (fromIntegral w) (fromIntegral h)
                                                                                              return False
                                                return False

  simCanvas `on` buttonPressEvent $ do (x1,y1) <- eventCoordinates
                                       liftIO $ do simCanvas `on` buttonReleaseEvent $ do (x2,y2) <- eventCoordinates
                                                                                          liftIO $ do (w, h) <- widgetGetSize simCanvas
                                                                                                      simDrawing <- widgetGetDrawWindow simCanvas
                                                                                                      renderWithDrawable simDrawing $
                                                                                                        drawLin (fromIntegral w) (fromIntegral h) x1 y1 x2 y2
                                                                                                      return False
                                                   return False

  --network <- compile $ do
    --(w,h) <- widgetGetSize simCanvas
    --simDrawing <- widgetGetDrawWindow simCanvas
    --eChanged <- event0 nameEntry editableChanged
    --reactimate $ fmap (const $ drawMe simDrawing nameEntry w h) eChanged

  --actuate network
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

--drawMe simDrawing nameEntry w h = do
 -- renderWithDrawable simDrawing $
   -- drawL (fromIntegral w) (fromIntegral h)
  --return False
drawL :: String -> String -> Double -> Double -> Render()
drawL keyv1 keyv2 w h = do
  let r = 150
  let t = pi/3
  let k1 = read keyv1 :: Double
  let k2 = read keyv2 :: Double
  moveTo (w/2 + r*sin (k1*t)) (h/2 + r*cos (k1*t))
  lineTo (w/2 + r*sin (k2*t)) (h/2 + r*cos (k2*t))
  stroke
  newPath
               {-else if keyv=="1" then do moveTo (w/2 + r*sin(1*t)-15) (h/2 + r*cos (1*t))
                                         lineTo (w/2 + r*sin (3*t)) (h/2 + r*cos (3*t)+15)
                                   else if keyv=="2" then do moveTo (w/2 + r*sin(1*t)-15) (h/2 + r*cos (1*t))
                                                           lineTo (w/2 + r*sin (4*t)) (h/2 + r*cos (4*t)+15)
                                                   else if keyv=="3" then do moveTo (w/2 + r*sin(1*t)-15) (h/2 + r*cos (1*t))
                                                                             lineTo (w/2 + r*sin (5*t)) (h/2 + r*cos (5*t)+15)
                                                                     else if keyv=="4" then do moveTo (w/2 + r*sin(1*t)-15) (h/2 + r*cos (1*t))
                                                                                               lineTo (w/2 + r*sin (6*t)) (h/2 + r*cos (6*t)+15)
                                                                                   else if keyv=="5" then do moveTo (w/2 + r*sin(2*t)-15) (h/2 + r*cos (2*t))
                            lineTo (w/2 + r*sin (3*t)) (h/2 + r*cos (3*t)+15)
  else if keyv=="6" then do moveTo (w/2 + r*sin(2*t)-15) (h/2 + r*cos (2*t))
                            lineTo (w/2 + r*sin (4*t)) (h/2 + r*cos (4*t)+15)
  else if keyv=="7" then do moveTo (w/2 + r*sin(2*t)-15) (h/2 + r*cos (2*t))
                            lineTo (w/2 + r*sin (5*t)) (h/2 + r*cos (5*t)+15)
  else if keyv=="8" then do moveTo (w/2 + r*sin(2*t)-15) (h/2 + r*cos (2*t))
                            lineTo (w/2 + r*sin (6*t)) (h/2 + r*cos (6*t)+15)
  else if keyv=="9" then do moveTo (w/2 + r*sin(3*t)-15) (h/2 + r*cos (3*t))
                            lineTo (w/2 + r*sin (4*t)) (h/2 + r*cos (4*t)+15)
  else if keyv=="a" then do moveTo (w/2 + r*sin(3*t)-15) (h/2 + r*cos (3*t))
                            lineTo (w/2 + r*sin (5*t)) (h/2 + r*cos (5*t)+15)
  else if keyv=="b" then do moveTo (w/2 + r*sin(3*t)-15) (h/2 + r*cos (3*t))
                            lineTo (w/2 + r*sin (6*t)) (h/2 + r*cos (6*t)+15)
  else if keyv=="c" then do moveTo (w/2 + r*sin(4*t)-15) (h/2 + r*cos (4*t))
                            lineTo (w/2 + r*sin (5*t)) (h/2 + r*cos (5*t)+15)
  else if keyv=="d" then do moveTo (w/2 + r*sin(4*t)-15) (h/2 + r*cos (4*t))
                            lineTo (w/2 + r*sin (6*t)) (h/2 + r*cos (6*t)+15)
  else do moveTo (w/2 + r*sin(5*t)-15) (h/2 + r*cos (5*t))
          lineTo (w/2 + r*sin (6*t)) (h/2 + r*cos (6*t)+15)-}

drawLin w h x1 y1 x2 y2 = do
  let r = 150
  let t = pi/3
  if x1>(w/2 + r*sin(t)-15) && x1 <(w/2 + r*sin(t)+15) && y1>(h/2 + r*cos (t)-15) && y1<(h/2 + r*cos (t)+15) && x2>(w/2 + r*sin(6*t)-15) && x2 <(w/2 + r*sin(6*t)+15) && y2>(h/2 + r*cos (6*t)-15) && y2<(h/2 + r*cos (6*t)+15)
    then do moveTo (w/2 + r*sin(t)-15) (h/2 + r*cos (t))
            lineTo (w/2 + r*sin (6*t)) (h/2 + r*cos (6*t)-15)
    else do moveTo (w/2 + r*sin(t)-15) (h/2 + r*cos (t))
            lineTo (w/2 + r*sin (6*t)) (h/2 + r*cos (6*t)-15)
  stroke
