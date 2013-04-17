{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad       (forM_, void)
import Control.Monad.Trans (liftIO)
import Data.IORef

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk

import Paths_Sim
import Sim.Board
import Sim.Game
import Sim.Types

main :: IO ()
main = do
  initGUI

  builder <- builderNew
  getDataFileName "src/sim.glade" >>= builderAddFromFile builder

  simWindow <- builderGetObject builder castToWindow "simWindow"
  simWindow `on` deleteEvent $ liftIO mainQuit >> return False

  simNewDialog <- builderGetObject builder castToDialog "simNewDialog"
  newGameImageMenuItem <- builderGetObject builder castToMenuItem "newGameImageMenuItem"
  quitGameImageMenuItem <- builderGetObject builder castToMenuItem "quitGameImageMenuItem"

  simAboutDialog <- builderGetObject builder castToDialog "simAboutDialog"
  aboutHelpImageMenuItem <- builderGetObject builder castToMenuItem "aboutHelpImageMenuItem"

  simFrame <- builderGetObject builder castToFrame "simFrame"
  widgetSetCanFocus simFrame True
  simCanvas <- drawingAreaNew
  containerAdd simFrame simCanvas

  let a = mkPlayer "Player A" A
  let b = mkPlayer "Player B" B
  board <- newIORef $ Board a b MoveA

  simCanvas `on` exposeEvent $ liftIO $ do d@(w, h) <- widgetGetSize simCanvas
                                           simDrawing <- widgetGetDrawWindow simCanvas
                                           renderWithDrawable simDrawing $
                                             drawBoard (fromIntegral w, fromIntegral h)
                                           simFrame `on` keyPressEvent $
                                             do key <- eventKeyName
                                                if elem key $ map show [1..6]
                                                  then do let v = toEnum ((read key :: Int) - 1) :: Vertex
                                                          board' <- liftIO $ readIORef board
                                                          board'' <- liftIO $ handleInput simCanvas v board'
                                                          liftIO $ writeIORef board board''
                                                  else return ()
                                                return True
                                           return True

  timeoutAdd ((readIORef board >>= print . status) >> return True) 1000

  network <- compile $ do
    eNewGame <- event0 newGameImageMenuItem menuItemActivate
    eQuitGame <- event0 quitGameImageMenuItem menuItemActivate
    eAboutGame <- event0 aboutHelpImageMenuItem menuItemActivate

    reactimate $ fmap (const $ void $ dialogRun simNewDialog) eNewGame
    reactimate $ fmap (const $ mainQuit) eQuitGame
    reactimate $ fmap (const $ void $ dialogRun simAboutDialog) eAboutGame

  actuate network
  widgetShowAll simWindow
  mainGUI
