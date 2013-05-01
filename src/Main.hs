{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad       (forM_, void, when)
import Control.Monad.Trans (liftIO)

import           Data.IORef
import qualified Data.Text  as T

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Reactive.Banana
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

  simNewDialog <- builderGetObject builder castToDialog "simNewDialog"
  newGameImageMenuItem <- builderGetObject builder castToMenuItem "newGameImageMenuItem"
  quitGameImageMenuItem <- builderGetObject builder castToMenuItem "quitGameImageMenuItem"

  simAboutDialog <- builderGetObject builder castToDialog "simAboutDialog"
  aboutHelpImageMenuItem <- builderGetObject builder castToMenuItem "aboutHelpImageMenuItem"

  simStatusBar <- builderGetObject builder castToStatusbar "simStatusBar"

  simFrame <- builderGetObject builder castToFrame "simFrame"
  widgetSetCanFocus simFrame True
  simCanvas <- drawingAreaNew
  containerAdd simFrame simCanvas

  let a = mkPlayer "Player A" A Human
  let b = mkPlayer "Player B" B Human
  board <- newIORef $ Board a b MoveA

  simCanvas `on` exposeEvent $ liftIO $ do d@(w, h) <- widgetGetSize simCanvas
                                           simDrawing <- widgetGetDrawWindow simCanvas
                                           renderWithDrawable simDrawing $
                                             drawBoard (fromIntegral w, fromIntegral h)
                                           simFrame `on` keyPressEvent $
                                             do key <- eventKeyName
                                                let allowedKeys = map (show . (+1) . fromEnum) $ enumFrom One
                                                when (elem key allowedKeys) $
                                                  do let v = toEnum ((read key :: Int) - 1) :: Vertex
                                                     board' <- liftIO $ readIORef board
                                                     board'' <- liftIO $ gameLoop simCanvas v board'
                                                     liftIO $ writeIORef board board''
                                                return True
                                           return True

  timeoutAdd (do b <- readIORef board
                 cid <- statusbarGetContextId simStatusBar "Sim board status"
                 statusbarPush simStatusBar cid $ statusBarText b
                 return True
             ) 100

  network <- compile $ do
    eNewGame <- event0 newGameImageMenuItem menuItemActivate
    eQuitGame <- event0 quitGameImageMenuItem menuItemActivate
    eAboutGame <- event0 aboutHelpImageMenuItem menuItemActivate
    eDeleted <- eventM simWindow deleteEvent eventSent

    reactimate $ void (dialogRun simNewDialog) <$ eNewGame
    reactimate $ mainQuit <$ eQuitGame
    reactimate $ void (dialogRun simAboutDialog) <$ eAboutGame
    reactimate $ mainQuit <$ eDeleted

  actuate network
  widgetShowAll simWindow
  mainGUI

statusBarText :: Board -> String
statusBarText b =
  case status b of
    WinA -> pnA ++ " wins!"
    WinB -> pnB ++ " wins!"
    MoveA -> pnA ++ ": " ++ show (startVertices $ validMoves pA)
    MoveB -> pnB ++ ": " ++ show (startVertices $ validMoves pB)
    HalfMoveA v -> pnA ++ ": " ++ show (verticesToFrom v $ validMoves pA)
    HalfMoveB v -> pnB ++ ": " ++ show (verticesToFrom v $ validMoves pB)
  where pnA = T.unpack $ playerName pA
        pnB = T.unpack $ playerName pB
        pA = playerA b
        pB = playerB b
