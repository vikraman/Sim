module Main where

import Control.Monad       (void)
import Control.Monad.Trans (liftIO)

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

  widgetShowAll simWindow
  mainGUI
