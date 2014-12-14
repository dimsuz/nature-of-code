{-# LANGUAGE NoMonomorphismRestriction #-}
import Graphics.UI.Gtk
import Diagrams.Prelude
import Diagrams.Backend.Gtk
import Diagrams.Backend.Cairo
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar
import System.Random
import Walker

renderFigure :: MVar State -> DrawingArea -> EventM EExpose Bool
renderFigure state canvas = do
  s <- liftIO $ readMVar state
  --liftIO $ putStrLn ("state" ++ (show s))
  liftIO $ defaultRender canvas (figure s)
  return True

figure :: State -> Diagram B R2
figure state =  (square 100) <> position (zip (fst state) (repeat dot))
    where dot = circle 0.5 # fc green # lw none

update :: DrawingArea -> MVar State -> IO Bool
update canvas state = do
  modifyMVar_ state (\s -> return $ updateState s)
  widgetQueueDraw canvas
  return True

main :: IO ()
main = do
  instate <- initState
  stateVar <- newMVar instate
  initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  canvas `on` sizeRequest $ return (Requisition 256 256)
  canvas `on` exposeEvent $ renderFigure stateVar canvas
  set window [windowDefaultWidth := 600, windowDefaultHeight := 400,
              containerChild := canvas, containerBorderWidth := 8]
  onDestroy window mainQuit
  widgetShowAll window
  timeoutAdd (update canvas stateVar) 50
  mainGUI
