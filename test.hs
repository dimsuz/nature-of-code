{-# LANGUAGE NoMonomorphismRestriction #-}
import Graphics.UI.Gtk
import Diagrams.Backend.Gtk
import Diagrams.Backend.Cairo
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar
-- import a particular module here which does init/update/draw implementations
import Walker as M

renderFigure :: MVar State -> DrawingArea -> EventM EExpose Bool
renderFigure state canvas = do
  s <- liftIO $ readMVar state
  --liftIO $ putStrLn ("state" ++ (show s))
  liftIO $ defaultRender canvas (M.drawState s)
  return True

update :: DrawingArea -> MVar State -> IO Bool
update canvas state = do
  modifyMVar_ state (\s -> return $ M.updateState s)
  widgetQueueDraw canvas
  return True

main :: IO ()
main = do
  instate <- M.initState
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
