{-# LANGUAGE NoMonomorphismRestriction #-}
import Graphics.UI.Gtk
import Diagrams.Prelude
import Diagrams.Backend.Gtk
import Diagrams.Backend.Cairo
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar
import Walker

type State = [P2]

renderFigure :: MVar State -> DrawingArea -> EventM EExpose Bool
renderFigure state canvas = do
  s <- liftIO $ readMVar state
  --liftIO $ putStrLn ("state" ++ (show s))
  liftIO $ defaultRender canvas (figure s)
  return True

figure :: State -> Diagram B R2
figure state =  position (zip state (repeat dot))
    where dot = circle 0.5 # fc green

update :: DrawingArea -> MVar State -> IO Bool
update canvas state = do
  putStrLn "updating"
  modifyMVar_ state (\s -> return (if length s <= 5 then (p2 (fromIntegral $ length s,0)):s else s))
  widgetQueueDraw canvas
  return True

main :: IO ()
main = do
  state <- newMVar [p2 (0,0)]
  initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  canvas `on` sizeRequest $ return (Requisition 256 256)
  canvas `on` exposeEvent $ renderFigure state canvas
  set window [windowDefaultWidth := 600, windowDefaultHeight := 400,
              containerChild := canvas, containerBorderWidth := 8]
  onDestroy window mainQuit
  widgetShowAll window
  timeoutAdd (update canvas state) 500
  mainGUI
