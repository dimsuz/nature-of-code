{-# LANGUAGE NoMonomorphismRestriction #-}
import Graphics.UI.Gtk
import Diagrams.Prelude
import Diagrams.Backend.Gtk
import Diagrams.Backend.Cairo
import Control.Monad.Trans (liftIO)

renderFigure :: DrawingArea -> EventM EExpose Bool
renderFigure canvas= do
  liftIO $ defaultRender canvas figure
  return True

figure :: Diagram Cairo R2
figure =  circle 2


type State = [Int]

update :: DrawingArea -> State -> IO Bool
update canvas state = do
  putStrLn "updating"
  -- TODO change state MVar here
  widgetQueueDraw canvas
  return True

main :: IO ()
main = do
  initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  canvas `on` sizeRequest $ return (Requisition 256 256)
  canvas `on` exposeEvent $ renderFigure canvas
  set window [windowDefaultWidth := 600, windowDefaultHeight := 400,
              containerChild := canvas, containerBorderWidth := 8]
  onDestroy window mainQuit
  widgetShowAll window
  timeoutAdd (update canvas [1..3]) 500
  mainGUI
