module Gaussian where
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import System.Random
import Data.Random.Normal

type State = ([P2], [Double])

initState :: IO State
initState = do
  randomGen <- newStdGen
  return ([p2 (0,0)], normals randomGen)

updateState :: State -> State
updateState ([],ds) = ([(0 ^& 0)], ds)
updateState (ap@(p:ps), (d:ds)) = ( (x ^& 0)  : ap, ds)
                                  where sd = 60
                                        mean = 0
                                        x = d * sd + mean

drawState :: State -> Diagram B R2
drawState state =  (square 640) <> position (zip points (repeat ellipse))
    where points = (fst state)
          ellipse = circle 16 # fcA (black `withOpacity` 0.1) # lw none
