module Walker where
import Prelude hiding (Left, Right)
import System.Random
import Diagrams.Prelude
import Diagrams.Backend.Cairo

type State = ([P2],[Float])

data Direction = Left | Right | Up | Down
               deriving (Show, Enum, Bounded)

instance Random Direction where
    randomR (a,b) g =
        case randomR (fromEnum a, fromEnum b) g of
          (x, g') -> (toEnum x, g')
    random g = randomR (minBound, maxBound) g

walk :: Direction -> P2 -> P2
walk d p = case d of
             Left -> translateX (-1) p
             Right -> translateX 1 p
             Up -> translateY 1 p
             Down -> translateY (-1) p

randToDir :: Float -> Direction
randToDir n
          | n < 0.4 = Right
          | n < 0.6 = Left
          | n < 0.8 = Down
          | otherwise = Up

initState :: IO State
initState = do
  randomGen <- newStdGen
  return ([p2 (0,0)], randomRs (0.0,1.0) randomGen)

updateState :: State -> State
updateState ([],(d:ds)) = ([(0 ^& 0)], ds)
updateState (ap@(p:ps), (d:ds)) = (walk dir p : ap, ds)
                                  where dir = randToDir d

drawState :: State -> Diagram B R2
drawState state =  (square 100) <> position (zip (fst state) (repeat dot))
    where dot = circle 0.5 # fc green # lw none
