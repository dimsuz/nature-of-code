module Walker where
import Prelude hiding (Left, Right)
import System.Random
import Data.Random.Normal
import Diagrams.Prelude
import Diagrams.Backend.Cairo

type State = ([P2],[Double])

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

randToDir :: Double -> Direction
randToDir n
          | n < 0.4 = Right
          | n < 0.6 = Left
          | n < 0.8 = Down
          | otherwise = Up

walkDirectionStep :: [Double] -> P2 -> ([Double], P2)
walkDirectionStep (r:rs) p = (rs, nextPoint)
    where nextPoint = walk (randToDir r) p

walkStep :: [Double] -> P2 -> ([Double], P2)
walkStep (dx:dy:rs) p = (rs, nextPoint)
    where nextPoint = translate (dx ^& dy) p

walkRandomStep :: [Double] -> P2 -> ([Double], P2)
walkRandomStep (rstep:r1:r2:rs) p = (rs, nextPoint)
    where nextPoint = translate ((r1*stepsize) ^& (r2*stepsize)) p
          stepsize = rstep * 10

initState :: IO State
initState = do
  randomGen <- newStdGen
  return ([p2 (0,0)], randomRs (0.0,1.0) randomGen)

walkFn :: [Double] -> P2 -> ([Double], P2)
-- walkFn rs p = walkDirectionStep rs p
-- walkFn rs p = walkStep rs p
walkFn rs p = walkRandomStep rs p

updateState :: State -> State
updateState ([],(d:ds)) = ([(0 ^& 0)], ds)
updateState (ap@(p:ps), rs) = (nextPoint : ap, restRs)
                              where (restRs, nextPoint) = walkFn rs p

drawState :: State -> Diagram B R2
drawState state =  (square 100) <> position (zip (fst state) (repeat dot))
    where dot = circle 0.5 # fc green # lw none
