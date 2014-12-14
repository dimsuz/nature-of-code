module Walker where
import Prelude hiding (Left, Right)
import System.Random
import Diagrams.Prelude(P2,translateX,translateY, (^&))

type Walker = P2

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

updateState :: ([P2],[Direction]) -> ([P2],[Direction])
updateState ([],(d:ds)) = ([(0 ^& 0)], ds)
updateState (ap@(p:ps), (d:ds)) = (walk d p : ap, ds)
