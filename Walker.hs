module Walker where
import System.Random
import Diagrams.Prelude(P2)

type Walker = P2

data Direction = Left | Right | Up | Down
               deriving (Show, Enum, Bounded)

instance Random Direction where
    randomR (a,b) g =
        case randomR (fromEnum a, fromEnum b) g of
          (x, g') -> (toEnum x, g')
    random g = randomR (minBound, maxBound) g
