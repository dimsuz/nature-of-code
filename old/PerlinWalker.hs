module PerlinWalker where
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import System.Random
import Numeric.Noise.Perlin

type State = (P2, Double, Double)

perlinNoise seed = perlin seed octaves scale persistance
              where octaves = 4
                    scale = 0.05
                    persistance = 0.5

mapRange :: (Num a, Fractional a) => a -> (a,a) -> (a,a) -> a
mapRange val (imin,imax) (omin,omax) =
    omin + (omax - omin) * ((val - imin) / (imax - imin))

initState :: IO State
initState = do
  return (p2 (0,0), 0, 0)

updateState :: State -> State
updateState (p, tx, ty) = ((dx ^& dy), tx + 0.3, ty + 0.3)
                     where dx = mapRange n1 (0.0,1.0) (0.0,15.0)
                           dy = mapRange n2 (0.0,1.0) (0.0,15.0)
                           n1 = noiseValue noise1 (tx,0,0)
                           n2 = noiseValue noise2 (ty,0,0)
                           noise1 = perlinNoise 1
                           noise2 = perlinNoise 10000

drawState :: State -> Diagram B R2
drawState (p, _, _) = (square 30) <> position [(p, dot)]
    where dot = circle 3 # fc gray
