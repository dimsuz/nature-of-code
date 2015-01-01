{-# LANGUAGE BangPatterns #-}
import Graphics.Gloss.Raster.Array
import qualified Graphics.Gloss as G
import Data.Array.Repa as R
import Numeric.Noise.Perlin
import GHC.Float
import System.IO.Unsafe

perlinNoise seed = perlin seed octaves scale persistance
              where octaves = 5
                    scale = 0.05
                    persistance = 0.6

mapRange :: (Num a, Fractional a) => a -> (a,a) -> (a,a) -> a
mapRange val (imin,imax) (omin,omax) =
    omin + (omax - omin) * ((val - imin) / (imax - imin))

main :: IO ()
main = do
  run 800 600 1 1

run :: Int -> Int -> Int -> Int -> IO ()
run windowX windowY scaleX scaleY
 = do
  let !sizeX  = windowX `div` scaleX
  let !sizeY  = windowY `div` scaleY
  let !noise = perlinNoise 1001

  let frame time
          = let bright c = rgb c c c
                makePixel (Z :. i1 :. i2) = bright $ realToFrac
                                            (mapRange (noiseValue noise (xoff, yoff, 0)) (-1.0, 1.0) (0.0, 1.0))
                                            where xoff = realToFrac time + (fromIntegral i1)*0.3
                                                  yoff = realToFrac time + (fromIntegral i2)*0.3
            in R.fromFunction (Z :. sizeX :. sizeY) makePixel

  animateArray
        (InWindow "Perlin 2D" (windowX, windowY) (10, 10))
        (scaleX, scaleY)
        frame
