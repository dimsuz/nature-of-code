import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss
import System.Random

type MousePosition = Vector

data Mover = Mover { location :: Vector,
                     velocity :: Vector,
                     acceleration :: Vector,
                     mass :: Float
                   }

data World = World { movers :: [Mover], mousePosition:: MousePosition, size :: Vector, debug :: String }

normalizeV' (0,0) = (0,0)
normalizeV' v = normalizeV v

limitV :: Vector -> Float -> Vector
limitV v len = if magV v > len then len `mulSV` normalizeV' v else v

mapRange :: (Num a, Fractional a) => a -> (a,a) -> (a,a) -> a
mapRange val (imin,imax) (omin,omax) =
    omin + (omax - omin) * ((val - imin) / (imax - imin))

drawCircle :: Float -> Float -> Vector -> Picture
drawCircle rad m (x,y) = Translate x y $ Pictures [color (makeColor r g b 1.0) $ circleSolid rad,
                                             color black $ thickCircle rad 2]
                       where r = mapRange m (0,20) (0,1)
                             g = mapRange m (0,20) (0,0.9)
                             b = mapRange m (0,20) (0,0.3)

draw :: World -> Picture
draw world = Pictures (map drawMover $ movers world)
    where drawMover mover = drawCircle (2 * (mass mover)) (mass mover) (location mover)

computeVelocity :: World -> Mover -> Mover
computeVelocity world mover =  mover { velocity = vel }
    where vel = limitV vel' 10
          vel' = velocity mover + acceleration mover

computeLocation :: World -> Mover -> Mover
computeLocation world mover = mover { location = loc }
    where loc = location mover + velocity mover

bounceOffWalls :: World -> Mover -> Mover
bounceOffWalls (World _ _ (szx, szy) _) mover@(Mover (x,y) _ _ _)
               | x > szx / 2 = mover { location = (szx / 2, y), velocity = (velocity mover) * (-1, 1) }
               | x < (- szx) / 2 = mover { location = ((-szx) / 2, y), velocity = (velocity mover) * (-1, 1)}
               | y > szy / 2 = mover { location = (x, szy / 2), velocity = (velocity mover) * (1, -1) }
               | y < (-szy) / 2 = mover { location = (x, (-szy) / 2), velocity = (velocity mover) * (1, -1)}
               | otherwise = mover

resetForces :: World -> Mover -> Mover
resetForces world mover = mover { acceleration = (0,0) }

type Force = Vector
applyForce :: Force -> World -> Mover -> Mover
applyForce force world mover = mover { acceleration = acc }
                               where acc = (acceleration mover) + mulSV (1/m) force
                                     m = mass mover

update :: Float -> World -> World
update time world@(World ms mousePos sz dbg) = World {
                                                   movers = newMovers,
                                                   mousePosition = mousePos,
                                                   size = sz,
                                                   debug = dbg }
    where newMovers = map newMover ms
          newMover m = transition m
              where transition = foldr (.) id curried
                    curried = map ($ world) transforms
                    transforms = reverse
                                 [
                                  applyForce (0.01, 0),
                                  applyForce (0.0, -1.0),
                                  computeVelocity,
                                  computeLocation,
                                  bounceOffWalls,
                                  resetForces
                                 ]

event :: Event -> World -> World
event _ world = world

mkMover m = Mover { location = (-400 + m*10, 0), velocity = (0,0), acceleration = (0.0,0.0), mass = m }

main = do
   stdGen <- getStdGen
   play (InWindow "GameEvent" (800, 600) (10, 10))
        white
        100
        World {
     movers = map mkMover $ take 50 (randomRs (5, 80) stdGen),
     mousePosition = (0,0), debug = "hello", size = (800, 600)
        }
        draw
        event
        update
