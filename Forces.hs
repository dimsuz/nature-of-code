import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss

type MousePosition = Vector

data Mover = Mover { location :: Vector,
                     velocity :: Vector,
                     acceleration :: Vector,
                     mass :: Float
                   }

data World = World { mover :: Mover, mousePosition:: MousePosition, size :: Vector, debug :: String }

normalizeV' (0,0) = (0,0)
normalizeV' v = normalizeV v

limitV :: Vector -> Float -> Vector
limitV v len = if magV v > len then len `mulSV` normalizeV' v else v

drawCircle :: Float -> Vector -> Picture
drawCircle r (x,y) = Translate x y $ Pictures [color (makeColor 0.5 0.5 0.5 1.0) $ circleSolid r,
                                             color black $ thickCircle r 2]

draw :: World -> Picture
draw world = Pictures [
              drawCircle 20 $ location (mover world)
             ]

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
update time world@(World m mousePos sz dbg) = World {
                                                   mover = newMover,
                                                   mousePosition = mousePos,
                                                   size = sz,
                                                   debug = dbg }
    where newMover = transition m
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
event e world = case e of
                  EventMotion v -> World (mover world) v (size world) (show e)
                  otherwise -> world

main
 = play (InWindow "GameEvent" (800, 600) (10, 10))
        white
        100
        World {
     mover = Mover { location = (0, 0), velocity = (0,0), acceleration = (0.0,0.0), mass = 10.0 },
     mousePosition = (0,0), debug = "hello", size = (800, 600)
        }
        draw
        event
        update
