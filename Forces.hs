import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss

type MousePosition = Vector

data Mover = Mover { location :: Vector,
                     velocity :: Vector,
                     acceleration :: Vector
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

computeAcceleration :: World -> Mover ->  Mover
computeAcceleration world mover =  Mover { location = location mover, velocity = velocity mover, acceleration = acc }
    where acc = (normalizeV' direction) `mult` 0.5
          mousePos = mousePosition world
          loc = location mover
          direction = mousePos - loc
          mult = flip mulSV

computeVelocity :: World -> Mover -> Mover
computeVelocity world mover =  Mover { location = location mover, velocity = vel, acceleration = acceleration mover }
    where vel = limitV vel' 10
          vel' = velocity mover + acceleration mover

computeLocation :: World -> Mover -> Mover
computeLocation world mover = Mover { location = loc, velocity = velocity mover, acceleration = acceleration mover }
    where loc = location mover + velocity mover

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
                                        [ computeAcceleration,
                                          computeVelocity,
                                          computeLocation ]

event :: Event -> World -> World
event e world = case e of
                  EventMotion v -> World (mover world) v (size world) (show e)
                  otherwise -> world

main
 = play (InWindow "GameEvent" (800, 600) (10, 10))
        white
        100
        World {
     mover = Mover { location = (0, 0), velocity = (0,0), acceleration = (0.0,0.0)},
     mousePosition = (0,0), debug = "hello", size = (800, 600)
        }
        draw
        event
        update
