import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss

type MousePosition = Vector

data Mover = Mover { location :: Vector, velocity :: Vector, acceleration :: Vector }

data World = World { mover :: Mover, mousePosition:: MousePosition, debug :: String }

normalizeV' (0,0) = (0,0)
normalizeV' v = normalizeV v

limitV :: Vector -> Float -> Vector
limitV v len = if magV v > len then len `mulSV` normalizeV' v else v

drawCircle :: Vector -> Picture
drawCircle (x,y) = Translate x y $ Pictures [color (makeColor 0.5 0.5 0.5 1.0) $ circleSolid 40,
                                             color black $ thickCircle 40 2]

draw :: World -> Picture
draw world = Pictures [
              drawCircle $ location (mover world),
              line [location (mover world), mousePosition world],
              line [(-300, 0), (300, 0)], line [(0, -300), (0, 300)],
              Translate (-360) (-200) $ scale 0.1 0.1 $ color black $ text (debug world)
             ]

update :: Float -> World -> World
update time (World mover mousePos debug) = World { mover = newMover, mousePosition = mousePos, debug = dbg }
                    where newMover = Mover { location = loc, velocity = vel, acceleration = acc }
                          acc = computeAcceleration (location mover) mousePos
                          vel' = velocity mover + acc
                          vel  = limitV vel' 10
                          loc = location mover + vel
                          dbg = show vel'

computeAcceleration :: Vector -> MousePosition -> Vector
computeAcceleration location mousePosition = (normalizeV' direction) `mult` 0.5
                                             where direction = mousePosition - location
                                                   mult = flip mulSV

event :: Event -> World -> World
event e world = case e of
                  EventMotion v -> World (mover world) v (show e)
                  otherwise -> world

main
 = play (InWindow "GameEvent" (800, 600) (10, 10))
        white
        100
        World {
     mover = Mover { location = (0, 0), velocity = (0,0), acceleration = (0.0,0.0)},
     mousePosition = (0,0), debug = "hello"
        }
        draw
        event
        update
