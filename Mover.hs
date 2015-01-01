import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

type MousePosition = Vector

data Mover = Mover { location :: Vector, velocity :: Vector, acceleration :: Vector }

data World = World { mover :: Mover, mousePosition:: MousePosition, debug :: String }


drawCircle :: Vector -> Picture
drawCircle (x,y) = Translate x y $ Pictures [color (makeColor 0.5 0.5 0.5 1.0) $ circleSolid 40,
                                             color black $ thickCircle 40 2]

draw :: World -> Picture
draw world = Pictures [
              drawCircle $ location (mover world),
              Translate (-360) (-200) $ scale 0.1 0.1 $ color black $ text (debug world),
              drawCircle $ mousePosition world
             ]

update :: Float -> World -> World
update time (World mover pos debug) = World { mover = newMover, mousePosition = pos, debug = debug }
                    where newMover = Mover { location = loc, velocity = vel, acceleration = acc }
                          vel' = velocity mover + acceleration mover
                          vel  = min vel' 10
                          loc = location mover + vel
                          acc = acceleration mover

event :: Event -> World -> World
event e world = case e of
                  EventMotion v -> World (mover world) v (show e)
                  otherwise -> world

main
 = play (InWindow "GameEvent" (800, 600) (10, 10))
        white
        100
        World {
     mover = Mover { location = (0, 0), velocity = (0,0), acceleration = (0.01,0.001)},
     mousePosition = (0,0), debug = "hello"
        }
        draw
        event
        update
