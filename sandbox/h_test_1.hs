import FRP.Helm
import qualified FRP.Helm.Graphics as Graphics
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window   as Window

--------------------------------------------------------
-------------Types for game enteties states-------------
--------------------------------------------------------

data State = State { x :: Double, y :: Double }

data ShipState = ShipState {shipX :: Int, shipY :: Int} 

--data GameState = 

--------------------------------------------------------
--------------Initialisation of resources---------------
--------------------------------------------------------

background :: Element
background = Graphics.fittedImage 640 480 "img/paper.png"

engineConfig :: EngineConfig
engineConfig = EngineConfig (640,480) False False "Monad Invaders v0.0.1"

spaceShipImg :: Element
spaceShipImg = Graphics.fittedImage 100 100 "img/space_impact_ship.png"

redInvaderImg :: Element
redInvaderImg = Graphics.fittedImage 100 100 "img/red_invader.png"

--------------------------------------------------------
-----------------------Game logic-----------------------
--------------------------------------------------------
shipSignal :: Signal ShipState
shipSignal = signal
  where initialState = ShipState {shipX = 300, shipY = 400} --TODO: Избавиться от хардкода
        signal = foldp newState initialState Keyboard.arrows
        newState :: (Int,Int) -> ShipState -> ShipState
        newState (dx,dy) state = state {shipX = shipX', shipY = shipY'}
          where shipX' = shipX state + dx
                shipY' = shipY state

step :: (Int, Int) -> State -> State
step (dx, dy) state = state { x = (10 * (realToFrac dx)) + x state,
                              y = (10 * (realToFrac dy)) + y state }

render :: (Int, Int) -> State -> Element
render (w, h) (State { x = x, y = y }) =
  collage w h [toForm background, move (x, y) $ toForm $ spaceShipImg]

main :: IO ()
main = run engineConfig $ render <~ Window.dimensions ~~ stepper
  where
    initialState = State { x = 300, y = 400} --TODO: Избавиться от хардкода
    stepper = foldp step initialState Keyboard.arrows

