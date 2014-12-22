--module Main where

import Data.Ord 

import FRP.Helm
import qualified FRP.Helm.Graphics  as Graphics
import qualified FRP.Helm.Keyboard  as Keyboard
import qualified FRP.Helm.Window    as Window
import qualified FRP.Helm.Text      as Text
import qualified FRP.Helm.Color     as Color
import qualified FRP.Helm.Time      as Time


data GameConfig = GameConfig {
  windowDims :: (Int,Int),
  shipDims   :: (Int,Int),
  rocketDims :: (Int,Int),
  invaderDims :: (Int,Int)
}

data GameStatus = Startup | InProcess 
  deriving (Enum, Bounded,Eq)

data GameState = GameState {status :: GameStatus}

data InvaderState = InvaderState {invaderX :: Int, invaderY :: Int, invaderM :: InvaderMovement}

data InvaderMovement = R|D|L|D2
   deriving (Eq)

gameConfig :: GameConfig
gameConfig = GameConfig {
      windowDims = (800,600),
      shipDims   = (140,200),
      rocketDims = (20,20),
      invaderDims = (100,100)
}


engineConfig :: GameConfig -> EngineConfig
engineConfig gameConfig = 
  EngineConfig (windowDims gameConfig) False False "Monad Invaders v0.0.1"

backgroundImg :: GameConfig -> Element
backgroundImg gameConfig = Graphics.fittedImage 
                (fst . windowDims $ gameConfig)
                (snd . windowDims $ gameConfig) "paper_fullhd_20.png"


redInvaderImg ::GameConfig -> Element
redInvaderImg gameConfig = Graphics.fittedImage (fst . invaderDims $ gameConfig) (snd . invaderDims $ gameConfig) "red_invader.png"

gameSignal :: Signal GameState
gameSignal = foldp modifyState initialState (Keyboard.isDown Keyboard.SpaceKey)
  where
    initialState = GameState {status = Startup}
    modifyState :: Bool -> GameState -> GameState
    modifyState pressed state = 
      if pressed && (status state == Startup) 
      then state {status = nextStatus} 
      else  state 
      where
        nextStatus = 
          let s = status state in (succ s)

invaderSignal :: Signal GameState -> Signal [InvaderState]
invaderSignal gameSignal  = foldp modifyState  initialState controlSignal
   where 
     initialState = zipWith (\ n state -> state {invaderX = (fst . invaderDims $ gameConfig )*(n-1) +20 })  [1..7] 
                                               (replicate 7 $ InvaderState {invaderX = 0, invaderY = 150, invaderM = R})

     controlSignal :: Signal (GameState,Time,Bool)
     controlSignal = lift3 (,,) gameSignal
                     (Time.every $ 1000 * Time.millisecond)
                     (Keyboard.isDown Keyboard.SpaceKey)
     modifyState :: (GameState,Time,Bool) -> [InvaderState] -> [InvaderState]
     modifyState (gameState,time, pressed) states =
                       if (status gameState == InProcess) && not  pressed
                       then (zipWith (\n st -> f st n) [1..length states] states)
                       else states
		       where
                          f state n = let (x',y',m') = (case (invaderX state,invaderY state , invaderM state ) of
                                                               (x,y,m) | x < 40 + (fst . invaderDims $ gameConfig )*(n-1) && m == R -> (x + 20,y, R)
                                                                       | m == R -> (x,y +20 , D)
                                                                       | m == D -> (x - 20, y,L)
                                                                       | m == L -> (x-20,y, D2)
                                                                       | otherwise -> (x,y +20,R) ) in  InvaderState {invaderX = x', invaderY = y' , invaderM = m'}

renderDebugString :: String -> Form
renderDebugString = move (400, 100) . toForm . Text.plainText

startupMessage :: Form 
startupMessage = move (400, 100) . toForm . Text.text . formatText $ message
  where 
    formatText = (Text.color $ color) . Text.bold . Text.header . Text.toText
    message = "Press Space to play"
    color =  Color.rgba (50.0 / 255) (50.0 / 255) (50.0 / 255) (0.7)

invaderForm :: InvaderState -> Form
invaderForm state = move (fromIntegral $ invaderX state ,
                       fromIntegral $ invaderY state) $ toForm (redInvaderImg gameConfig)

render :: (Int, Int) -> GameState -> [InvaderState] -> Element
render (w, h) gameState invaderState =
  let gameStatus = status gameState in 
  case gameStatus of 
    Startup -> collage w h $ 
      [toForm (backgroundImg gameConfig),startupMessage] ++ (map invaderForm invaderState )
    InProcess -> collage w h $ 
      [toForm (backgroundImg gameConfig),renderDebugString "InProcess"] ++ (map invaderForm invaderState )

main :: IO ()
main = 
  let windowSignal = Window.dimensions
      invaderSignal' = invaderSignal gameSignal 
      --gameSignal' = gameSignal invaderSignal'
  in  run (engineConfig gameConfig) $ render <~ 
        windowSignal ~~ gameSignal ~~ invaderSignal' 





