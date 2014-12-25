--module Main where

import Data.Ord 
import Data.List

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

data GameStatus = Startup | InProcess |Over
  deriving (Enum, Bounded,Eq)

data GameState = GameState {status :: GameStatus}

data ShipState = ShipState {shipX :: Int, shipY :: Int} 

data InvaderState = InvaderState {invaderX :: Int, invaderY :: Int, invaderM :: InvaderMovement, isAlive :: Bool} deriving (Eq)

data RocketState = RocketState { rocketX :: Int, rocketY :: Int, rocketFlying :: Bool}

data InvaderMovement = R|D|L|D2
   deriving (Eq)

gameConfig :: GameConfig
gameConfig = GameConfig {
      windowDims = (450,800),
      shipDims   = (70,100),
      rocketDims = (10,10),
      invaderDims = (100,100)
}


engineConfig :: GameConfig -> EngineConfig
engineConfig gameConfig = 
  EngineConfig (windowDims gameConfig) False False "Monad Invaders v0.0.1"

backgroundImg :: GameConfig -> Element
backgroundImg gameConfig = Graphics.fittedImage 
                (fst . windowDims $ gameConfig)
                (snd . windowDims $ gameConfig) "graphics/background/paper_smashed_vertical.png"


invaderImg ::FilePath -> GameConfig -> Element
invaderImg file gameConfig = Graphics.fittedImage 
  (fst . invaderDims $ gameConfig) (snd . invaderDims $ gameConfig) file


spaceShipImg :: GameConfig -> Element
spaceShipImg gameConfig = Graphics.fittedImage 
                (fst . shipDims $ gameConfig) 
                (snd . shipDims $ gameConfig) "graphics/ship/ship.png"

gameSignal :: Signal GameState
gameSignal = foldp modifyState initialState (Keyboard.isDown Keyboard.SpaceKey)
  where
    initialState = GameState {status = Startup}

    --controlSignal :: Signal (Bool,(Int,Int))
    --controlSignal = lift2 (,) (Keyboard.isDown Keyboard.SpaceKey)

    modifyState :: Bool -> GameState -> GameState
    modifyState pressed state = 
      if pressed && (status state == Startup) 
      then state {status = nextStatus} 
      else state {status = status state}
      where
        nextStatus = 
          let s = status state in
          if s == (maxBound :: GameStatus) then s else succ s

invaderSignal :: Int -> Signal GameState -> Signal RocketState -> Signal [InvaderState]
invaderSignal color gameSignal rocketSignal = foldp modifyState  initialState controlSignal
  where 
    yPosition = case color of 
                     0 -> 0
                     1 -> 80
                     2 -> 160
    initialState = zipWith (\n state -> 
        state {invaderX = (fst . invaderDims $ gameConfig )*(n-1) +20 })  [1..7] 
          (replicate 7 $ InvaderState {invaderX = 0, invaderY = yPosition , invaderM = R, isAlive = True})

    controlSignal :: Signal (GameState,Time,Bool, RocketState)
    controlSignal = lift4 (,,,) gameSignal
                     (Time.every $ 1000 * Time.millisecond)
                     (Keyboard.isDown Keyboard.SpaceKey)
                     rocketSignal

    modifyState :: (GameState,Time,Bool,RocketState) -> [InvaderState] -> [InvaderState]
    modifyState (gameState,time, pressed,rocket) states =
      if (status gameState == InProcess) && not  pressed
      then (zipWith (\n st -> if (rocketFlying rocket) 
                              then f (murder st) n 
                              else f st n) 
                      [1..length states] states)
      else states
		    where
          murder state = let isalive = (null . intersect [rocketX rocket +1..rocketX rocket + 19]) [invaderX state..invaderX state +40] in
                                          if isalive then state else state{isAlive = False}  
          f state n = let (x',y',m',a')=(case (invaderX state,invaderY state , invaderM state, isAlive state) of
                                        (x,y,m,a)| x < 40 + (fst . invaderDims $ gameConfig )*(n-1) && m == R -> (x + 20,y, R,a)
                                                 | m == R -> (x,y +2 , D,a)
                                                 | m == D -> (x - 20, y,L,a)
                                                 | m == L -> (x-20,y, D2,a)
                                                 | otherwise -> (x,y +2,R,a) ) in  InvaderState {invaderX = x',invaderY = y',invaderM = m',isAlive= a'}

shipSignal :: Signal GameState -> Signal ShipState
shipSignal gameSignal = foldp modifyState initialState controlSignal
  where 
    initialState = 
      let (w,h)   = windowDims gameConfig
          (sw,sh) = shipDims   gameConfig
      in ShipState {shipX = w `div` 2 - sw `div` 2 - 10, 
                    shipY = h - sh}
    
    controlSignal :: Signal ((Int,Int),GameState)
    controlSignal = lift2 (,) Keyboard.arrows gameSignal

    modifyState :: ((Int,Int),GameState) -> ShipState -> ShipState
    modifyState ((dx,dy),gameState) state = 
      if status gameState == InProcess
      then state {shipX = shipX', shipY = shipY'}
      else state
        where shipX' = shipX state + 40 * dx --TODO: Мб стоит вынести константу в конфигурацию
              shipY' = shipY state

rocketSignal :: Signal GameState -> Signal ShipState -> Signal RocketState
rocketSignal gameSignal shipSignal = foldp modifyState initialState controlSignal
  where 
    initialState = RocketState {rocketX = -30, rocketY = 550, rocketFlying = False}
    
    controlSignal :: Signal (Bool, Double, GameState, ShipState)
    controlSignal = lift4 (,,,) (Keyboard.isDown Keyboard.SpaceKey) 
                                (Time.every $ 30 * Time.millisecond) -- тут скорость ракеты
                                gameSignal
                                shipSignal

    modifyState :: (Bool, Double, GameState, ShipState) -> RocketState -> RocketState
    modifyState (launched,time,gameState, shipState) state =
      if status gameState == InProcess 
      then state {rocketX = rocketX', rocketY = rocketY', rocketFlying = rocketFlying'}
      else initialState
      where
        rocketX' = if   rocketFlying' 
                   then rocketX state 
                   else shipX shipState + 70 -- TODO: скорректировать смешение ракеты к центру кораблика 
        rocketY' = if   rocketFlying' 
                   then rocketY state - 20 -- Равномерненько
                   else shipY shipState + 150
        rocketFlying' = launched || 
                        (rocketY state > 0 && 
                          rocketY state < (snd . windowDims $ gameConfig) - 60)


renderDebugString :: String -> Form
renderDebugString = move (400, 100) . toForm . Text.plainText

startupMessage :: Form 
startupMessage = move (400, 300) . toForm . Text.text . formatText $ message
  where 
    formatText = (Text.color $ color) . Text.bold . Text.header . Text.toText
    message = "Press Space to play"
    color =  Color.rgba (50.0 / 255) (50.0 / 255) (50.0 / 255) (0.7)

invaderForm :: Int -> InvaderState -> Form
invaderForm color state = case color of 
                              0 -> move (fromIntegral $ invaderX state , fromIntegral $ invaderY state) $ toForm (invaderImg "graphics/invaders/red_invader.png" gameConfig)
                              1 -> move (fromIntegral $ invaderX state , fromIntegral $ invaderY state) $ toForm (invaderImg "graphics/invaders/black_invader.png" gameConfig)
                              2 -> move (fromIntegral $ invaderX state , fromIntegral $ invaderY state) $ toForm (invaderImg "graphics/invaders/green_invader.png" gameConfig)

shipForm :: ShipState -> Form
shipForm state = move (fromIntegral $ shipX state,
                       fromIntegral $ shipY state) $ toForm (spaceShipImg gameConfig)

rocketForm :: RocketState -> Form
rocketForm state =
  move (fromIntegral $ rocketX state,
        fromIntegral $ rocketY state) $ filled rocketColor $ rect 20 20
  where
    rocketColor = Color.rgba (0.0 / 255) (0.0 / 255) (255.0 / 255) (0.7)

--нужно поправить размеры захватчика , поправить конфигурацию изначальную (размеры)
--ну и скорость кораблика не радует , должен двигаться при удерживании стрелок
--добавить счет(не знаю , что под этим предполагает логика игры. Число убитых захватчиков? )
--



render :: (Int, Int) -> GameState -> [InvaderState]->[InvaderState] -> [InvaderState] -> ShipState ->RocketState -> Element
render (w, h) gameState rInvState bInvState gInvState shipState rocketState=
  let gameStatus = status gameState in 
  case gameStatus of 
   Startup -> collage w h $ 
      [toForm (backgroundImg gameConfig),startupMessage,shipForm shipState] ++ (map (invaderForm 0) rInvState )++
			 (map (invaderForm 1) bInvState ) ++ ( map (invaderForm 2) gInvState )                                           
   InProcess -> let (gInvState',bInvState',rInvState') = (filterInv gInvState,filterInv bInvState,filterInv rInvState)in
                case shipWin gInvState' bInvState' rInvState' of
                   True -> collage w h $ [toForm (backgroundImg gameConfig),renderDebugString "Game over , you win!"]
                   _-> case (any (\x -> invaderY x >= 350) $ rInvState ++ bInvState++gInvState) of
                         True -> collage w h $ [toForm (backgroundImg gameConfig),renderDebugString "Game over , you are dead! "]
                         _-> collage w h $ [toForm (backgroundImg gameConfig),renderDebugString "InProcess", shipForm shipState,rocketForm rocketState] ++ 
                                (map (invaderForm 0) rInvState') ++ (map (invaderForm 1) bInvState' )++ (map (invaderForm 2) gInvState' )
                
                         
filterInv invader = filter isAlive  invader

shipWin a b c = null (a++b++c)

main :: IO ()
main = 
  let windowSignal = Window.dimensions
      shipSignal'   = shipSignal gameSignal
      rocketSignal' = rocketSignal gameSignal shipSignal' 
      redInvaderSignal' = invaderSignal 0 gameSignal rocketSignal'
      blackInvaderSignal' = invaderSignal 1 gameSignal rocketSignal'
      greenInvaderSignal' = invaderSignal 2 gameSignal rocketSignal'
  in  run (engineConfig gameConfig) $ render <~ 
        windowSignal ~~ gameSignal ~~ redInvaderSignal' ~~ blackInvaderSignal' ~~greenInvaderSignal' ~~ shipSignal'~~ rocketSignal'



