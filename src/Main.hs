module Main where

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

data GameState = GameState {status :: GameStatus, lastTime :: Time, tickTime :: Time}

-- Тип, описывающий состояние фона окна, меняется вслед за стуатусом игры
--    До старта игры выводится приграшение к старту
--    В процессе игры выводится количество очков и здоровье кораблика
--    При проигрыше выводится соответсвующее сообщение

-- Тип, описывающий состояние кораблика: 
--    shipX, shipY - Координаты кораблика
--    rocketX, rocketY - Координаты выпущенной корабликом ракеты
--    rocketFlying - Выпущена ли ракета
--      На данный момент поддерживается только одина ракета, 
--      надо придумать, как сделать много
data ShipState = ShipState {shipX :: Int, shipY :: Int, shipHP :: Int} 

data InvaderState = InvaderState {invaderX :: Double, invaderY :: Double, invaderM :: InvaderMovement, isAlive :: Bool} deriving (Eq)

data RocketState = RocketState { rocketX :: Double, rocketY :: Double, rocketFlying :: Bool}

data InvaderMovement = R|D|L|D2
   deriving (Eq)

gameConfig :: GameConfig
gameConfig = GameConfig {
      windowDims = (450,600),
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

spaceShipImg :: GameConfig -> Element
spaceShipImg gameConfig = Graphics.fittedImage 
                (fst . shipDims $ gameConfig) 
                (snd . shipDims $ gameConfig) "graphics/ship/ship.png"

invaderImg ::FilePath -> GameConfig -> Element
invaderImg file gameConfig = Graphics.fittedImage 
  (fst . invaderDims $ gameConfig) (snd . invaderDims $ gameConfig) file

--------------------------------------------------------
-----------------------Game logic-----------------------
--------------------------------------------------------

-- Глобавльный таймер
gameTimer :: Signal Time
gameTimer = (Time.every $ 100 * Time.millisecond)

-- Главный сигнал игры. Описывает изменения состояния игры (GameState).
{-- 
  На данный момент работает так:
    В начале поле структуры status структуры GameState 
    имеет значение Startup (элемент перечисления GameStatus) -- 
    на экране изображено приглашение к началу игры.
    После первого нажатия на пробел поле status ппринимает значение 
    InProcess, то есть игра переходит в активное состояние, на экране
    виден кораблик, можно двигать его стрелочками.

  TODO: Реализовать переход в состояние Over, для этого надо расширить 
        состояние кораблика и его сигнал, а в сигнале игры обработать 
        "смерть" кораблика
--}
gameSignal :: Signal GameState
gameSignal = foldp modifyState initialState (lift2 (,) (Keyboard.isDown Keyboard.SpaceKey) gameTimer)
  where
    initialState = GameState {status = Startup, lastTime = 0, tickTime = 0}

    modifyState :: (Bool, Time) -> GameState -> GameState
    modifyState (pressed, time) state = 
      if pressed && (status state == Startup) 
      then state {status = nextStatus, lastTime = time, tickTime = time - lastTime state} 
      else state {status = status state, lastTime = time, tickTime = time - lastTime state}
      where
        nextStatus = 
          let s = status state in
          if s == (maxBound :: GameStatus) then s else succ s

-- Сигнал инвейдеров 
-- TODO: вынести в конфигурацию кол-во инвейдеров 
invaderSignal :: Int -> Signal GameState -> Signal RocketState -> Signal [InvaderState]
invaderSignal color gameSignal rocketSignal = foldp modifyState  initialState controlSignal
  where 
    yPosition = case color of 
                     0 -> 0
                     1 -> 80
                     2 -> 160
    initialState = zipWith (\n state -> 
        state {invaderX = fromIntegral (fst . invaderDims $ gameConfig )*(n-1) +20 })  [1..4] 
          (replicate 4 $ InvaderState {invaderX = 0, invaderY = yPosition , invaderM = R, isAlive = True})

    controlSignal :: Signal (GameState,Bool, RocketState)
    controlSignal = lift3 (,,) gameSignal
                     --(Time.every $ 1000 * Time.millisecond)
                     (Keyboard.isDown Keyboard.SpaceKey)
                     rocketSignal

    modifyState :: (GameState,Bool,RocketState) -> [InvaderState] -> [InvaderState]
    modifyState (gameState,pressed,rocket) states =
      if (status gameState == InProcess) && not  pressed
      then (zipWith (\n st -> if (rocketFlying rocket) 
                              then f (murder st) n 
                              else f st n) 
                      [1..length states] states)
      else states
        where
          --eqy state = (invaderY state <= rocketY rocket) && rocketFlying rocket
          murder state = 
            let isDead =  (( rocketX rocket >= invaderX state) && ( rocketX rocket <= invaderX state + 75 )) && -- ((null . intersect [rocketX rocket +1..rocketX rocket + 9]) [invaderX state..invaderX state +40]) &&
                             (rocketY rocket <= invaderY state + 35) && rocketFlying rocket in
                                          if not isDead then state else state{isAlive = False}  
          f state n = let (x',y',m',a')=(case (invaderX state, invaderY state, invaderM state, isAlive state) of
                                        (x,y,m,a)| x < 40 + (fromIntegral $ fst . invaderDims $ gameConfig )*((fromIntegral n)-1) && m == R -> (x + (tickTime gameState) * 0.02,y, R,a)
                                                 | m == R -> (x, y + (tickTime gameState) * 0.02 , D,a)
                                                 | m == D -> (x - (tickTime gameState) * 0.02, y, L,a)
                                                 | m == L -> (x - (tickTime gameState) * 0.02, y, D2,a)
                                                 | otherwise -> (x, y + (tickTime gameState) * 0.02, R,a) )
                      in  InvaderState {invaderX = x',invaderY = y',invaderM = m',isAlive= a'}

-- Сигнал кораблика, описывает поведение кораблика во времени
shipSignal :: Signal GameState -> Signal ShipState
shipSignal gameSignal = foldp modifyState initialState controlSignal
  where 
    initialState = 
      let (w,h)   = windowDims gameConfig
          (sw,sh) = shipDims   gameConfig
      in ShipState {shipX = w `div` 2 - sw `div` 2, 
                    shipY = h - sh, shipHP = 100}
    
    controlSignal :: Signal ((Int,Int),GameState)
    controlSignal = lift2 (,) Keyboard.arrows gameSignal

    modifyState :: ((Int,Int),GameState) -> ShipState -> ShipState
    modifyState ((dx,dy),gameState) state = 
      if status gameState == InProcess
      then state {shipX = shipX', shipY = shipY'}
      else state
        where shipX' = shipX state + 30 * dx
              shipY' = shipY state
-- Сигнал ракеты, зависит от сигнала кораблика.
--   Можно стрелять, нажмая на пробел, движение управляется таймером. 
--   TODO: БАГ!! Скорость движения ракеты зависит от нажатия на стрелочки 
--         (управление карабликом) -- пока не придумал, как с этим бороться
--         Дело в том, что сигнал кораблика является частью управляющего сигнала ракеты
--         но что с этим делать -- не понятно.
--         
--         Ещё БАГ!!! Первый выстрел происходит по нажатию на пробел, 
--         который относится к старту игры, а не к выстрелам
--   В этой функции  есть хардкод: он отражает положение ракеты относительно кораблика и 
--   количество пикселей, на которые смещается ракета при полёте
rocketSignal :: Signal GameState -> Signal ShipState -> Signal RocketState
rocketSignal gameSignal shipSignal = foldp modifyState initialState controlSignal
  where 
    initialState = RocketState {rocketX = -30, rocketY = 550, rocketFlying = False}
    
    controlSignal :: Signal (Bool, Time, GameState, ShipState)
    controlSignal = lift4 (,,,) (Keyboard.isDown Keyboard.SpaceKey) 
                                ((/ 2) `fmap` gameTimer)
                                gameSignal
                                shipSignal

    modifyState :: (Bool, Time, GameState, ShipState) -> RocketState -> RocketState
    modifyState (launched,time,gameState, shipState) state =
      if status gameState == InProcess 
      then state {rocketX = rocketX', rocketY = rocketY', rocketFlying = rocketFlying'}
      else initialState
      where
        rocketX' = if   rocketFlying' 
                   then rocketX state 
                   else fromIntegral $ shipX shipState + 35 -- TODO: скорректировать смешение ракеты к центру кораблика 
        rocketY' = if   rocketFlying' 
                   then rocketY state - (tickTime gameState) * 0.1 -- TODO: Вернуть родную скорость или подобрать новую годную. Равномерненько
                   else fromIntegral $ shipY shipState + 75
        rocketFlying' = launched || 
                        (rocketY state > 0 && 
                          rocketY state < (fromIntegral . snd . windowDims $ gameConfig) - 30)

invaderForm :: Int -> InvaderState -> Form
invaderForm color state = case color of 
                              0 -> move (invaderX state , invaderY state) $ toForm (invaderImg "graphics/invaders/red_invader.png" gameConfig)
                              1 -> move (invaderX state , invaderY state) $ toForm (invaderImg "graphics/invaders/black_invader.png" gameConfig)
                              2 -> move (invaderX state , invaderY state) $ toForm (invaderImg "graphics/invaders/green_invader.png" gameConfig)

shipForm :: ShipState -> Form
shipForm state = move (fromIntegral $ shipX state,
                       fromIntegral $ shipY state) $ toForm (spaceShipImg gameConfig)

-- Здоровье кораблика
shipHPForm :: ShipState -> Form 
shipHPForm state = 
  let (w,h) = windowDims gameConfig 
  in move (70, 
           fromIntegral $ h - 30) $ toForm . Text.text . formatText $ message
  where 
    formatText = (Text.color $ color) . Text.bold . Text.toText
    message = "Health: " ++ (show (shipHP state))
    color =  Color.rgba (50.0 / 255) (50.0 / 255) (50.0 / 255) (0.7)

-- Ракета кораблика
rocketForm :: RocketState -> Form
rocketForm state =
  move (rocketX state, rocketY state) $ filled rocketColor $ 
                                          rect (fromIntegral . fst . rocketDims $ gameConfig)
                                               (fromIntegral . snd . rocketDims $ gameConfig)
  where
    rocketColor = Color.rgba (0.0 / 255) (0.0 / 255) (0.0 / 255) (0.7)

renderMessage :: Int -> Int -> String -> Form 
renderMessage x y = move (fromIntegral x, fromIntegral y) . toForm . Text.text . formatText
  where 
    formatText = (Text.color $ color) . Text.bold . (Text.height 20) . Text.toText
    color =  Color.rgba (50.0 / 255) (50.0 / 255) (50.0 / 255) (0.7)

controlsMessage :: Form
controlsMessage = renderMessage 200 400 $ "   Use ← → to move \n And space to shoot"

--нужно поправить размеры захватчика , поправить конфигурацию изначальную (размеры)
--ну и скорость кораблика не радует , должен двигаться при удерживании стрелок
--добавить счет(не знаю , что под этим предполагает логика игры. Число убитых захватчиков? )
--



render :: (Int, Int) -> GameState -> [InvaderState]->[InvaderState] -> [InvaderState] -> ShipState ->RocketState -> Element
render (w, h) gameState rInvState bInvState gInvState shipState rocketState=
  let gameStatus = status gameState in 
  case gameStatus of 
   Startup -> collage w h $ 
      [toForm (backgroundImg gameConfig),renderMessage 200 300 "Press space to play", controlsMessage, shipForm shipState] ++ (map (invaderForm 0) rInvState )++
       (map (invaderForm 1) bInvState ) ++ ( map (invaderForm 2) gInvState )                                           
   InProcess -> let (gInvState',bInvState',rInvState') = (filterInv gInvState,filterInv bInvState,filterInv rInvState)in
                case shipWin gInvState' bInvState' rInvState' of
                   True -> collage w h $ [toForm (backgroundImg gameConfig),renderMessage 200 300 "You win!"]
                   _-> case (any (\x -> invaderY x >= fromIntegral ((snd . windowDims $ gameConfig) - (snd . shipDims $ gameConfig))) $ rInvState ++ bInvState++gInvState) of
                         True -> collage w h $ [toForm (backgroundImg gameConfig),renderMessage 200 300 "  Game over! \n You are dead!"]
                         _-> collage w h $ [toForm (backgroundImg gameConfig),{-renderDebugString "InProcess",-} shipForm shipState,rocketForm rocketState] ++ 
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



