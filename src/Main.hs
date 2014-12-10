module Main where

import Data.Ord 

import FRP.Helm
import qualified FRP.Helm.Graphics as Graphics
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window   as Window
import qualified FRP.Helm.Text     as Text

-- Disclaimer: в рабочей версии комментариев возможны 
-- орфографические, синтаксические, пунктуационные, семантические
-- и другие ошибки. 

--------------------------------------------------------
-------------Types for game enteties states-------------
--------------------------------------------------------

-- Статус игры: ещё не начата, в процессе, окончена
data GameStatus = Startup | InProcess | Over
  deriving (Enum, Bounded,Eq)

data GameState = GameState {status :: GameStatus}

-- Тип, описывающий состояние фона окна, меняется вслед за стуатусом игры
--    До старта игры выводится приграшение к старту
--    В процессе игры выводится количество очков и здоровье кораблика
--    При проигрыше выводится соответсвующее сообщение
--data BackgroundState = BackgroundState {backgroundStatus :: GameStatus}

-- Тип, описывающий состояние кораблика: 
--    (shipX, shipY - Координаты кораблика)
--    ()   
data ShipState = ShipState {shipX :: Int, shipY :: Int} 

--------------------------------------------------------
--------------Initialisation of resources---------------
--------------------------------------------------------

-- Конфигурационная информация для библиотеки: размеры окна, заголовок окна, что-то там ещё
engineConfig :: EngineConfig
engineConfig = EngineConfig (800,600) False False "Monad Invaders v0.0.1"

backgroundImg :: Element
backgroundImg = Graphics.fittedImage 800 600 "Graphics/paper_fullhd_20.png"

spaceShipImg :: Element
spaceShipImg = Graphics.fittedImage 140 200 "Graphics/ship_pencil.png"

--redInvaderImg :: Element
--redInvaderImg = Graphics.fittedImage 100 100 "img/red_invader.png"

--------------------------------------------------------
-----------------------Game logic-----------------------
--------------------------------------------------------

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
gameSignal = foldp modifyState initialState (Keyboard.isDown Keyboard.SpaceKey)
  where
    initialState = GameState {status = Startup}
    modifyState :: Bool -> GameState -> GameState
    modifyState pressed state = 
      if pressed && (status state == Startup) then state {status = nextStatus} else state
      where
        nextStatus = 
          let s = status state in
          if s == (maxBound :: GameStatus) then s else succ s

-- Сигнал кораблика, описывает поведение кораблика во времени
shipSignal :: Signal ShipState
shipSignal = foldp modifyState initialState Keyboard.arrows
  where 
    initialState = ShipState {shipX = 300, shipY = 400} --TODO: Избавиться от хардкода
    modifyState :: (Int,Int) -> ShipState -> ShipState
    modifyState (dx,dy) state = state {shipX = shipX', shipY = shipY'}
      where shipX' = shipX state + 20 * dx --TODO: Мб стоит вынести константу в конфигурацию
            shipY' = shipY state

--------------------------------------------------------
-----------------------Rendering------------------------
--------------------------------------------------------

-- TODO: ИСКОРЕНИТЬ ХАРДКОД!!1!11

renderDebugString :: String -> Form
renderDebugString = move (400, 100) . toForm . Text.plainText

startupMessage :: Form 
startupMessage = move (400, 100) . toForm . Text.text . formatText $ message
  where 
    formatText = (Text.color $ color) . Text.bold . Text.header . Text.toText
    message = "Press Space to play"
    color =  rgba (50.0 / 255) (50.0 / 255) (50.0 / 255) (0.7)

-- Рендеринг форм на основе элементов (изображений в формате png) и состояний объектов 

-- Фон
-- TODO: Можно добавить что-нибудь на фон (Хп кораблика, очки и т.п.)
backgroundForm :: GameStatus -> Form
backgroundForm Startup   = group [toForm backgroundImg,startupMessage] 
backgroundForm InProcess = group [toForm backgroundImg,renderDebugString "InProcess"]
backgroundForm Over      = group [toForm backgroundImg,renderDebugString "Over"]


-- Кораблик
shipForm :: ShipState -> Form
shipForm state = move (fromIntegral $ shipX state,
                       fromIntegral $ shipY state) $ toForm spaceShipImg

-- Рендеринг общей сцены 
render :: (Int, Int) -> GameState -> ShipState -> Element
render (w, h) gameState shipState =
  let gameStatus = status gameState in 
  case gameStatus of 
    Startup   -> collage w h $ [backgroundForm gameStatus]
    InProcess -> collage w h $ [backgroundForm gameStatus,shipForm shipState]
    Over      -> collage w h $ [backgroundForm gameStatus]

main :: IO ()
main = run engineConfig $ render <~ Window.dimensions ~~ gameSignal ~~ shipSignal 
  --where
  --  initialState = State { x = 300, y = 400} --TODO: Избавиться от хардкода
  --  stepper = foldp step initialState Keyboard.arrows

