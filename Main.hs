import FRP.Helm
import qualified FRP.Helm.Graphics as Graphics
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window   as Window

--------------------------------------------------------
-------------Types for game enteties states-------------
--------------------------------------------------------

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

background :: Element
background = Graphics.fittedImage 800 600 "img/paper.png"

spaceShipImg :: Element
spaceShipImg = Graphics.fittedImage 175 250 "img/ship_25.png"

redInvaderImg :: Element
redInvaderImg = Graphics.fittedImage 100 100 "img/red_invader.png"

--------------------------------------------------------
-----------------------Game logic-----------------------
--------------------------------------------------------
-- Сигнал кораблика, описывает поведение кораблика во времени
shipSignal :: Signal ShipState
shipSignal = foldp modifyState initialState Keyboard.arrows
  where 
    initialState = ShipState {shipX = 300, shipY = 350} --TODO: Избавиться от хардкода
    modifyState :: (Int,Int) -> ShipState -> ShipState
    modifyState (dx,dy) state = state {shipX = shipX', shipY = shipY'}
      where shipX' = shipX state + 25 * dx --TODO: Мб стоит вынести константу в конфигурацию
            shipY' = shipY state

--------------------------------------------------------
-----------------------Rendering------------------------
--------------------------------------------------------

-- Рендеринг форм на основе элементов (изображений в формате png) и состояний объектов 

-- Задник 
-- TODO: Можно добавить что-нибудь на задник (Хп кораблика, очки и т.п.)
backgroundForm :: Form
backgroundForm = toForm background

-- Кораблик
shipForm :: ShipState -> Form
shipForm state = move (fromIntegral $ shipX state,
                       fromIntegral $ shipY state) $ toForm spaceShipImg

-- Рендеринг общей сцены 
render :: (Int, Int) -> ShipState -> Element
render (w, h) shipState =
  collage w h $ [backgroundForm,shipForm shipState]

main :: IO ()
main = run engineConfig $ render <~ Window.dimensions ~~ shipSignal
  --where
  --  initialState = State { x = 300, y = 400} --TODO: Избавиться от хардкода
  --  stepper = foldp step initialState Keyboard.arrows

