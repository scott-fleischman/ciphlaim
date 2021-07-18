module Ciphlaim where

import Optics ((^.))
import qualified Optics
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Graphics.Vty (Vty)
import qualified Graphics.Vty as Vty

data WindowSize = WindowSize {width :: Int, height :: Int} deriving (Generic)

data State = State
  { selectedLine :: Int,
    windowSize :: WindowSize
  }
  deriving (Generic)

makePicture :: State -> Vty.Picture
makePicture State {selectedLine, windowSize} =
  let isSelectedToColor = \case
        True -> Vty.defAttr ` Vty.withBackColor ` Vty.brightYellow
        False -> Vty.defAttr
      line0Attr = isSelectedToColor $ selectedLine == 0
      line1Attr = isSelectedToColor $ selectedLine == 1
      lineStatus = Vty.string Vty.defAttr $ show (windowSize ^. #width) <> "x" <> show (windowSize ^. #height)
      line0 = Vty.string line0Attr "first line"
      line1 = Vty.string line1Attr "second line"
      img = Vty.vertCat [lineStatus, line0, line1]
      pic = Vty.picForImage img
  in pic

updateState :: Vty.Event -> State -> Maybe State
updateState =
  \case
    Vty.EvKey Vty.KUp _ -> Just . Optics.over #selectedLine (\x -> (x - 1) `mod` 2)
    Vty.EvKey Vty.KDown _ -> Just . Optics.over #selectedLine (\x -> (x + 1) `mod` 2)
    Vty.EvKey Vty.KEsc _ -> \_ -> Nothing
    Vty.EvResize width height -> Just . Optics.set #windowSize WindowSize {width, height}
    _ -> \_ -> Nothing

loop :: Vty -> State -> IO Vty.Event
loop vty inputState = do
  let picture = makePicture inputState
  Vty.update vty picture
  event <- Vty.nextEvent vty
  let maybeOutputState = updateState event inputState
  case maybeOutputState of
    Nothing -> pure event
    Just outputState -> loop vty outputState

getWindowSize :: Vty -> IO WindowSize
getWindowSize vty = do
  (width, height) <- Vty.displayBounds (Vty.outputIface vty)
  pure WindowSize {width, height}

main :: IO ()
main = do
  cfg <- Vty.standardIOConfig
  vty <- Vty.mkVty cfg
  Vty.setWindowTitle vty "Ciphlaim"
  windowSize <- getWindowSize vty
  let initialState = State {selectedLine = 0, windowSize}
  e <- loop vty initialState
  Vty.shutdown vty
  print ("Last event was: " ++ show e)
