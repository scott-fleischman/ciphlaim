module Ciphlaim where

import qualified Graphics.Vty as Vty

makePicture :: Int -> Vty.Picture
makePicture selected =
  let isSelectedToColor = \case
        True -> Vty.defAttr ` Vty.withBackColor ` Vty.brightYellow
        False -> Vty.defAttr
      line0Attr = isSelectedToColor $ selected == 0
      line1Attr = isSelectedToColor $ selected == 1
      line0 = Vty.string line0Attr "first line"
      line1 = Vty.string line1Attr "second line"
      img = Vty.vertJoin line0 line1
      pic = Vty.picForImage img
  in pic

updateState :: Vty.Event -> Int -> Maybe Int
updateState =
  \case
    Vty.EvKey Vty.KUp _ -> \x -> Just ((x - 1) `mod` 2)
    Vty.EvKey Vty.KDown _ -> \x -> Just ((x + 1) `mod` 2)
    Vty.EvKey Vty.KEsc _ -> \_ -> Nothing
    _ -> \_ -> Nothing

loop :: Vty.Vty -> Int -> IO Vty.Event
loop vty inputState = do
  let picture = makePicture inputState
  Vty.update vty picture
  event <- Vty.nextEvent vty
  let maybeOutputState = updateState event inputState
  case maybeOutputState of
    Nothing -> pure event
    Just outputState -> loop vty outputState

main :: IO ()
main = do
  cfg <- Vty.standardIOConfig
  vty <- Vty.mkVty cfg
  e <- loop vty 0
  Vty.shutdown vty
  print ("Last event was: " ++ show e)