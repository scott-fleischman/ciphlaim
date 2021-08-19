module Ciphlaim where

import Brick (App(..))
import Brick qualified
import Brick.Widgets.Border qualified as Border
import Brick.Widgets.Border.Style qualified as Border.Style
import Data.Generics.Labels ()
import Graphics.Vty qualified as Vty

main :: IO ()
main = do
  let hebrewAndGreek =
        Brick.withBorderStyle Border.Style.unicodeRounded $ Border.border $ Brick.hLimit 50 $
        Brick.vBox
          [ Brick.str "top"
          , Brick.str "middle"
          , Brick.str "bottom"
          ]

      appDraw () = [hebrewAndGreek]
      appChooseCursor () [] = Nothing
      appChooseCursor () (x : _) = Just x
      appHandleEvent () (Brick.VtyEvent (Vty.EvKey _ _)) = Brick.halt ()
      appHandleEvent () _ = Brick.continue ()
      appStartEvent () = pure ()
      appAttrMap () = Brick.attrMap mempty []
      app :: App () () ()
      app = App { appDraw, appChooseCursor, appHandleEvent, appStartEvent, appAttrMap }
      initialState = ()
  () <- Brick.defaultMain app initialState
  putStrLn "All done"
