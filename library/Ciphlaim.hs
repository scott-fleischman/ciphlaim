module Ciphlaim where

import Brick (App(..))
import qualified Brick
import qualified Graphics.Vty as Vty

main :: IO ()
main = do
  let appDraw () = [Brick.str "Yo"]
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
