module Ciphlaim where

import Brick (App(..))
import qualified Brick
import Control.Lens.Operators
import Data.Generics.Labels ()
import qualified Graphics.Vty as Vty
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Border.Style as Border.Style

main :: IO ()
main = do
  let hebrew = "בְּרֵאשִׁית בָּרָא אֱלֹהִים אֵת הַשָּׁמַיִם וְאֵת הָאָרֶץ׃"
      greek = "Τοῦ δὲ Ἰησοῦ χριστοῦ ἡ γένεσις οὕτως ἦν."
      -- Latin Letter Dental Click '\x01C0'
      -- Cyrillic Letter Palochka	'\x04CF'
      borderStyleWithEnglish = Border.Style.unicodeRounded & #bsVertical .~ '\x01C0'
      hebrewAndGreek =
        Brick.withBorderStyle borderStyleWithEnglish $ Border.border $ Brick.hLimit 50 $
        Brick.vBox
          [ Brick.str hebrew
          , Brick.str greek
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
