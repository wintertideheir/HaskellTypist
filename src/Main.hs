module Main where

import Session

import Control.Monad.IO.Class (liftIO)

import Brick
import Brick.Types
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V

initialSession :: Session
initialSession = sessionFromText "Gallia est omnis divisa in partes tres, \
                                 \quarum unam incolunt Belgae, aliam Aquitani, \
                                 \tertiam qui ipsorum lingua Celtae, \
                                 \nostra Galli appellantur."

themes     :: AttrMap
themeMiss  :: AttrName
themeMatch :: AttrName
themes = attrMap (V.black `on` V.white)
    [ (themeMiss,  bg V.red)
    , (themeMatch, fg $ V.rgbColor 50 50 50)
    ]
themeMiss  = attrName "miss"
themeMatch = attrName "match"

app :: App Session () ()
app = App { appDraw         = drawFunction
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = keyHandler
          , appStartEvent   = return
          , appAttrMap      = const themes }

keyHandler :: Session -> BrickEvent () () -> EventM () (Next Session)
keyHandler s (VtyEvent (V.EvKey V.KEsc []))      = halt s
keyHandler s (VtyEvent (V.EvKey (V.KChar c) [])) = liftIO (recordKeystroke s c) >>= continue
keyHandler s _                                   = continue s

drawFunction :: Session -> [Widget ()]
drawFunction s =
    let applyTheme c (Just True)  = withAttr themeMatch $ str [c]
        applyTheme c (Just False) = withAttr themeMiss  $ str [c]
        applyTheme c Nothing      = showCursor () (Location (0, 0)) (str [c])
    in [withBorderStyle unicode $ borderWithLabel (str "Haskell Typist") $ center
        $ foldr (<+>) emptyWidget (renderKeystrokes s applyTheme)]

main :: IO Session
main = defaultMain app initialSession
