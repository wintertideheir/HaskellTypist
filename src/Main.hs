module Main where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V

data AppState = AppState { testInput :: String
                         , testText :: String }
initialAppState :: AppState
initialAppState = AppState { testInput = ""
                           , testText = "Gallia est omnis divisa in partes tres, \
                                        \quarum unam incolunt Belgae, aliam Aquitani, \
                                        \tertiam qui ipsorum lingua Celtae, \
                                        \nostra Galli appellantur."
                           }

app :: App AppState () ()
app = App { appDraw = drawFunction
          , appChooseCursor = neverShowCursor
          , appHandleEvent = keyHandler
          , appStartEvent = return
          , appAttrMap = const $ attrMap V.defAttr []
          }

keyHandler :: AppState -> BrickEvent () () -> EventM () (Next AppState)
keyHandler s (VtyEvent (V.EvKey V.KEsc []))        = halt s
keyHandler s (VtyEvent (V.EvKey (V.KChar c) [])) = 
    if (length $ testInput s) == (length $ testText s)
    then continue s
    else
        if c == ((testText s) !! (length $ testInput s))
        then continue (s {testInput = (testInput s) ++ [c]})
        else continue (s {testInput = (testInput s) ++ ['_']})
keyHandler s _                                     = continue s

drawFunction :: AppState -> [Widget n]
drawFunction s = [withBorderStyle unicode $ borderWithLabel (str "Haskell Typist") $ center $ ((str $ testText s) <=> (str $ testInput s))]

main :: IO AppState
main = defaultMain app initialAppState
