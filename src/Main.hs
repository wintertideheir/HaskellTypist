{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Passage
import Interface

import Control.Monad.IO.Class (liftIO)

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V

-----------------------------------------------------------------------
--                             Example                               --
-----------------------------------------------------------------------

exampleText :: String
exampleText = "Gallia est omnis divisa in partes tres, quarum \
              \unam incolunt Belgae, aliam Aquitani, tertiam qui \
              \ipsorum lingua Celtae, nostra Galli appellantur.\n\
              \\
              \Hi omnes lingua, institutis, legibus inter se \
              \differunt. Gallos ab Aquitanis Garumna flumen, a \
              \Belgis Matrona et Sequana dividit.\n\
              \\
              \Horum omnium fortissimi sunt Belgae, propterea \
              \quod a cultu atque humanitate provinciae \
              \longissime absunt, minimeque ad eos mercatores \
              \saepe commeant atque ea quae ad effeminandos \
              \animos pertinent important,\n\
              \\
              \proximique sunt Germanis, qui trans Rhenum \
              \incolunt, quibuscum continenter bellum gerunt. Qua \
              \de causa Helvetii quoque reliquos Gallos virtute \
              \praecedunt, quod fere cotidianis proeliis cum \
              \Germanis contendunt, cum aut suis finibus eos \
              \prohibent aut ipsi in eorum finibus bellum gerunt.\n\
              \\
              \Eorum una, pars, quam Gallos obtinere dictum est, \
              \initium capit a flumine Rhodano, continetur \
              \Garumna flumine, Oceano, finibus Belgarum, attingit \
              \etiam ab Sequanis et Helvetiis flumen Rhenum, \
              \vergit ad septentriones."

-----------------------------------------------------------------------
--                              Themes                               --
-----------------------------------------------------------------------

themes       :: AttrMap
themeNormal  :: AttrName
themeSpecial :: AttrName
themeQ25     :: AttrName
themeQ50     :: AttrName
themeQ75     :: AttrName
themeQ100    :: AttrName
themes =
    let themeNormal'  :: AttrName -> Int -> Int -> Int -> (AttrName, V.Attr)
        themeSpecial' :: AttrName -> Int -> Int -> Int -> (AttrName, V.Attr)
        themeNormal'  t r g b = (themeNormal <> t,  fg $ V.rgbColor r g b)
        themeSpecial' t r g b = (themeSpecial <> t, bg $ V.rgbColor r g b)
    in attrMap (V.black `on` V.white)
        [ (themeNormal               , V.black `on` V.white)
        , themeNormal'  themeQ25  100 0   0
        , themeNormal'  themeQ50  100 50  0
        , themeNormal'  themeQ75  50  100 0
        , themeNormal'  themeQ100 0   100 0
        , (themeSpecial              , V.white `on` V.black)
        , themeSpecial' themeQ25  150 0  0
        , themeSpecial' themeQ50  100 50 0
        , themeSpecial' themeQ75  50 100 0
        , themeSpecial' themeQ100 0  150 0
        ]
themeNormal  = attrName "normal"
themeSpecial = attrName "special"
themeQ25     = attrName "1st Quartile"
themeQ50     = attrName "2nd Quartile"
themeQ75     = attrName "3rd Quartile"
themeQ100    = attrName "4th Quartile"

applyTheme :: AttrName -> String -> Maybe Float -> Widget ()
applyTheme t s Nothing = showCursor () (Location (0, 0))
                       $ withAttr t
                       $ str s
applyTheme t s (Just x)
    |             x <= 0.25 = withAttr (t <> themeQ25)  $ str s
    | 0.25 < x && x <= 0.5  = withAttr (t <> themeQ50)  $ str s
    | 0.5  < x && x <= 0.75 = withAttr (t <> themeQ75)  $ str s
    | otherwise             = withAttr (t <> themeQ100) $ str s

-----------------------------------------------------------------------
--                            App Data                               --
-----------------------------------------------------------------------

data AppState = AppState TypistData [Keystroke]

app :: App AppState () ()
app = App { appDraw         = drawFunction
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = keyHandler
          , appStartEvent   = return
          , appAttrMap      = const themes
          }

keyHandler :: AppState -> BrickEvent () () -> EventM () (Next AppState)
keyHandler as (VtyEvent (V.EvKey V.KEsc [])) = halt as
keyHandler (AppState td k) (VtyEvent (V.EvKey (V.KChar c) [])) =
    liftIO (toKeystroke c    >>= return . AppState td . (k ++) . pure) >>= continue
keyHandler (AppState td k) (VtyEvent (V.EvKey V.KEnter    [])) =
    liftIO (toKeystroke '\n' >>= return . AppState td . (k ++) . pure) >>= continue
keyHandler as _ = continue as

drawFunction :: AppState -> [Widget ()]
drawFunction (AppState td k) =
    let checkedLines = map groupByScore
                     $ groupByLines
                     $ renderFragment (text $ head td.passages)
                     $ map (\(Keystroke _ x) -> x) k
        normalLines = vBox
                    $ map hBox
                    $ map (map (\(s, m) -> applyTheme themeNormal (filter (/= '\n') s) m))
                    $ checkedLines
        specialLines = vBox
                     $ map (\(s, m) -> case last s of
                                       '\n' -> applyTheme themeSpecial "\\n" m
                                       _    -> applyTheme themeSpecial "<-"  m)
                     $ map last
                     $ checkedLines
    in [withBorderStyle unicode
        $ borderWithLabel (str "Haskell Typist")
        $ center
        $ (normalLines <+> specialLines)]

main :: IO AppState
main =
    do let td = TypistData []
       td'  <- newPassage td  "Example Passage" exampleText
       defaultMain app (AppState td' [])
