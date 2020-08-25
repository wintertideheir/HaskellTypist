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
themeMiss    :: AttrName
themeMatch   :: AttrName
themeNormal  :: AttrName
themeSpecial :: AttrName
themes = attrMap (V.black `on` V.white)
    [ (themeNormal,                V.black `on` V.white)
    , (themeNormal <> themeMiss,   bg $ V.rgbColor 255 150 150)
    , (themeNormal <> themeMatch,  fg $ V.rgbColor  50  50  50)
    , (themeSpecial,               bg $ V.rgbColor 200 200 200)
    , (themeSpecial <> themeMiss,  fg $ V.rgbColor 255  50  50)
    , (themeSpecial <> themeMatch, fg $ V.rgbColor 150 150 150)
    ]
themeMiss    = attrName "miss"
themeMatch   = attrName "match"
themeNormal  = attrName "normal"
themeSpecial = attrName "special"

applyTheme :: AttrName -> String -> Maybe Bool -> Widget ()
applyTheme t s Nothing = showCursor () (Location (0, 0))
                       $ withAttr t
                       $ str s
applyTheme t s (Just True)  = withAttr (t <> themeMatch) $ str s
applyTheme t s (Just False) = withAttr (t <> themeMiss)  $ str s

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
    liftIO (record k c    >>= return . AppState td) >>= continue
keyHandler (AppState td k) (VtyEvent (V.EvKey V.KEnter    [])) =
    liftIO (record k '\n' >>= return . AppState td) >>= continue
keyHandler as _ = continue as

drawFunction :: AppState -> [Widget ()]
drawFunction (AppState td k) =
    let checkedLines = map groupByScore
                     $ groupByLines
                     $ Passage.render (head td.passages) k
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
