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
initialSession = sessionFromText "Gallia est omnis divisa in partes tres, quarum \
                                 \unam incolunt Belgae, aliam Aquitani, tertiam qui \
                                 \ipsorum lingua Celtae, nostra Galli appellantur.\
                                 \\n\
                                 \Hi omnes lingua, institutis, legibus inter se \
                                 \differunt. Gallos ab Aquitanis Garumna flumen, a \
                                 \Belgis Matrona et Sequana dividit.\
                                 \\n\
                                 \Horum omnium fortissimi sunt Belgae, propterea\ 
                                 \quod a cultu atque humanitate provinciae \
                                 \longissime absunt, minimeque ad eos mercatores \
                                 \saepe commeant atque ea quae ad effeminandos \
                                 \animos pertinent important,\
                                 \\n\
                                 \proximique sunt Germanis, qui trans Rhenum \
                                 \incolunt, quibuscum continenter bellum gerunt. \
                                 \Qua de causa Helvetii quoque reliquos Gallos \
                                 \virtute praecedunt, quod fere cotidianis proeliis \
                                 \cum Germanis contendunt, cum aut suis finibus eos \
                                 \prohibent aut ipsi in eorum finibus bellum gerunt.\
                                 \\n\
                                 \Eorum una, pars, quam Gallos obtinere dictum est, \
                                 \initium capit a flumine Rhodano, continetur \
                                 \Garumna flumine, Oceano, finibus Belgarum, attingit \
                                 \etiam ab Sequanis et Helvetiis flumen Rhenum, \
                                 \vergit ad septentriones."

themes       :: AttrMap
themeMiss    :: AttrName
themeMatch   :: AttrName
themeNewline :: AttrName
themes = attrMap (V.black `on` V.white)
    [ (themeMiss,                  bg $ V.rgbColor 255 150 150)
    , (themeMatch,                 fg $ V.rgbColor  50  50  50)
    , (themeNewline,               V.white `on` V.black)
    , (themeNewline <> themeMiss,  fg $ V.rgbColor 255  50  50)
    , (themeNewline <> themeMatch, bg $ V.rgbColor 150 150 150)
    ]
themeMiss    = attrName "miss"
themeMatch   = attrName "match"
themeNewline = attrName "newline"

app :: App Session () ()
app = App { appDraw         = drawFunction
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = keyHandler
          , appStartEvent   = return
          , appAttrMap      = const themes }

keyHandler :: Session -> BrickEvent () () -> EventM () (Next Session)
keyHandler s (VtyEvent (V.EvKey V.KEsc []))      = halt s
keyHandler s (VtyEvent (V.EvKey (V.KChar c) [])) = liftIO (recordKeystroke s c)    >>= continue
keyHandler s (VtyEvent (V.EvKey V.KEnter    [])) = liftIO (recordKeystroke s '\n') >>= continue
keyHandler s _                                   = continue s

drawFunction :: Session -> [Widget ()]
drawFunction s =
    let applyTheme s m =
            if last s == '\n'
            then applyThemeSafe (init s) m <+> themedNewline m
            else applyThemeSafe s        m
        applyThemeSafe s (Just True)  = withAttr themeMatch                   $ str s
        applyThemeSafe s (Just False) = withAttr themeMiss                    $ str s
        applyThemeSafe s Nothing      = showCursor () (Location (0, 0))       $ str s
        themedNewline    (Just True)  = withAttr (themeNewline <> themeMatch) $ str "\\n"
        themedNewline    (Just False) = withAttr (themeNewline <> themeMiss)  $ str "\\n"
        themedNewline    Nothing      = showCursor () (Location (0, 0))
                                      $ withAttr themeNewline                 $ str "\\n"
    in [withBorderStyle unicode
        $ borderWithLabel (str "Haskell Typist")
        $ center
        $ vBox
        $ map hBox (renderKeystrokes s applyTheme)]

main :: IO Session
main = defaultMain app initialSession
