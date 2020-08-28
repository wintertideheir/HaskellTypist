module Interface.InterfaceSession where

import Passage
import Interface

import Themes
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import qualified Graphics.Vty            as V
import qualified Data.Time.Clock         (getCurrentTime)
import qualified Data.Time.Clock.System  (SystemTime, getSystemTime)
import qualified Control.Monad.IO.Class  (liftIO)

data InterfaceSession = InterfaceSession { passages   :: [Passage]
                                         , begin      :: Maybe Data.Time.Clock.System.SystemTime
                                         , keystrokes :: [Keystroke]
                                         }

instance Interface InterfaceSession where
    input interface (VtyEvent (V.EvKey V.KEsc []))      = halt interface
    input interface (VtyEvent (V.EvKey (V.KChar c) [])) = Control.Monad.IO.Class.liftIO (record interface c)    >>= continue
    input interface (VtyEvent (V.EvKey V.KEnter    [])) = Control.Monad.IO.Class.liftIO (record interface '\n') >>= continue
    input interface _                                   = continue interface
    draw  interface =
        let checkedLines = map groupByScore
                         $ groupByLines
                         $ Passage.render (head interface.passages) interface.keystrokes
            normalLines = vBox
                        $ map hBox
                        $ map (map (\(s, m) -> themeRendered themeNormal (filter (/= '\n') s) m))
                        $ checkedLines
            specialLines = vBox
                         $ map (\(s, m) -> case last s of
                                           '\n' -> themeRendered themeSpecial "\\n" m
                                           _    -> themeRendered themeSpecial "<-"  m)
                         $ map last
                         $ checkedLines
        in [withBorderStyle unicode
            $ borderWithLabel (str "Haskell Typist")
            $ center
            $ (normalLines <+> specialLines)]

record :: InterfaceSession -> Char -> IO InterfaceSession
record td c =
    do t <- Data.Time.Clock.System.getSystemTime
       case td.begin of
           Just begin' -> return td{keystrokes ++ [Keystroke ((asCentiseconds t) - (asCentiseconds begin')) c]}
           Nothing     -> return td{keystrokes ++ [Keystroke 0 c],
                                    begin = Just t}
