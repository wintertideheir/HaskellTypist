module Passage where

import qualified Data.Time.Clock         (UTCTime)
import qualified Data.Time.Clock.System  (SystemTime, getSystemTime)

data Keystroke = Keystroke Data.Time.Clock.System.SystemTime Char

data Session = Session Data.Time.Clock.UTCTime [Keystroke]

data Passage = Passage { uid      :: Int
                       , name     :: String
                       , date     :: Data.Time.Clock.UTCTime
                       , text     :: String
                       , sessions :: [Session]
                       }

render :: Passage -> [Keystroke] -> [(Char, Maybe Bool)]
render passage keystrokes =
    let render' []     _      = []
        render' t      []     = [(t', Nothing) | t' <- t]
        render' (t:ts) (s:ss) =
            if t == s
            then (t, Just True)  : render' ts ss
            else (t, Just False) : render' ts ss
        keystrokeToChar (Keystroke _ c) = c
    in render' passage.text (map keystrokeToChar keystrokes)
