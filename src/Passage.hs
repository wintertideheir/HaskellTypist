module Passage where

import qualified Data.Time.Clock        (UTCTime)

data Keystroke = Keystroke { centiseconds :: Int
                           , character    :: Char
                           }

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
    in render' passage.text (map character keystrokes)
