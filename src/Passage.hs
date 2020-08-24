module Passage where

import qualified Data.Time.Clock         (UTCTime)
import qualified Data.Time.Clock.System  (SystemTime, getSystemTime)
import qualified Data.List.Extra         (groupOn)

----------------------------------------------------------------------
--                              Types                               --
----------------------------------------------------------------------

data Keystroke = Keystroke Data.Time.Clock.System.SystemTime Char

data Session = Session Data.Time.Clock.UTCTime [Keystroke]

data Passage = Passage { uid      :: Int
                       , name     :: String
                       , date     :: Data.Time.Clock.UTCTime
                       , text     :: String
                       , sessions :: [Session]
                       }

toKeystroke :: Char -> IO Keystroke
toKeystroke c =
    do t <- Data.Time.Clock.System.getSystemTime
       return (Keystroke t c)

renderFragment :: String -> String -> [(Char, Maybe Float)]
renderFragment []     _      = []
renderFragment t      []     = [(t', Nothing) | t' <- t]
renderFragment (t:ts) (s:ss) =
    if t == s
    then (t, Just 1.0) : renderFragment ts ss
    else (t, Just 0.0) : renderFragment ts ss

groupByLines :: [(Char, Maybe Float)] -> [[(Char, Maybe Float)]]
groupByLines x =
    let lineShouldEnd l  ' '  = length l > 50
        lineShouldEnd _  '\n' = True
        lineShouldEnd _  _    = False
        stackReadable [] c     = [[c]]
        stackReadable (l:ls) c =
            if lineShouldEnd l (fst c)
            then []:(c:l):ls
            else    (c:l):ls
    in map reverse
       $ reverse
       $ foldl stackReadable [] x

groupByScore :: [(Char, Maybe Float)] -> [(String, Maybe Float)]
groupByScore x =
    let collapseSameScore [] = ([],        Nothing)
        collapseSameScore l  = (map fst l, snd $ head l)
    in map collapseSameScore
       $ Data.List.Extra.groupOn snd x
