module Passage where

import qualified Data.Time.Clock        (UTCTime, getCurrentTime)
import qualified Data.Time.Clock.System (SystemTime, getSystemTime)
import qualified Data.List               (find)

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

newPassage :: [Passage] -> String -> String -> IO [Passage]
newPassage passages name' text' =
    do date' <- Data.Time.Clock.getCurrentTime
       let fallibleFind m p f =
               case Data.List.find p f of
                   Nothing -> error m
                   Just x -> x
           uid' = fallibleFind ("No unique passage identifier possible for \"" ++ name' ++ "\".")
                               (`notElem` (map uid passages))
                               [0..maxBound]
           passage = Passage { uid      = uid'
                             , name     = name'
                             , date     = date'
                             , text     = text'
                             , sessions = []
                             }
       return (passage:passages)

newSession :: [Passage] -> Int -> [Keystroke] -> IO [Passage]
newSession passages uid' keystrokes =
    do date' <- Data.Time.Clock.getCurrentTime
       let fallibleReplace m p r l =
               case break p l of
                   (a, (b:bs)) -> a ++ ((r b):bs)
                   (_, [])     -> error m
           session = Session date' keystrokes
           passages' = fallibleReplace
                           ("No passage with ID " ++ show uid' ++ " to save session to.")
                           ((== uid') . uid)
                           (\passage -> passage{sessions ++ [session]})
                           passages
       return passages'
