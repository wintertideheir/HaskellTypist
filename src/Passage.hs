module Passage where

import qualified Data.Time.Clock        (UTCTime, getCurrentTime)
import qualified Data.List              (find)

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
       let free_uids = Data.List.find (`notElem` (map uid passages)) [0..maxBound]
           error_msg = "No unique passage identifier possible for \"" ++ name' ++ "\"."
           uid' = case free_uids of
                      Nothing    -> error error_msg
                      Just uid'' -> uid''
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
       let session = Session date' keystrokes
           compare_uid = (== uid') . uid
           error_msg = "No passage with ID " ++ show uid' ++ " to save session to."
           passages' = case break compare_uid passages of
                           (a, (b:bs)) -> a ++ ((b{sessions ++ [session]}):bs)
                           (_, [])     -> error error_msg
       return passages'
