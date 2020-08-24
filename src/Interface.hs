module Interface where

import Passage

import qualified Data.Time.Clock         (getCurrentTime)
import qualified Data.List               (find)

data TypistData = TypistData { passages :: [Passage]
                             }

newPassage :: TypistData -> String -> String -> IO TypistData
newPassage td name' text' =
    do date' <- Data.Time.Clock.getCurrentTime
       let uid' = fallibleFind ("No unique passage identifier possible for \"" ++ name' ++ "\".")
                               (`notElem` (map uid td.passages))
                               [0..maxBound]
           passage = Passage { uid      = uid'
                             , name     = name'
                             , date     = date'
                             , text     = text'
                             , sessions = []
                             }
       return td{passages = passage:(td.passages)}

newSession :: TypistData -> Int -> [Keystroke] -> IO TypistData
newSession td uid' k =
    do date' <- Data.Time.Clock.getCurrentTime
       let session = Session date' k
           passages' = fallibleReplace
                           ("No passage with ID " ++ show uid' ++ " to save session to.")
                           ((== uid') . uid)
                           (\passage -> passage{sessions `prefix` session})
                           td.passages
       return td{passages = passages'}

prefix :: [a] -> a -> [a]
prefix = flip (:)

fallibleFind :: Foldable t => String -> (a -> Bool) -> t a -> a
fallibleFind m p f =
    case Data.List.find p f of
        Nothing -> error m
        Just x -> x

fallibleReplace :: String -> (a -> Bool) -> (a -> a) -> [a] -> [a]
fallibleReplace m p r l =
    case break p l of
        (a, (b:bs)) -> a ++ ((r b):bs)
        (_, [])     -> error m

fallibleIndex :: String -> [a] -> Int -> a
fallibleIndex m l i =
    if i > length l
    then error m
    else l !! i