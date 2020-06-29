module Session where

import qualified System.CPUTime (getCPUTime)
import qualified Data.List.Extra (groupOn)
import qualified Data.List.Split (splitOneOf)

data Session = Session { keystrokes :: [(Integer, Char)]
                       , text       :: String
                       }

maximumTextWidth :: Int
maximumTextWidth = 70

whitespaceASCII :: [Char]
whitespaceASCII = [' ','\t','\n','\r','\v','\f']

sessionFromText :: String -> Session
sessionFromText t = Session { keystrokes = []
                            , text       = t  }

sessionsFromText :: String -> [Session]
sessionsFromText s =
    let combineChunks [] y     = [y]
        combineChunks (x:xs) y = if maximumTextWidth < (length (x ++ " " ++ y))
                                 then y:x:xs
                                 else (x ++ " " ++ y):xs
    in map sessionFromText
       $ foldl combineChunks []
       $ filter (not . null)
       $ Data.List.Split.splitOneOf whitespaceASCII s

completeSession :: Session -> Bool
completeSession s = (length $ keystrokes s) >= (length $ text s)

recordKeystroke :: Session -> Char -> IO Session
recordKeystroke s c =
    if completeSession s
    then return s
    else do
        t <- System.CPUTime.getCPUTime
        return s { keystrokes = (keystrokes s) ++ [(t, c)] }

renderKeystrokes :: Session -> (String -> Maybe Bool -> a) -> [a]
renderKeystrokes s t =
    let twin (x,y) = x == y
        matched    = Data.List.Extra.groupOn twin (zip (map snd $ keystrokes s) (text s))
        complete   = map (\g -> t (map fst g) (Just (twin $ head g))) matched
        incomplete = t (drop (length $ keystrokes s) $ text s) Nothing
    in complete ++ [incomplete]
