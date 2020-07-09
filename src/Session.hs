module Session where

import qualified System.CPUTime (getCPUTime)
import qualified Data.List.Extra (groupOn)
import qualified Data.List.Split (splitWhen, chunksOf)

data Session = Session { keystrokes :: [(Integer, Char)]
                       , text       :: String
                       }

sessionFromText :: String -> Session
sessionFromText t = Session { keystrokes = []
                            , text       = t  }

completeSession :: Session -> Bool
completeSession s = (length $ keystrokes s) >= (length $ text s)

recordKeystroke :: Session -> Char -> IO Session
recordKeystroke s c =
    if completeSession s
    then return s
    else do
        t <- System.CPUTime.getCPUTime
        return s { keystrokes = (keystrokes s) ++ [(t, c)] }

renderKeystrokes :: Session -> (String -> Maybe Bool -> a) -> [[[a]]]
renderKeystrokes s t =
    let remainingLength = (length $ text s) - (length $ keystrokes s)
        matchTuple '\n' (Just g) = ('\n', Just ('\n' == g))
        matchTuple e    (Just g) = (g,    Just (e    == g))
        matchTuple e    Nothing  = (e,    Nothing)
    in map (map (map (\g -> t (map fst g) (snd $ head g))))
       $ map (map (Data.List.Extra.groupOn snd))
       $ map (Data.List.Split.chunksOf 50)
       $ Data.List.Split.splitWhen ((== '\n') . fst)
       $ zipWith matchTuple (text s)
       $ (map Just $ map snd $ keystrokes s) ++
         (replicate remainingLength Nothing)
