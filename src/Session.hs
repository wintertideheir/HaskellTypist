module Session where

import qualified System.CPUTime (getCPUTime)
import qualified Data.List.Extra (groupOn)

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

renderKeystrokes :: Session -> (String -> Maybe Bool -> a) -> [[a]]
renderKeystrokes s t =
    let zipKeystrokes (k:ks) ('\n':ts) = ('\n', Just (k == '\n')) : zipKeystrokes ks ts
        zipKeystrokes (k:ks) (t   :ts) = (k,    Just (k == t))    : zipKeystrokes ks ts
        zipKeystrokes []     (t   :ts) = (t,    Nothing)          : zipKeystrokes [] ts
        zipKeystrokes []     []        = []

        lineShouldEnd line ' '  = length line > 50
        lineShouldEnd _    '\n' = True
        lineShouldEnd _    _    = False

        stackReadable []           cm = [[cm]]
        stackReadable (line:lines) cm =
            if lineShouldEnd line (fst cm)
            then []:(cm:line):lines
            else    (cm:line):lines
    in map (map (\g -> t (map fst g) (snd $ head g)))
       $ map (Data.List.Extra.groupOn snd)
       $ reverse
       $ map reverse
       $ foldl stackReadable []
       $ zipKeystrokes (map snd $ keystrokes s) (text s)
