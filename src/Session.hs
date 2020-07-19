module Session where

import qualified System.CPUTime (getCPUTime)
import qualified Data.List.Extra (groupOn)

data Session = Session { keystrokes :: [(Integer, Char)]
                       , text       :: String
                       }

sessionFromString :: String -> Session
sessionFromString t = Session { keystrokes = []
                              , text       = t  }

sessionComplete :: Session -> Bool
sessionComplete s = (length $ keystrokes s) >= (length $ text s)

recordKeystroke :: Session -> Char -> IO Session
recordKeystroke s c =
    if sessionComplete s
    then return s
    else do
        t <- System.CPUTime.getCPUTime
        return s { keystrokes = (keystrokes s) ++ [(t, c)] }

sessionCheckedLines :: Session -> [[(String, Maybe Bool)]]
sessionCheckedLines Session { keystrokes = k
                            , text       = t } =
    let zipKeystrokes (k:ks) (t   :ts) = (t,    Just (k == t))    : zipKeystrokes ks ts
        zipKeystrokes []     (t   :ts) = (t,    Nothing)          : zipKeystrokes [] ts
        zipKeystrokes []     []        = []

        lineShouldEnd line ' '  = length line > 50
        lineShouldEnd _    '\n' = True
        lineShouldEnd _    _    = False

        stackReadable = reverse
                      . map reverse
                      . foldl stackReadable' []
        stackReadable' []           cm = [[cm]]
        stackReadable' (line:lines) cm =
            if lineShouldEnd line (fst cm)
            then []:(cm:line):lines
            else    (cm:line):lines
    in map (map (\g -> (map fst g, snd $ head g)))
       $ map (Data.List.Extra.groupOn snd)
       $ stackReadable
       $ zipKeystrokes (map snd k) t
