module Session where

import qualified System.CPUTime (getCPUTime)

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

renderKeystrokes :: Session -> (Char -> Maybe Bool -> a) -> [a]
renderKeystrokes s t =
    let matched    = zip (map snd (keystrokes s)) (text s)
        complete   = map (\x -> t (snd x) (Just ((fst x) == (snd x)))) matched
        incomplete = map (\x -> t x Nothing)                           (drop (length $ keystrokes s) $ text s)
    in complete ++ incomplete
