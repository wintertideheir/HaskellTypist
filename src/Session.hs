module Session where

import qualified System.CPUTime (getCPUTime)
import qualified Data.List.Extra (groupOn)
import qualified Data.Audio (Audio)

-- |A fragment, often a sentence, of a passage.
data PassageFragment = PassageTextFragment String
                     | PassageAudioFragment String (Data.Audio.Audio Float)

-- |A complete passage, with it's identifier, name,
-- and list of fragments.
data Passage = Passage { passageId        :: Int,
                         passageName      :: String
                         passageFragments :: [PassageFragment]}

-- |A session preset, composed of it's identifier, name,
-- a list of passage and fragment indicies, and previous
-- session data. Passage identifiers with an empty list are
-- interpreted as including the entire passage.
data SessionPreset = SessionPreset { sessionId        :: Int,
                                     sessionNam       :: String,
                                     sessionFragments :: [(Int, [Int])],
                                     sessionPrevious  :: [Session] }

-- |A typing session, which is a list of typed characters and
-- their time in picoseconds.
data Session = Session [(Integer, Char)]

data TypistData = TypistData [Passage] [SessionPreset]

-------------------------------------------------------------
-- TODO: Rewrite the below code to utilize the above types --
-------------------------------------------------------------

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
