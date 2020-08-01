module Session where

import qualified System.CPUTime (getCPUTime)
import qualified Data.List.Extra (groupOn)
import qualified Data.Audio (Audio)
import qualified Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.Text.Metrics (damerauLevenshteinNorm)

-- |A fragment, often a sentence, of a passage.
data PassageFragment = PassageTextFragment String
                     | PassageAudioFragment String (Data.Audio.Audio Float)

{-|
    Consume a fragment from typed input. Taking a 'PassageFragment' and
    a 'String', render a list of characters with a possible normalized
    score.

    If the fragment is a 'PassageTextFragment', compare each typed
    character to the fragment and score it, rendering the entire fragment.

    If the fragment is a 'PassageAudioFragment', first decide whether
    the fragment is complete when the typed input is at least as long as
    the fragment itself, by checking if the string metric improves with
    consuming more characters. If a 'PassageAudioFragment' is complete,
    then render the fragment with it's string metrics, otherwise return
    the typed input.

    'consumeFragment' is the foundation for a consumer based input
    processing system where determining completeness is handled by
    detecting changed in rendered output.
-}
consumeFragment :: PassageFragment -> String -> [(Char, Maybe Float)]
consumeFragment (PassageTextFragment  [])     _ = []
consumeFragment (PassageTextFragment  (f:fs)) [] =
    map (f, Nothing) : consumeFragment (PassageTextFragment fs) []
consumeFragment (PassageTextFragment  (f:fs)) (s:ss) =
    if f == s
    then (f, Just 1.0) : consumeFragment (PassageTextFragment fs) ss
    else (f, Just 0.0) : consumeFragment (PassageTextFragment fs) ss
consumeFragment (PassageAudioFragment f _) s =
    let subMetric l = Data.Text.Metrics.damerauLevenshteinNorm (pack (take l s)) (pack f)
        decideMetric' l = if l >= length s
                          then map (\x -> (x, Nothing)) s
                          else if subMetric l >= subMetric (l+1)
                               then map (\x -> (x, subMetric (l+1))) f
                               else decideMetric' (l+1)
    in decideMetric' (length f)

-- |A complete passage, with it's identifier, name,
-- and list of fragments.
data Passage = Passage { passageId        :: Int
                       , passageName      :: String
                       , passageDate      :: Data.Time.Clock.UTCTime
                       , passageFragments :: [PassageFragment]
                       }

-- |A session preset, composed of it's identifier, name,
-- a list of passage and fragment indicies, and previous
-- session data. Passage identifiers with an empty list are
-- interpreted as including the entire passage.
data SessionPreset = SessionPreset { sessionId        :: Int
                                   , sessionName      :: String
                                   , sessionDate      :: Data.Time.Clock.UTCTime
                                   , sessionFragments :: [(Int, [Int])]
                                   , sessionPrevious  :: [Session]
                                   }

-- |A typing session, which is a list of typed characters and
-- their time in picoseconds.
data Session = Session Data.Time.Clock.UTCTime [(Integer, Char)]

data TypistData = TypistData [Passage] [SessionPreset]

-- |Constructor for a passage in a 'TypistData'
-- given its name and list of fragments. Will
-- assign the smallest unique, nonnegative
-- integer identifier, and fails through 'head'
-- otherwise. Records the date of creation through
-- 'Data.Time.Clock.getCurrentTime'.
addPassage :: TypistData -> String -> [PassageFragment] -> IO TypistData
addPassage (TypistData ps sps) name pfs =
    do t <- Data.Time.Clock.getCurrentTime
       let p = Passage { passageId        = head ([0..(maxBound :: Int)] \\ (map passageId ps))
                       , passageName      = name
                       , passageDate      = t
                       , passageFragments = pfs
                       }
       TypistData p:ps sps

-- |Constructor for a session preset in a
-- 'TypistData' given its name and list of session
-- and fragment indices. Will assign the smallest
-- unique, nonnegative integer identifier, and
-- fails through 'head' otherwise. Records the date
-- of creation through
-- 'Data.Time.Clock.getCurrentTime'.
addPreset :: TypistData -> String -> [(Int, [Int])] -> IO TypistData
addPreset (TypistData ps sps) name sfs =
    do t <- Data.Time.Clock.getCurrentTime
       let sp = SessionPreset { sessionId        = head ([0..(maxBound :: Int)] \\ (map sessionId sps))
                              , sessionName      = name
                              , sessionDate      = t
                              , sessionFragments = sfs
                              , sessionPrevious  = []
                              }
       TypistData ps sp:sps

-- |Appends a session to its session preset in a
-- 'TypistData'. If the given session preset
-- identifer does not exist, fails through
-- incomplete pattern matching. Records the date of
-- addition through
-- 'Data.Time.Clock.getCurrentTime'.
addSession :: TypistData -> Int -> [(Integer, Char)] -> IO TypistData
addSession (TypistData ps sps) id ks =
    do t <- Data.Time.Clock.getCurrentTime
       let (sps1, (sp : sps2)) = break ((== id) . sessionId) sps
           s = Session t ks
           sp' = sp { sessionPrevious = s : sessionPrevious sp }
       TypistData ps (sps1 ++ (sp':sps2))

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
