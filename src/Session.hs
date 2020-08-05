module Session where

import qualified Data.List.Extra   (groupOn)
import qualified Data.Audio        (Audio)
import qualified Data.Time.Clock   (UTCTime, getCurrentTime)
import qualified Data.Text         (pack)
import qualified Data.List
import qualified Data.Text.Metrics (damerauLevenshteinNorm)

{-|
    A fragment, often a sentence, of a passage.
-}
data PassageFragment = PassageTextFragment String
                     | PassageAudioFragment String (Data.Audio.Audio Float)

{-|
    Consume a fragment from typed input. Taking a 'PassageFragment' and
    a 'String', render a list of characters with a possible normalized
    score.

    There should be a string s such that
    > consumeFragment s == consumeFragment (s ++ s')
    for any string s'. The string s \"completes\" the fragment.

    If the fragment is a 'PassageTextFragment', compare each typed
    character to the fragment and score it, rendering the entire fragment.

    If the fragment is a 'PassageAudioFragment', first decide whether
    the fragment is complete when the typed input is at least as long as
    the fragment itself, by checking if the string metric improves with
    consuming more characters. If a 'PassageAudioFragment' is complete,
    then render the fragment with it's string metrics, otherwise return
    the typed input.
-}
consumeFragment :: PassageFragment -> String -> [(Char, Maybe Float)]
consumeFragment (PassageTextFragment [])     _      = []
consumeFragment (PassageTextFragment f)      []     = [(f', Nothing) | f' <- f]
consumeFragment (PassageTextFragment (f:fs)) (s:ss) =
    if f == s
    then (f, Just 1.0) : consumeFragment (PassageTextFragment fs) ss
    else (f, Just 0.0) : consumeFragment (PassageTextFragment fs) ss
consumeFragment (PassageAudioFragment f _) s =
    let subMetric l = fromRational
                      $ toRational
                      $ Data.Text.Metrics.damerauLevenshteinNorm
                        (Data.Text.pack (take l s))
                        (Data.Text.pack f)
                    :: Float
        decideMetric' l = if l >= length s
                          then map (\x -> (x, Nothing)) s
                          else if subMetric l >= subMetric (l+1)
                               then map (\x -> (x, Just (subMetric (l+1)))) f
                               else decideMetric' (l+1)
    in decideMetric' (length f)

{-|
    Consume a number of fragments.

    Repeatedly feeds substrings of the given typed input to the list of
    fragments in order, moving to the next fragment when the output of
    consumption doesn't change for a given substring length, then
    concatenating the output.

    The input string is treated as a substring of the concatenation of
    substrings that complete each fragment in the given list. 
-}
consumeFragments :: [PassageFragment] -> String -> [(Char, Maybe Float)]
consumeFragments pfs "" = concat [consumeFragment pf "" | pf <- pfs]
consumeFragments pfs s  =
    let consumeFragments' pfs' "" _ = consumeFragments pfs' ""
        consumeFragments' []   [] _ = []
        consumeFragments' (pf':pfs') s' l =
            if consumeFragment pf' (take l s') == consumeFragment pf' (take (l+1) s')
            then (consumeFragment pf' (take l s')) ++ (consumeFragments' pfs' (drop l s') 0)
            else consumeFragments' (pf':pfs') s' (l+1)
    in consumeFragments' pfs s 0

{-|
    A complete passage, with it's identifier, name, and list of fragments.
-}
data Passage = Passage { passageId        :: Int
                       , passageName      :: String
                       , passageDate      :: Data.Time.Clock.UTCTime
                       , passageFragments :: [PassageFragment]
                       }

{-|
    Retrive a list of passage fragments given their indices. Out of bounds
    indices are simply discarded.
-}
subPassage :: Passage -> [Int] -> [PassageFragment]
subPassage p i =
    map ((passageFragments p) !!)
    $ filter (>= 0)
    $ filter (< length (passageFragments p)) i

{-|
    A session preset, composed of it's identifier, name, a list of passage
    and fragment indicies, and previous session data.
-}
data SessionPreset = SessionPreset { sessionId        :: Int
                                   , sessionName      :: String
                                   , sessionDate      :: Data.Time.Clock.UTCTime
                                   , sessionFragments :: [(Int, [Int])]
                                   , sessionPrevious  :: [Session]
                                   }

{-|
    A typing session, which is a list of typed characters and their time in
    picoseconds.
-}
data Session = Session Data.Time.Clock.UTCTime [(Integer, Char)]

data TypistData = TypistData [Passage] [SessionPreset]

{-|
    Constructor for a passage in a 'TypistData' given its name and list of
    fragments. Will assign the smallest unique, nonnegative integer
    identifier, and fails through 'head' otherwise. Records the date of
    creation through 'Data.Time.Clock.getCurrentTime'.
-}
addPassage :: TypistData -> String -> [PassageFragment] -> IO TypistData
addPassage (TypistData ps sps) name pfs =
    do t <- Data.Time.Clock.getCurrentTime
       let p = Passage { passageId        = head ([0..(maxBound :: Int)]
                                                  Data.List.\\
                                                  (map passageId ps))
                       , passageName      = name
                       , passageDate      = t
                       , passageFragments = pfs
                       }
       return (TypistData (p:ps) sps)

{-|
    Constructor for a session preset in a 'TypistData' given its name and
    list of session and fragment indices. Will assign the smallest unique,
    nonnegative integer identifier, and fails through 'head' otherwise.
    Records the date of creation through 'Data.Time.Clock.getCurrentTime'.
-}
addPreset :: TypistData -> String -> [(Int, [Int])] -> IO TypistData
addPreset (TypistData ps sps) name sfs =
    do t <- Data.Time.Clock.getCurrentTime
       let sp = SessionPreset { sessionId        = head ([0..(maxBound :: Int)]
                                                         Data.List.\\
                                                         (map sessionId sps))
                              , sessionName      = name
                              , sessionDate      = t
                              , sessionFragments = sfs
                              , sessionPrevious  = []
                              }
       return (TypistData ps (sp:sps))

{-|
    Appends a session to its session preset in a 'TypistData'. If the given
    session preset identifer does not exist, fails through incomplete
    pattern matching. Records the date of addition through
    'Data.Time.Clock.getCurrentTime'.
-}
addSession :: TypistData -> Int -> [(Integer, Char)] -> IO TypistData
addSession (TypistData ps sps) identifer ks =
    do t <- Data.Time.Clock.getCurrentTime
       let (sps1, (sp : sps2)) = break ((== identifer) . sessionId) sps
           s = Session t ks
           sp' = sp { sessionPrevious = s : sessionPrevious sp }
       return (TypistData ps (sps1 ++ (sp':sps2)))

{-|
    Retrive a list of passage fragments from different passages given the
    passage identifiers and fragments indices. Out of bounds indices and
    identifers are simply discarded.
-}
subPassages :: TypistData -> [(Int, [Int])] -> [PassageFragment]
subPassages (TypistData p _) identifiers =
    let subPassages' []           = []
        subPassages' ((x1,x2):xs) =
            case Data.List.find ((== x1) . passageId) p of
                Nothing ->                     (subPassages' xs)
                Just j  -> (subPassage j x2) : (subPassages' xs)
    in concat $ subPassages' identifiers

{-|
    Consume a string based on a session preset identifier.
-}
consumePreset :: TypistData -> Int -> String -> [(Char, Maybe Float)]
consumePreset td@(TypistData _ sps) identifier s =
    case Data.List.find ((== identifier) . sessionId) sps of
        Just sp -> consumeFragments (subPassages td
                                     $ sessionFragments sp) s
        Nothing -> []
