{-# LANGUAGE DuplicateRecordFields #-}

module Session where

import qualified Data.Audio              (Audio)
import qualified Data.Time.Clock         (UTCTime, getCurrentTime)
import qualified Data.Time.Clock.System  (SystemTime, getSystemTime)
import qualified Data.Text               (pack)
import qualified Data.List
import qualified Data.List.Extra         (groupOn)
import qualified Data.Text.Metrics       (damerauLevenshteinNorm)

----------------------------------------------------------------------
--                              Types                               --
----------------------------------------------------------------------

data Fragment = FragmentText  String
              | FragmentAudio String (Data.Audio.Audio Float)

data Passage = Passage { uid       :: Int
                       , name      :: String
                       , date      :: Data.Time.Clock.UTCTime
                       , fragments :: [Fragment]
                       }

data Reference = ReferenceAll  Int
               | ReferencePart Int [Int]

data Preset = Preset { uid        :: Int
                     , name       :: String
                     , date       :: Data.Time.Clock.UTCTime
                     , references :: [Reference]
                     , sessions   :: [Session]
                     }

data Keystroke = Keystroke Data.Time.Clock.System.SystemTime Char

data Session = Session Data.Time.Clock.UTCTime [Keystroke]

data TypistData = TypistData { passages :: [Passage]
                             , presets  :: [Preset]
                             }
----------------------------------------------------------------------
--                             Utility                              --
----------------------------------------------------------------------

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

----------------------------------------------------------------------
--                           Constuctors                            --
----------------------------------------------------------------------

newPassage :: TypistData -> String -> [Fragment] -> IO TypistData
newPassage t name' fragments' =
    do date' <- Data.Time.Clock.getCurrentTime
       let uid' = fallibleFind ("No unique passage identifier possible for \"" ++ name' ++ "\".")
                               (`notElem` (map (uid :: Passage -> Int) t.passages))
                               [0..maxBound]
           passage = Passage { uid       = uid'
                             , name      = name'
                             , date      = date'
                             , fragments = fragments'
                             }
       return t{ passages `prefix` passage }

newPreset :: TypistData -> String -> [Reference] -> IO TypistData
newPreset t name' references' =
    do date' <- Data.Time.Clock.getCurrentTime
       let uid' = fallibleFind ("No unique preset identifier possible for \"" ++ name' ++ "\".")
                               (`notElem` (map (uid :: Preset -> Int) t.presets))
                               [0..maxBound]
           preset = Preset { uid        = uid'
                           , name       = name'
                           , date       = date'
                           , references = references'
                           , sessions   = []
                           }
       return t{ presets `prefix` preset }

newSession :: TypistData -> Int -> [Keystroke] -> IO TypistData
newSession t uid' k =
    do date' <- Data.Time.Clock.getCurrentTime
       return t{ presets = fallibleReplace ("No preset with ID " ++ show uid' ++ " to save session to.")
                                           ((== uid') . (uid :: Preset -> Int))
                                           (\p -> p{sessions `prefix` (Session date' k)})
                                           t.presets }

toKeystroke :: Char -> IO Keystroke
toKeystroke c =
    do t <- Data.Time.Clock.System.getSystemTime
       return (Keystroke t c)

----------------------------------------------------------------------
--                            Retrieval                             --
----------------------------------------------------------------------

{-|
    Retrive a list of passage fragments given a reference. Out of bounds
    references throw errors.
-}
dereference :: TypistData -> Reference -> [Fragment]
dereference t (ReferenceAll uid') =
    fragments $ fallibleFind ("Invalid reference with passage UID " ++ show uid' ++ ".")
                             ((== uid') . (uid :: Passage -> Int))
                             t.passages
dereference t (ReferencePart uid' indices) =
    map (\i -> fallibleIndex ("Out of bounds reference with passage UID " ++
                              show uid' ++ " and fragment index" ++ show i ++ ".")
                             (dereference t $ ReferenceAll uid') i)
        indices

----------------------------------------------------------------------
--                              Render                              --
----------------------------------------------------------------------

{-|
    Render a fragment from typed input. Taking a 'Fragment' and
    a 'String', render a list of characters with a possible normalized
    score.

    There should be a string s such that
    > renderFragment s == renderFragment (s ++ s')
    for any string s'. The string s \"completes\" the fragment.

    If the fragment is a 'FragmentText', compare each typed
    character to the fragment and score it, rendering the entire fragment.

    If the fragment is a 'FragmentAudio', first decide whether
    the fragment is complete when the typed input is at least as long as
    the fragment itself, then by checking if the string metric improves with
    consuming more characters. If a 'FragmentAudio' is complete,
    then render the fragment with it's string metrics, otherwise return
    the typed input.
-}
renderFragment :: Fragment -> String -> [(Char, Maybe Float)]
renderFragment (FragmentText [])     _      = []
renderFragment (FragmentText f)      []     = [(f', Nothing) | f' <- f]
renderFragment (FragmentText (f:fs)) (s:ss) =
    if f == s
    then (f, Just 1.0) : renderFragment (FragmentText fs) ss
    else (f, Just 0.0) : renderFragment (FragmentText fs) ss
renderFragment (FragmentAudio f _) s =
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
    Render a number of fragments.

    Repeatedly feeds substrings of the given typed input to the list of
    fragments in order, moving to the next fragment when the output of
    consumption doesn't change for a given substring length, then
    concatenating the output.

    The input string is treated as a substring of the concatenation of
    substrings that complete each fragment in the given list. 
-}
renderFragments :: [Fragment] -> String -> [(Char, Maybe Float)]
renderFragments []       s  = zip s (repeat Nothing)
renderFragments pfs      "" = concat [renderFragment pf "" | pf <- pfs]
renderFragments (pf:pfs) s  =
    let textLength (FragmentText  s'  ) = length s'
        textLength (FragmentAudio s' _) = length s'
        renderFragments' pf' pfs' s' l =
            if renderFragment pf' (take l s') == renderFragment pf' (take (l+1) s')
            then (renderFragment pf' (take l s')) ++ (renderFragments pfs' (drop l s'))
            else renderFragments' pf' pfs' s' (l+1)
    in renderFragments' pf pfs s (textLength pf)

groupByLines :: [(Char, Maybe Float)] -> [[(Char, Maybe Float)]]
groupByLines x =
    let lineShouldEnd l  ' '  = length l > 50
        lineShouldEnd _  '\n' = True
        lineShouldEnd _  _    = False
        stackReadable [] c     = [[c]]
        stackReadable (l:ls) c =
            if lineShouldEnd l (fst c)
            then []:(c:l):ls
            else    (c:l):ls
    in foldl stackReadable [] x

groupByScore :: [(Char, Maybe Float)] -> [(String, Maybe Float)]
groupByScore x =
    let collapseSameScore [] = ([],        Nothing)
        collapseSameScore l  = (map fst l, snd $ head l)
    in map collapseSameScore
       $ Data.List.Extra.groupOn snd x
