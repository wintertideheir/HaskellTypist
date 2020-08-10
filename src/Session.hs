module Session where

import qualified Data.List.Extra         (groupOn)
import qualified Data.Audio              (Audio)
import qualified Data.Time.Clock         (UTCTime, getCurrentTime)
import qualified Data.Time.Clock.System  (SystemTime)
import qualified Data.Text               (pack)
import qualified Data.List
import qualified Data.Text.Metrics       (damerauLevenshteinNorm)

----------------------------------------------------------------------
--                              Types                               --
----------------------------------------------------------------------

data Metadata = Metadata { uid  :: Int
                         , name :: String
                         , date :: Data.Time.Clock.UTCTime
                         }

data Fragment = FragmentText  String
              | FragmentAudio String (Data.Audio.Audio Float)

data Passage = Passage Metadata [Fragment]

data Reference = ReferenceAll  Int
               | ReferencePart Int [Int]

data Preset = Preset Metadata [Reference] [Session]

data Keystroke = Keystroke Data.Time.Clock.System.SystemTime Char

data Session = Session Data.Time.Clock.UTCTime [Keystroke]

data TypistData = TypistData [Passage] [Preset]

----------------------------------------------------------------------
--                           Constuctors                            --
----------------------------------------------------------------------

{-|
    Create new 'Metadata' given a list of existing 'Metadata' and a name.
    All non-negative values of 'Int' are possible identifiers. If a unique
    identifier cannot be found, throw an error. The date recorded is the time
    of evaluation.
-}
newMetadata :: [Metadata] -> String -> IO Metadata
newMetadata m name' =
    case [0..maxBound] Data.List.\\ (map uid m) of
         []       -> error ("No unique identifier possible for \"" ++ show name' ++ "\".")  
         (uid':_) -> do t <- Data.Time.Clock.getCurrentTime
                        return Metadata { uid  = uid'
                                        , name = name'
                                        , date = t
                                        }

newPassage :: TypistData -> String -> [Fragment] -> IO TypistData
newPassage (TypistData p1 p2) n f =
    do m <- newMetadata (map (\(Passage m' _) -> m') p1) n
       return (TypistData (Passage m f : p1) p2)

newPreset :: TypistData -> String -> [Reference] -> IO TypistData
newPreset (TypistData p1 p2) n r =
    do m <- newMetadata (map (\(Preset m' _ _) -> m') p2) n
       return (TypistData p1 (Preset m r [] : p2))

newSession :: TypistData -> Int -> [Keystroke] -> IO TypistData
newSession (TypistData p1 p2) i k =
    do t <- Data.Time.Clock.getCurrentTime
       let modifyOrError _ _ e []     = e
           modifyOrError c trans e (x:xs) =
               if c x
               then (trans x) : xs
               else x         : (modifyOrError c trans e xs)
           p2' = modifyOrError
                     (\(Preset m _ _) -> uid m == i)
                     (\(Preset m r s) -> Preset m r (Session t k : s))
                     (error ("No preset with UID " ++ show i ++ " to add session to."))
                     p2
       return (TypistData p1 p2')

----------------------------------------------------------------------
--                            Retrieval                             --
----------------------------------------------------------------------

{-|
    Retrive a list of passage fragments given a reference. Out of bounds
    references throw errors.
-}
dereference :: TypistData -> Reference -> [Fragment]
dereference (TypistData p _) (ReferenceAll  uid'  ) =
    case Data.List.find (\(Passage m _) -> uid m == uid') p of
        Just (Passage _ f) -> f
        Nothing            -> error ("No passage with UID " ++ show uid' ++ ".")
dereference (TypistData p _) (ReferencePart uid' i) =
    let error1 = error ("Out of bounds passage fragment with passage UID " ++ show uid' ++ ".")
        error2 = error ("No passage with UID "                             ++ show uid' ++ ".")
        indexFragment f i' = if i' > (length f)
                            then error1
                            else f !! i'
    in case Data.List.find (\(Passage m _) -> uid m == uid') p of
           Just (Passage _ f) -> map (indexFragment f) i
           Nothing            -> error2

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
renderFragments pfs "" = concat [renderFragment pf "" | pf <- pfs]
renderFragments pfs s  =
    let renderFragments' pfs' "" _ = renderFragments pfs' ""
        renderFragments' []   x  _ = zip x (repeat Nothing)
        renderFragments' (pf':pfs') s' l =
            if renderFragment pf' (take l s') == renderFragment pf' (take (l+1) s')
            then (renderFragment pf' (take l s')) ++ (renderFragments' pfs' (drop l s') 0)
            else renderFragments' (pf':pfs') s' (l+1)
    in renderFragments' pfs s 0
