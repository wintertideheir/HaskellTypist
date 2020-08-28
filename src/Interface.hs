module Interface where

import Passage
import Themes
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import qualified Graphics.Vty            as V
import qualified Data.Time.Clock         (getCurrentTime)
import qualified Data.List               (find)
import qualified Data.List.Extra         (groupOn)
import qualified Data.Time.Clock.System  (SystemTime, getSystemTime, systemSeconds, systemNanoseconds)
import qualified Control.Monad.IO.Class  (liftIO)

-----------------------------------------------------------------------
--                            Interfaces                             --
-----------------------------------------------------------------------

class Interface a where
    input :: a -> BrickEvent () () -> EventM () (Next a)
    draw  :: a -> [Widget ()]

data InterfaceSession = InterfaceSession { passages   :: [Passage]
                                         , begin      :: Maybe Data.Time.Clock.System.SystemTime
                                         , keystrokes :: [Keystroke]
                                         }

instance Interface InterfaceSession where
    input interface (VtyEvent (V.EvKey V.KEsc []))      = halt interface
    input interface (VtyEvent (V.EvKey (V.KChar c) [])) = Control.Monad.IO.Class.liftIO (record interface c)    >>= continue
    input interface (VtyEvent (V.EvKey V.KEnter    [])) = Control.Monad.IO.Class.liftIO (record interface '\n') >>= continue
    input interface _                                   = continue interface
    draw  interface =
        let checkedLines = map groupByScore
                         $ groupByLines
                         $ Passage.render (head interface.passages) interface.keystrokes
            normalLines = vBox
                        $ map hBox
                        $ map (map (\(s, m) -> themeRendered themeNormal (filter (/= '\n') s) m))
                        $ checkedLines
            specialLines = vBox
                         $ map (\(s, m) -> case last s of
                                           '\n' -> themeRendered themeSpecial "\\n" m
                                           _    -> themeRendered themeSpecial "<-"  m)
                         $ map last
                         $ checkedLines
        in [withBorderStyle unicode
            $ borderWithLabel (str "Haskell Typist")
            $ center
            $ (normalLines <+> specialLines)]


-----------------------------------------------------------------------
--                            Auxiliary                              --
-----------------------------------------------------------------------

asCentiseconds :: Data.Time.Clock.System.SystemTime -> Int
asCentiseconds x =
    let seconds'     = (fromIntegral $ Data.Time.Clock.System.systemSeconds x)     * 100           :: Int
        nanoseconds' = (fromIntegral $ Data.Time.Clock.System.systemNanoseconds x) `quot` 10000000 :: Int
    in seconds' + nanoseconds'

record :: InterfaceSession -> Char -> IO InterfaceSession
record td c =
    do t <- Data.Time.Clock.System.getSystemTime
       case td.begin of
           Just begin' -> return td{keystrokes ++ [Keystroke ((asCentiseconds t) - (asCentiseconds begin')) c]}
           Nothing     -> return td{keystrokes ++ [Keystroke 0 c],
                                    begin = Just t}

newPassage :: InterfaceSession -> String -> String -> IO InterfaceSession
newPassage td name' text' =
    do date' <- Data.Time.Clock.getCurrentTime
       let uid' = fallibleFind ("No unique passage identifier possible for \"" ++ name' ++ "\".")
                               (`notElem` (map uid td.passages))
                               [0..maxBound]
           passage = Passage { uid      = uid'
                             , name     = name'
                             , date     = date'
                             , text     = text'
                             , sessions = []
                             }
       return td{passages ++ [passage]}

newSession :: InterfaceSession -> Int -> [Keystroke] -> IO InterfaceSession
newSession td uid' k =
    do date' <- Data.Time.Clock.getCurrentTime
       let session = Session date' k
           passages' = fallibleReplace
                           ("No passage with ID " ++ show uid' ++ " to save session to.")
                           ((== uid') . uid)
                           (\passage -> passage{sessions ++ [session]})
                           td.passages
       return td{passages = passages'}

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

groupByLines :: [(Char, Maybe Bool)] -> [[(Char, Maybe Bool)]]
groupByLines x =
    let lineShouldEnd l  ' '  = length l > 50
        lineShouldEnd _  '\n' = True
        lineShouldEnd _  _    = False
        stackReadable [] c     = [[c]]
        stackReadable (l:ls) c =
            if lineShouldEnd l (fst c)
            then []:(c:l):ls
            else    (c:l):ls
    in map reverse
       $ reverse
       $ foldl stackReadable [] x

groupByScore :: [(Char, Maybe Bool)] -> [(String, Maybe Bool)]
groupByScore x =
    let collapseSameScore [] = ([],        Nothing)
        collapseSameScore l  = (map fst l, snd $ head l)
    in map collapseSameScore
       $ Data.List.Extra.groupOn snd x
