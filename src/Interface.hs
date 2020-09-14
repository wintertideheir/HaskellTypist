module Interface where

import Passage
import Themes

import qualified Control.Monad.IO.Class
import qualified Data.List.Extra
import qualified Data.Time.Clock

import qualified Brick        as B
import qualified Graphics.Vty as V

data Interface = IPassage { passages   :: [Passage] }
               | ISession { passages   :: [Passage]
                          , keystrokes :: [Keystroke]
                          }

input :: Interface -> B.BrickEvent () () -> B.EventM () (B.Next Interface)
input interface@(IPassage _)   (B.VtyEvent (V.EvKey V.KEsc []))      = B.halt interface
input interface@(ISession _ _) (B.VtyEvent (V.EvKey V.KEsc []))      = B.halt interface
input interface@(ISession _ _) (B.VtyEvent (V.EvKey (V.KChar c) [])) = Control.Monad.IO.Class.liftIO (record interface c)    >>= B.continue
input interface@(ISession _ _) (B.VtyEvent (V.EvKey V.KEnter    [])) = Control.Monad.IO.Class.liftIO (record interface '\n') >>= B.continue
input interface              _                                       = B.continue interface

draw  :: Interface -> B.Widget ()
draw interface@(IPassage _) =
    if null interface.passages
    then B.str "Nothing yet!"
    else let column n f = B.vBox
                        $ ((B.withAttr themeSpecial $ B.str n):)
                        $ map (B.str . f)
                        $ take 5 interface.passages
             pad l s = if length s < l
                       then s ++ (replicate (l - length s) ' ')
                       else s
         in B.hBox [column (pad (7 +1) "ID")      (take 7  . ('#':) . show . uid),
                    column (pad (49+1) "Passage") (take 49 . name),
                    column (pad 10     "Date")    (take 10 . show . Data.Time.Clock.utctDay . date)]
draw interface@(ISession _ _) =
    let checkedLines = map groupByScore
                     $ groupByLines
                     $ Passage.render (head interface.passages) interface.keystrokes
        normalLines = B.vBox
                    $ map B.hBox
                    $ map (map (\(s, m) -> themeRendered themeNormal (filter (/= '\n') s) m))
                    $ checkedLines
        specialLines = B.vBox
                     $ map (\(s, m) -> case last s of
                                       '\n' -> themeRendered themeSpecial "\\n" m
                                       _    -> themeRendered themeSpecial "<-"  m)
                     $ map last
                     $ checkedLines
    in normalLines B.<+> specialLines

record :: Interface -> Char -> IO Interface
record interface@(ISession _ _) c =
    do keystroke' <- toKeystroke c
       return interface{keystrokes ++ [keystroke']}

themeRendered :: B.AttrName -> String -> Maybe Bool -> B.Widget ()
themeRendered t s Nothing = B.showCursor () (B.Location (0, 0))
                          $ B.withAttr t
                          $ B.str s
themeRendered t s (Just True)  = B.withAttr (t <> themeMatch) $ B.str s
themeRendered t s (Just False) = B.withAttr (t <> themeMiss)  $ B.str s

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
