{-# LANGUAGE CPP #-}

module Interface where

import Passage
import Themes
import Data.Focus

import qualified Control.Monad.IO.Class
import qualified Data.List.Extra
import qualified Data.Time.Clock

import qualified Brick        as B
import qualified Graphics.Vty as V

data Interface = IPassage { passages   :: Maybe (FocusedList Passage)
                          }
               | ISession { passage    :: FocusedList Passage
                          , keystrokes :: [Keystroke]
                          }

#define IPASSAGE(x) x@(IPassage _)
#define ISESSION(x) x@(ISession _ _)

input :: Interface -> B.BrickEvent () () -> B.EventM () (B.Next Interface)
input IPASSAGE(interface) (B.VtyEvent (V.EvKey V.KEsc []))      = B.halt interface
input IPASSAGE(interface) (B.VtyEvent (V.EvKey V.KUp []))       = B.continue (interface{passages = fmap focusUp interface.passages})
input IPASSAGE(interface) (B.VtyEvent (V.EvKey V.KDown []))     = B.continue (interface{passages = fmap focusDown interface.passages})
input IPASSAGE(interface) (B.VtyEvent (V.EvKey V.KEnter []))    = B.continue (case interface.passages of
                                                                                  Nothing -> interface
                                                                                  Just l  -> ISession l [])
input ISESSION(interface) (B.VtyEvent (V.EvKey V.KEsc []))      = B.halt interface
input ISESSION(interface) (B.VtyEvent (V.EvKey (V.KChar c) [])) = Control.Monad.IO.Class.liftIO (record interface c)    >>= B.continue
input ISESSION(interface) (B.VtyEvent (V.EvKey V.KEnter    [])) = Control.Monad.IO.Class.liftIO (record interface '\n') >>= B.continue
input interface           _                                     = B.continue interface

draw  :: Interface -> B.Widget ()
draw IPASSAGE(interface) =
    case interface.passages of
        Nothing -> B.str "Nothing yet!"
        Just passages' ->
            let column' l f = (map B.str
                              $ map (take l . f)
                              $ reverse
                              $ take 3
                              $ reverse
                              $ passages'.before)
                           ++ [B.withAttr (themeNormal <> themeMatch)
                              $ B.str
                              $ f
                              $ passages'.focus]
                           ++ (map B.str
                              $ map (take l . f)
                              $ take 3
                              $ passages'.after)
                column n l f = B.vBox
                               $ ((B.withAttr themeSpecial
                                  $ B.str
                                  $ pad (l+1)
                                  $ take l
                                  $ n):)
                               $ column' l f
                pad l s = if length s < l
                          then s ++ (replicate (l - length s) ' ')
                          else s
            in B.hBox [column "ID"      7  (('#':) . show . uid),
                       column "Passage" 49 name,
                       column "Date"    10 (show . Data.Time.Clock.utctDay . date)]
draw ISESSION(interface) =
    let checkedLines = map groupByScore
                     $ groupByLines
                     $ Passage.render interface.passage.focus interface.keystrokes
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
record ISESSION(interface) c =
    do keystroke' <- toKeystroke c
       return interface{keystrokes ++ [keystroke']}

boundedAdd :: Int -> Int -> Int -> Int -> Int
boundedAdd xmin xmax x1 x2 = max xmin (min xmax (x1 + x2))

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
