module Interface.Passage where

import Passage
import Interface
import Themes

import qualified Data.Time.Clock

import qualified Brick as B
import qualified Graphics.Vty as V

data IPassage = IPassage { passages :: [Passage] }

instance Interface IPassage where
    input interface (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt interface
    input interface _ = B.continue interface
    draw interface =
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
