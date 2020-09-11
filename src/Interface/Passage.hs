module Interface.Passage where

import Passage
import Interface

import qualified Brick as B
import qualified Graphics.Vty as V

data IPassage = IPassage { passages :: [Passage] }

instance Interface IPassage where
    input interface (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt interface
    input interface _ = B.continue interface
    draw interface =
        if null interface.passages
        then B.str "Nothing yet!"
        else let column n f = B.padLeft (B.Pad 1)
                            $ B.vBox
                            $ ((B.str n):)
                            $ map (B.str . f)
                            $ take 5 interface.passages
             in B.hBox [column "ID"      (show . uid),
                        column "Passage" name,
                        column "Date"    (show . date)]
