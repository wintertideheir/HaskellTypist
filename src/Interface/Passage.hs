module Interface.Passage where

import Passage
import Interface

import qualified Brick as B
import qualified Graphics.Vty as V

data IPassage = IPassage { passages :: [Passage] }

instance Interface IPassage where
    input interface (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt interface
    input interface _ = B.continue interface
    draw _ = B.str "Nothing yet!"
