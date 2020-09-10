module Interface.Passage where

import Passage
import Interface

import qualified Brick as B

data IPassage = IPassage { passage :: [Passage] }

instance Interface IPassage where
    input interface _ = B.continue interface
    draw _ = B.str "Nothing yet!"
