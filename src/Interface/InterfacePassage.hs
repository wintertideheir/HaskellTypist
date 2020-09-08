module Interface.InterfacePassage where

import Passage
import Interface

import qualified Brick as B

data InterfacePassage = InterfacePassage { passages   :: [Passage] }

instance Interface InterfacePassage where
    input interface _ = B.continue interface
    draw _ = B.str "Nothing yet!"
