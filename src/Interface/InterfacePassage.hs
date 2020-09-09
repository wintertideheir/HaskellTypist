module Interface.InterfacePassage where

import Passage
import Interface

import qualified Brick as B
import qualified Brick.Widgets.List as BL

import qualified Data.Functor

instance BL.Splittable [] where
    splitAt n x = (take n x, drop n x)
    slice i n = take n . drop i

data InterfacePassage = InterfacePassage { passage :: BL.GenericList () [] Passage }

instance Interface InterfacePassage where
    input interface _ = B.continue interface
    draw interface = BL.renderList (\_ e -> B.str e.name) True (passage interface)
