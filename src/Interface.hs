module Interface where

import qualified Brick                   (BrickEvent, EventM, Next, Widget)

class Interface a where
    input :: a -> Brick.BrickEvent () () -> Brick.EventM () (Brick.Next a)
    draw  :: a -> Brick.Widget ()
