module Interface where

import qualified Brick

class Interface a where
    input :: a -> Brick.BrickEvent () () -> Brick.EventM () (Brick.Next a)
    draw  :: a -> Brick.Widget ()
