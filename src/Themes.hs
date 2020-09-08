module Themes where

import Brick
import Graphics.Vty

themes       :: AttrMap
themeMiss    :: AttrName
themeMatch   :: AttrName
themeNormal  :: AttrName
themeSpecial :: AttrName
themes =
    let rgbColorInt :: Int -> Int -> Int -> Color
        rgbColorInt = rgbColor
    in attrMap (black `on` white)
           [ (themeNormal,                black `on` white)
           , (themeNormal <> themeMiss,   bg $ rgbColorInt 255 150 150)
           , (themeNormal <> themeMatch,  fg $ rgbColorInt  50  50  50)
           , (themeSpecial,               bg $ rgbColorInt 200 200 200)
           , (themeSpecial <> themeMiss,  fg $ rgbColorInt 255  50  50)
           , (themeSpecial <> themeMatch, fg $ rgbColorInt 150 150 150)
           ]
themeMiss    = attrName "miss"
themeMatch   = attrName "match"
themeNormal  = attrName "normal"
themeSpecial = attrName "special"
