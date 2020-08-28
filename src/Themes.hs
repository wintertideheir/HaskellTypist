module Themes where

import Brick
import Graphics.Vty

themes       :: AttrMap
themeMiss    :: AttrName
themeMatch   :: AttrName
themeNormal  :: AttrName
themeSpecial :: AttrName
themes = attrMap (black `on` white)
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

rgbColorInt :: Int -> Int -> Int -> Color
rgbColorInt = rgbColor

themeRendered :: AttrName -> String -> Maybe Bool -> Widget ()
themeRendered t s Nothing = Brick.showCursor () (Location (0, 0))
                          $ withAttr t
                          $ str s
themeRendered t s (Just True)  = withAttr (t <> themeMatch) $ str s
themeRendered t s (Just False) = withAttr (t <> themeMiss)  $ str s