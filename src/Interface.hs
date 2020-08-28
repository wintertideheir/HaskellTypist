module Interface where

import qualified Data.List.Extra         (groupOn)
import qualified Data.Time.Clock.System  (SystemTime, systemSeconds, systemNanoseconds)
import qualified Brick                   (BrickEvent, EventM, Next, Widget)

-----------------------------------------------------------------------
--                            Interfaces                             --
-----------------------------------------------------------------------

class Interface a where
    input :: a -> Brick.BrickEvent () () -> Brick.EventM () (Brick.Next a)
    draw  :: a -> Brick.Widget ()


-----------------------------------------------------------------------
--                            Auxiliary                              --
-----------------------------------------------------------------------

asCentiseconds :: Data.Time.Clock.System.SystemTime -> Int
asCentiseconds x =
    let seconds'     = (fromIntegral $ Data.Time.Clock.System.systemSeconds x)     * 100           :: Int
        nanoseconds' = (fromIntegral $ Data.Time.Clock.System.systemNanoseconds x) `quot` 10000000 :: Int
    in seconds' + nanoseconds'

groupByLines :: [(Char, Maybe Bool)] -> [[(Char, Maybe Bool)]]
groupByLines x =
    let lineShouldEnd l  ' '  = length l > 50
        lineShouldEnd _  '\n' = True
        lineShouldEnd _  _    = False
        stackReadable [] c     = [[c]]
        stackReadable (l:ls) c =
            if lineShouldEnd l (fst c)
            then []:(c:l):ls
            else    (c:l):ls
    in map reverse
       $ reverse
       $ foldl stackReadable [] x

groupByScore :: [(Char, Maybe Bool)] -> [(String, Maybe Bool)]
groupByScore x =
    let collapseSameScore [] = ([],        Nothing)
        collapseSameScore l  = (map fst l, snd $ head l)
    in map collapseSameScore
       $ Data.List.Extra.groupOn snd x
