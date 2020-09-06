{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Passage where

import qualified Data.Char
import qualified Data.Time.Clock
import qualified Data.Time.Clock.System
import qualified Data.Time.Calendar
import qualified Data.List
import qualified Flat
import qualified Flat.Decoder.Types
import qualified GHC.Generics
import qualified System.FilePath

-----------------------------------------------------------------------
--                          Serialization                            --
-----------------------------------------------------------------------

deriving instance GHC.Generics.Generic Data.Time.Clock.System.SystemTime
deriving instance GHC.Generics.Generic Data.Time.Calendar.Day
deriving instance GHC.Generics.Generic Data.Time.Clock.UTCTime
deriving instance Flat.Flat            Data.Time.Clock.System.SystemTime
deriving instance Flat.Flat            Data.Time.Calendar.Day
deriving instance Flat.Flat            Data.Time.Clock.UTCTime

instance Flat.Flat Data.Time.Clock.DiffTime where
    encode = Flat.encode . Data.Time.Clock.diffTimeToPicoseconds
    decode = fmap Data.Time.Clock.picosecondsToDiffTime Flat.decode
    size   = Flat.size . Data.Time.Clock.diffTimeToPicoseconds

data Transient a = Transient a

instance Functor Transient where
    fmap f (Transient a) = Transient (f a)

class HasDefault a where
    defaultValue :: a

instance HasDefault [a] where
    defaultValue = []

instance (HasDefault a) => Flat.Flat (Transient a) where
    encode _ = Flat.encode ()
    decode   = (Transient defaultValue) <$ (Flat.decode :: Flat.Decoder.Types.Get ())
    size   _ = Flat.size ()

-----------------------------------------------------------------------
--                              Types                                --
-----------------------------------------------------------------------

data Keystroke = Keystroke Data.Time.Clock.System.SystemTime Char
    deriving (GHC.Generics.Generic, Flat.Flat)

fromKeystroke :: Keystroke -> Char
fromKeystroke (Keystroke _ c) = c

toKeystroke :: Char -> IO Keystroke
toKeystroke c = ($ c) <$> (Keystroke <$> Data.Time.Clock.System.getSystemTime)

data Session = Session Data.Time.Clock.UTCTime [Keystroke]
    deriving (GHC.Generics.Generic, Flat.Flat)

data Passage = Passage { uid      :: Int
                       , name     :: String
                       , date     :: Data.Time.Clock.UTCTime
                       , text     :: String
                       , sessions :: Transient [Session]
                       }
    deriving (GHC.Generics.Generic, Flat.Flat)

-----------------------------------------------------------------------
--                            Functions                              --
-----------------------------------------------------------------------

render :: Passage -> [Keystroke] -> [(Char, Maybe Bool)]
render passage keystrokes =
    let render' []     _      = []
        render' t      []     = [(t', Nothing) | t' <- t]
        render' (t:ts) (s:ss) =
            if t == s
            then (t, Just True)  : render' ts ss
            else (t, Just False) : render' ts ss
    in render' passage.text (map fromKeystroke keystrokes)

newPassage :: [Passage] -> String -> String -> IO [Passage]
newPassage passages name' text' =
    do date' <- Data.Time.Clock.getCurrentTime
       let free_uids = Data.List.find (`notElem` (map uid passages)) [0..maxBound]
           error_msg = "No unique passage identifier possible for \"" ++ name' ++ "\"."
           uid' = case free_uids of
                      Nothing    -> error error_msg
                      Just uid'' -> uid''
           passage = Passage { uid      = uid'
                             , name     = name'
                             , date     = date'
                             , text     = text'
                             , sessions = Transient []
                             }
       return (passage:passages)

newSession :: [Passage] -> Int -> [Keystroke] -> IO [Passage]
newSession passages uid' keystrokes =
    do date' <- Data.Time.Clock.getCurrentTime
       let session = Session date' keystrokes
           compare_uid = (== uid') . uid
           error_msg = "No passage with ID " ++ show uid' ++ " to save session to."
           passages' = case break compare_uid passages of
                           (a, (b:bs)) -> a ++ ((b{sessions = fmap (++ [session]) b.sessions}):bs)
                           (_, [])     -> error error_msg
       return passages'

-----------------------------------------------------------------------
--                           Files                                   --
-----------------------------------------------------------------------

filenameExtPassage :: String
filenameExtSession :: String
filenameExtPassage = "htyp"
filenameExtSession = "data"

filenamePassage :: Passage -> System.FilePath.FilePath
filenamePassage = (System.FilePath.<.> filenameExtPassage)
                . filter Data.Char.isLetter
                . filter Data.Char.isPrint
                . name

filenameSession :: Passage -> System.FilePath.FilePath
filenameSession = (System.FilePath.<.> filenameExtSession)
                . filter Data.Char.isLetter
                . filter Data.Char.isPrint
                . name
