module Main where

import Passage
import Interface
import Themes

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V

-----------------------------------------------------------------------
--                             Example                               --
-----------------------------------------------------------------------

exampleText :: String
exampleText = "Gallia est omnis divisa in partes tres, quarum \
              \unam incolunt Belgae, aliam Aquitani, tertiam qui \
              \ipsorum lingua Celtae, nostra Galli appellantur.\n\
              \\
              \Hi omnes lingua, institutis, legibus inter se \
              \differunt. Gallos ab Aquitanis Garumna flumen, a \
              \Belgis Matrona et Sequana dividit.\n\
              \\
              \Horum omnium fortissimi sunt Belgae, propterea \
              \quod a cultu atque humanitate provinciae \
              \longissime absunt, minimeque ad eos mercatores \
              \saepe commeant atque ea quae ad effeminandos \
              \animos pertinent important,\n\
              \\
              \proximique sunt Germanis, qui trans Rhenum \
              \incolunt, quibuscum continenter bellum gerunt. Qua \
              \de causa Helvetii quoque reliquos Gallos virtute \
              \praecedunt, quod fere cotidianis proeliis cum \
              \Germanis contendunt, cum aut suis finibus eos \
              \prohibent aut ipsi in eorum finibus bellum gerunt.\n\
              \\
              \Eorum una, pars, quam Gallos obtinere dictum est, \
              \initium capit a flumine Rhodano, continetur \
              \Garumna flumine, Oceano, finibus Belgarum, attingit \
              \etiam ab Sequanis et Helvetiis flumen Rhenum, \
              \vergit ad septentriones."

-----------------------------------------------------------------------
--                            App Data                               --
-----------------------------------------------------------------------

app :: App InterfaceSession () ()
app = App { appDraw         = draw
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = input
          , appStartEvent   = return
          , appAttrMap      = const themes
          }

main :: IO InterfaceSession
main =
    do let td = InterfaceSession [] Nothing []
       td'  <- newPassage td  "Example Passage" exampleText
       defaultMain app td'
