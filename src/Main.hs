module Main where

import Passage
import Interface
import Themes
import Data.Focus

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

-----------------------------------------------------------------------
--                             Example                               --
-----------------------------------------------------------------------

deBelloGallico :: String
deBelloGallico =
      "Gallia est omnis divisa in partes tres, quarum unam incolunt \
      \Belgae, aliam Aquitani, tertiam qui ipsorum lingua Celtae, \
      \nostra Galli appellantur.\
      \\n\
      \Hi omnes lingua, institutis, legibus inter se differunt. \
      \Gallos ab Aquitanis Garumna flumen, a Belgis Matrona et \
      \Sequana dividit.\
      \\n\
      \Horum omnium fortissimi sunt Belgae, propterea quod a cultu \
      \atque humanitate provinciae longissime absunt, minimeque ad \
      \eos mercatores saepe commeant atque ea quae ad effeminandos \
      \animos pertinent important,\
      \\n\
      \proximique sunt Germanis, qui trans Rhenum incolunt, quibuscum \
      \continenter bellum gerunt. Qua de causa Helvetii quoque \
      \reliquos Gallos virtute praecedunt, quod fere cotidianis \
      \proeliis cum Germanis contendunt, cum aut suis finibus eos \
      \prohibent aut ipsi in eorum finibus bellum gerunt.\
      \\n\
      \Eorum una, pars, quam Gallos obtinere dictum est, initium \
      \capit a flumine Rhodano, continetur Garumna flumine, Oceano, \
      \finibus Belgarum, attingit etiam ab Sequanis et Helvetiis \
      \flumen Rhenum, vergit ad septentriones."

prideAndPrejudice :: String
prideAndPrejudice =
      "It is a truth universally acknowledged, that a single man in \
      \possession of a good fortune, must be in want of a wife.\
      \\n\
      \However little known the feelings or views of such a man may \
      \be on his first entering a neighbourhood, this truth is so \
      \well fixed in the minds of the surrounding families, that he \
      \is considered the rightful property of some one or other of \
      \their daughters.\
      \\n\
      \\"My dear Mr. Bennet,\" said his lady to him one day, \"have you \
      \heard that Netherfield Park is let at last?”\
      \\n\
      \Mr. Bennet replied that he had not.\
      \\n\
      \\"But it is,\" returned she; \"for Mrs. Long has just been here, \
      \and she told me all about it.\"\
      \\n\
      \Mr. Bennet made no answer.\
      \\n\
      \\"Do you not want to know who has taken it?\" cried his wife \
      \impatiently.\
      \\n\
      \\"You want to tell me, and I have no objection to hearing it.\"\
      \\n\
      \This was invitation enough.\
      \\n\
      \\"Why, my dear, you must know, Mrs. Long says that Netherfield \
      \is taken by a young man of large fortune from the north of \
      \England; that he came down on Monday in a chaise and four to \
      \see the place, and was so much delighted with it, that he \
      \agreed with Mr. Morris immediately; that he is to take \
      \possession before Michaelmas, and some of his servants are to \
      \be in the house by the end of next week.\"\
      \\n\
      \\"What is his name?\"\
      \\n\
      \\"Bingley.\"\
      \\n\
      \\"Is he married or single?\"\
      \\n\
      \\"Oh! Single, my dear, to be sure! A single man of large \
      \fortune; four or five thousand a year. What a fine thing for \
      \our girls!\"\
      \\n\
      \\"How so? How can it affect them?\"\
      \\n\
      \\"My dear Mr. Bennet,\" replied his wife, \"how can you be so \
      \tiresome! You must know that I am thinking of his marrying \
      \one of them.\""

-----------------------------------------------------------------------
--                            App Data                               --
-----------------------------------------------------------------------

app :: App Interface () ()
app = App { appDraw         = draw'
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = input
          , appStartEvent   = return
          , appAttrMap      = const themes
          }

draw' :: Interface -> [Widget ()]
draw' = pure
      . withBorderStyle unicode
      . borderWithLabel (str "Haskell Typist")
      . center
      . draw

main :: IO Interface
main =
    do passages' <- (newPassage "Commentarii de Bello Gallico" deBelloGallico []
                 >>= newPassage "Pride and Prejudice"          prideAndPrejudice
                 >>= newPassage "Example 1"                    "I'm just a short example!"
                 >>= newPassage "Example 2"                    "I'm another short example!"
                 >>= newPassage "Example 3"                    "I'm the third short example!"
                 >>= newPassage "Example 4"                    "I'm number four!"
                 >>= newPassage "Example 5"                    "Well, I'm number 5."
                 >>= newPassage "Example 6"                    "Wait, what number are we at!"
                 >>= newPassage "Example 7"                    "Twelve...?"
                 >>= newPassage "Example 8"                    "I'm number 13!"
                 >>= newPassage "Example 9"                    "*quiet*"
                 >>= newPassage "Example 10"                   "Let's start from the beginning.")
       defaultMain app (IPassage (Just (focusList passages')))
