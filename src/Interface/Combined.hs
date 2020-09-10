module Interface.Combined where

import Interface

import qualified Interface.Passage as IPassage
import qualified Interface.Session as ISession

data ICombined = IPassage IPassage.IPassage
               | ISession ISession.ISession

instance Interface ICombined where
    input (IPassage interface) k = (fmap . fmap) IPassage (input interface k)
    input (ISession interface) k = (fmap . fmap) ISession (input interface k)
    draw (IPassage interface) = draw interface
    draw (ISession interface) = draw interface
