{-# Language TemplateHaskell #-}
module Data.AnimalParts where

import Database.Persist.TH
import Prelude

data HeadPart = NoHead | Head1 | Head2
    deriving(Show, Read, Eq)
derivePersistField "HeadPart"

data BodyPart = NoBody | Body1 | Body2 
    deriving(Show, Read, Eq)
derivePersistField "BodyPart"

data LegPart = NoLeg | Leg1 | Leg2
    deriving(Show, Read, Eq)
derivePersistField "LegPart"
