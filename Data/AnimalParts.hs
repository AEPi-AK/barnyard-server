{-# Language TemplateHaskell #-}
module Data.AnimalParts where

import Database.Persist.TH
import Prelude

data HeadPart = Head1 | Head2
    deriving(Show, Read, Eq)
derivePersistField "HeadPart"

data BodyPart = Body1 | Body2 
    deriving(Show, Read, Eq)
derivePersistField "BodyPart"

data LegPart = Leg1 | Leg2
    deriving(Show, Read, Eq)
derivePersistField "LegPart"
