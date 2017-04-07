{-# Language TemplateHaskell #-}
module Data.AnimalParts where

import Database.Persist.TH
import Prelude

data HeadPart = NoHead | HeadErr | Head1 | Head2
    deriving(Show, Read, Eq)
derivePersistField "HeadPart"

data BodyPart = NoBody | BodyErr | Body1 | Body2 
    deriving(Show, Read, Eq)
derivePersistField "BodyPart"

data LegPart = NoLeg | LegErr | Leg1 | Leg2
    deriving(Show, Read, Eq)
derivePersistField "LegPart"

data AnimalPart = Head HeadPart | Body BodyPart | Leg LegPart

partForId :: String -> AnimalPart
partForId s | s == "head1" = Head Head1
            | s == "head2" = Head Head2
            | s == "body1" = Body Body1
            | s == "body2" = Body Body2
            | s == "leg1"  = Leg  Leg1
            | s == "leg2"  = Leg  Leg2
partForId _ = error "No such body part"
