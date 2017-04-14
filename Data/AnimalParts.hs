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

data Location = Desert | Tundra
    deriving(Show, Read, Eq)
derivePersistField "Location"

locations :: [Location]
locations = [Desert, Tundra]

data AnimalPart = Head HeadPart | Body BodyPart | Leg LegPart

partForId :: String -> AnimalPart
partForId s | s == "0e245" = Head Head1
            | s == "head2" = Head Head2
            | s == "d9fdb" = Body Body1
            | s == "body2" = Body Body2
            | s == "09d1f" = Leg  Leg1
            | s == "leg2"  = Leg  Leg2
partForId _ = error "No such body part"

partError :: AnimalPart -> Bool
partError (Head HeadErr) = True
partError (Body BodyErr) = True
partError (Leg  LegErr)  = True
partError _ = False
