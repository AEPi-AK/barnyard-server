{-# Language TemplateHaskell #-}
module Data.AnimalParts where

import Database.Persist.TH
import Prelude

data HeadPart = NoHead | HeadErr | Walrus | Vulture | Bison | Toucan
    deriving(Show, Read, Eq)
derivePersistField "HeadPart"

data BodyPart = NoBody | BodyErr | Penguin | Camel | Zebra | TreeFrog
    deriving(Show, Read, Eq)
derivePersistField "BodyPart"

data LegPart = NoLeg | LegErr | PolarBear | Lizard | Emu | Lemur
    deriving(Show, Read, Eq)
derivePersistField "LegPart"

data Location = Arctic | Desert | Grassland | Rainforest
    deriving(Show, Read, Eq)
derivePersistField "Location"

locations :: [Location]
locations = [Desert, Arctic, Grassland, Rainforest]

data AnimalPart = Head HeadPart | Body BodyPart | Leg LegPart

partForId :: String -> AnimalPart
partForId s | s == "0e245" = Head Walrus
            | s == "head2" = Head Vulture
            | s == "head3" = Head Bison
            | s == "head4" = Head Toucan 
            | s == "d9fdb" = Body Penguin 
            | s == "body2" = Body Camel 
            | s == "body3" = Body Zebra
            | s == "body4" = Body TreeFrog
            | s == "09d1f" = Leg  PolarBear 
            | s == "leg2"  = Leg  Lizard 
            | s == "leg3"  = Leg  Emu
            | s == "leg4"  = Leg  Lemur
partForId _ = error "No such body part"

partError :: AnimalPart -> Bool
partError (Head HeadErr) = True
partError (Body BodyErr) = True
partError (Leg  LegErr)  = True
partError _ = False

score :: Location -> AnimalPart -> Int
score Arctic (Head Walrus)       = 3
score Arctic (Head Vulture)      = 1
score Arctic (Head Bison)        = 1
score Arctic (Head Toucan)       = 1
score Arctic (Body Penguin)      = 3
score Arctic (Body Camel)        = 1
score Arctic (Body Zebra)        = 2
score Arctic (Body TreeFrog)     = 1
score Arctic (Leg PolarBear)     = 3
score Arctic (Leg Lizard)        = 1
score Arctic (Leg Emu)           = 2
score Arctic (Leg Lemur)         = 1
score Desert (Head Walrus)       = 1
score Desert (Head Vulture)      = 3
score Desert (Head Bison)        = 1
score Desert (Head Toucan)       = 1
score Desert (Body Penguin)      = 1
score Desert (Body Camel)        = 3
score Desert (Body Zebra)        = 2
score Desert (Body TreeFrog)     = 1
score Desert (Leg PolarBear)     = 1
score Desert (Leg Lizard)        = 3
score Desert (Leg Emu)           = 2
score Desert (Leg Lemur)         = 1
score Grassland (Head Walrus)    = 1
score Grassland (Head Vulture)   = 3
score Grassland (Head Bison)     = 3
score Grassland (Head Toucan)    = 2
score Grassland (Body Penguin)   = 2
score Grassland (Body Camel)     = 2
score Grassland (Body Zebra)     = 3
score Grassland (Body TreeFrog)  = 2
score Grassland (Leg PolarBear)  = 2
score Grassland (Leg Lizard)     = 2
score Grassland (Leg Emu)        = 3
score Grassland (Leg Lemur)      = 2
score Rainforest (Head Walrus)   = 2
score Rainforest (Head Vulture)  = 1
score Rainforest (Head Bison)    = 1
score Rainforest (Head Toucan)   = 3
score Rainforest (Body Penguin)  = 1
score Rainforest (Body Camel)    = 1
score Rainforest (Body Zebra)    = 1
score Rainforest (Body TreeFrog) = 3
score Rainforest (Leg PolarBear) = 1
score Rainforest (Leg Lizard)    = 3
score Rainforest (Leg Emu)       = 1
score Rainforest (Leg Lemur)     = 3
score _  _ = 0
