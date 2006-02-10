module Symbols where

data Item = Item { identifier :: String
                 , description :: String
                 }

items :: [Item]
items = [dummy, here, lamp, direction]

type Address = Integer

dummy = Item { identifier = "dummy", description = "dummy var" }
here = Item { identifier = "here", description = "a room!" }
lamp = Item { identifier = "lamp", description = "a lamp!" }
direction = Item { identifier = "direction", description = "up" }
unknown = Item { identifier = "", description = "what do you mean?" }

identifier_base :: Address
identifier_base = 0x80495d0

description_size = 1024 :: Integer

-- Get the description address for a given identifier
idDescAddress :: Address -> Address
idDescAddress addr = itemPos * description_size
    where itemPos = (addr - identifier_base) `div` 4

-- Get offset from beginning of description
idDescOffset :: Address -> Int
idDescOffset addr = fromInteger $ addr - (addr - start)
    where start = addr `mod` description_size

-- Get an item from its ID
itemById :: Address -> Item
itemById addr = item
    where offset = fromInteger $ addr --(addr - identifier_base)
          itemPos = offset `div` 1024
          item = if itemPos >= length items then unknown
                   else items !! itemPos

isIdentifier :: Address -> Bool
isIdentifier x = (x >= identifier_base) && (x < identifier_space)
    where identifier_space = identifier_base + ((numIdentifiers+1) * 4)

isDescription :: Address -> Bool
isDescription x = (x < identifier_base) && (x < description_space)
    where description_space = (numIdentifiers+1) * 1024

numIdentifiers :: Integer
numIdentifiers = toInteger $ (length items) + 2
