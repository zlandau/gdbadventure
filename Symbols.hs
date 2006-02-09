module Symbols where

data Item = Item { identifier :: String
                 , description :: String
                 }

items :: [Item]
items = [here, lamp]

type Address = Integer

here = Item { identifier = "here", description = "a room!" }
lamp = Item { identifier = "lamp", description = "a lamp!" }
unknown = Item { identifier = "", description = "what do you mean?" }

identifier_base :: Address
identifier_base = 0x80495bc

-- Get the description address for a given identifier
idDescAddress :: Address -> Address
idDescAddress addr = itemPos * 1024
    where itemPos = (addr - identifier_base) `div` 4

-- Get offset from beginning of description
idDescOffset :: Address -> Int
idDescOffset addr = fromInteger $ addr - (addr - start)
    where start = addr `mod` 1024

-- Get an item from its ID
itemById :: Address -> Item
itemById addr = items !! (offset `div` 4)
    where offset = fromInteger $ (addr - identifier_base)

-- XXX: Provide a real mechanism for determining range
isIdentifier :: Address -> Bool
isIdentifier x = (x < identifier_base) && (x < 100)

isDescription :: Address -> Bool
isDescription x = (x >= identifier_base) && (x < (identifier_base + 5000))
