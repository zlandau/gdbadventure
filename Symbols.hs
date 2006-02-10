module Symbols where

import Utils

--class Symbol a where
--    get_desc :: a -> String

data Symbol = Item { description :: String }
            | Action { act_handler :: IO () }


symbols :: [Symbol]
symbols = [dummy, here, lamp, direction]

type Address = Integer

dummy = Item { description = "dummy var" }
here = Item { description = "a room!" }
lamp = Item { description = "a lamp!" }
direction = Action { act_handler = putStrLn "hi im a direction"}
unknown = Item { description = "what do you mean?" }

identifier_base :: Address
identifier_base = 0x80495d4

description_size = 1024 :: Integer

-- Get the symbol address for a given identifier
idDescAddress :: Address -> Address
idDescAddress addr = itemPos * description_size
    where itemPos = (addr - identifier_base) `div` 4

-- Get offset from beginning of symbol
idDescOffset :: Address -> Int
idDescOffset addr = fromInteger $ addr - (addr - start)
    where start = addr `mod` description_size

-- Get a symbol from its ID
symbolById :: Address -> Symbol
symbolById addr = symbol
    where offset = fromInteger $ addr
          symbolPos = offset `div` 1024
          symbol = if symbolPos >= length symbols then unknown
                   else symbols !! symbolPos

--doSymbol :: Symbol -> IO ()
doSymbol (Item d) = putStrLn "an item"
doSymbol (Action a) = putStrLn "an action"

memoryGet :: Symbol -> Address -> Int -> String
memoryGet (Item desc) addr len = strToHexStr $ nullStr $ partialStr desc offset len
    where offset = idDescOffset addr
memoryGet (Action a) addr len = "E11"

memorySet :: Symbol -> Address -> Int -> Int -> String
memorySet (Item desc) addr len bytes = "E01"
memorySet (Action a) addr len bytes = "OK"

isIdentifier :: Address -> Bool
isIdentifier x = (x >= identifier_base) && (x < identifier_space)
    where identifier_space = identifier_base + ((numIdentifiers+1) * 4)

isDescription :: Address -> Bool
isDescription x = (x < identifier_base) && (x < description_space)
    where description_space = (numIdentifiers+1) * 1024

numIdentifiers :: Integer
numIdentifiers = toInteger $ (length symbols) + 2
