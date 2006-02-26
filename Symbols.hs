module Symbols where

import Utils
import State

--class Symbol a where
--    get_desc :: a -> String

data Symbol = Item { description :: String }
            | Action { act_handler :: IO () }

type Address = Integer

data MemoryRequest = MemoryRequest { address :: Address
                                   , request_len :: Int
                                   , request_data :: String
                                   }

            
symbols :: [Symbol]
symbols = [dummy, here, lamp, direction]


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

doSymbol :: State -> Symbol -> IO State
doSymbol state (Item d) = putStrLn "an item" >> return state
doSymbol state (Action a) = putStrLn "an action" >> return state

memoryGet :: Symbol -> MemoryRequest -> String
memoryGet (Item desc) (MemoryRequest addr len _)
                = strToHexStr $ nullStr $ partialStr desc offset len
    where offset = idDescOffset addr
memoryGet (Action a) (MemoryRequest _ _ _) = "E11"

memorySet :: Symbol -> MemoryRequest -> String
memorySet (Item desc) (MemoryRequest addr len bytes) = "E01"
memorySet (Action a) (MemoryRequest addr len bytes) = "OK"

memoryRequest :: MemoryRequest -> String
memoryRequest mr@(MemoryRequest addr len _)
              | isIdentifier addr = toAddress $ idDescAddress addr
              | isDescription addr = memoryGet symbol mr
    where symbol = symbolById addr
memoryRequest (MemoryRequest _ len _) = pad '0' (len-1) "0"

isIdentifier :: Address -> Bool
isIdentifier x = (x >= identifier_base) && (x < identifier_space)
    where identifier_space = identifier_base + ((numIdentifiers+1) * 4)

isDescription :: Address -> Bool
isDescription x = (x < identifier_base) && (x < description_space)
    where description_space = (numIdentifiers+1) * 1024

numIdentifiers :: Integer
numIdentifiers = toInteger $ (length symbols) + 2
