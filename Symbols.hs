module Symbols where

import Utils
import State
import Room

--class Symbol a where
--    get_desc :: a -> String

data Symbol = Item { description :: String }
            | Action { act_handler :: IO (),
                       read_memory :: State -> String }

type Address = Integer

data MemoryRequest = MemoryRequest { address :: Address
                                   , request_len :: Int
                                   , request_data :: String
                                   }

            
symbols :: [Symbol]
symbols = [dummy, here, lamp, direction]


dummy = Item { description = "dummy var" }
here = Action { act_handler = putStrLn "some action",
                read_memory = \state -> (room_desc $ room state) }
lamp = Item { description = "a lamp!" }
direction = Action { act_handler = putStrLn "hi im a direction",
                     read_memory = \state -> "hi" }
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

-- Fulfill a partial string request
getGDBString :: String -> Address -> Int -> String
getGDBString str addr len = strToHexStr $ nullStr $ partialStr str offset len
    where offset = idDescOffset addr

doSymbol :: State -> Symbol -> IO State
doSymbol state (Item d) = putStrLn "an item" >> return state
doSymbol state (Action a r) = putStrLn "an action" >> return state

memoryGet :: State -> Symbol -> MemoryRequest -> String
memoryGet state (Item desc) (MemoryRequest addr len _)
                = getGDBString desc addr len
memoryGet state (Action a r) (MemoryRequest addr len _)
                = getGDBString (r state) addr len
           

memorySet :: State -> Symbol -> MemoryRequest -> IO State
memorySet state (Item desc) (MemoryRequest addr len bytes) = return state
memorySet state (Action a r) (MemoryRequest addr len bytes) = do
        return state { room = (next_room r) bytes }
    where r = room state

memoryRequest :: State -> MemoryRequest -> String
memoryRequest state mr@(MemoryRequest addr len _)
              | isIdentifier addr = toAddress $ idDescAddress addr
              | isDescription addr = memoryGet state symbol mr
    where symbol = symbolById addr
memoryRequest _ (MemoryRequest _ len _) = pad '0' (len-1) "0"

isIdentifier :: Address -> Bool
isIdentifier x = (x >= identifier_base) && (x < identifier_space)
    where identifier_space = identifier_base + ((numIdentifiers+1) * 4)

isDescription :: Address -> Bool
isDescription x = (x < identifier_base) && (x < description_space)
    where description_space = (numIdentifiers+1) * 1024

numIdentifiers :: Integer
numIdentifiers = toInteger $ (length symbols) + 2
