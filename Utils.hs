module Utils where

import Numeric
import Char

nullStr :: String -> String
nullStr s = s ++ "\0" 

pad :: Char -> Int -> String -> String
pad padValue i val = padding ++ val
    where padding = take len $ repeat padValue
          len = i - (length val)

pad2 = pad '0' 2
pad8 = pad '0' 8

toAddress addr = reverseBytes $ pad8 $ intToHexStr $ fromInteger addr

reverseBytes :: String -> String
reverseBytes (a:b:c:d:xs) = c:d:a:b:reverseBytes xs
reverseBytes _ = ""

intToHexStr :: Int -> String
intToHexStr x = Numeric.showHex x []

strToHexStr :: String -> String
strToHexStr s = concatMap (pad2 . intToHexStr . ord) s

partialStr :: String -> Int -> Int -> String
partialStr s start len = take len $ drop start s
