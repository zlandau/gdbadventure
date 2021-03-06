module GDBStub where

import Char
import Maybe
import Numeric
import Network.Alt
import Control.Exception as E
import Symbols as Symbols
import State
import Utils
import Room

port = "1234"

{-
   Keep accepting connections.  Currently we only support one
   connection at a time
-}
server port = nullStreamServer { handlerSS = handler, servSS = port }
server_loop = do
          [(st,sa)] <- getAddrInfo "" port aiPassive $
                                SocketType afInet sockStream 0
          s <- socket st
          bind s sa
          listen s 1
          acceptLoop s `E.catch` (\e -> close s)

acceptLoop sock = do (csock, sa) <- accept sock
                     handler (return $ State csock "u" room1) `E.catch` (\e -> print e >> close csock)
                     acceptLoop sock

getChecksum :: Socket -> IO Int
getChecksum sock = do str <- recvString sock 2
                      case readHex str of
                        [(i,_)] -> return i
                        _       -> fail "Invalid checksum data"

calculateChecksum :: [Char] -> Int
calculateChecksum pkt = mod (foldl (+) 0 $ map Char.ord pkt) 256

verifyPacket :: [Char] -> Int -> Bool
verifyPacket pkt chk = (calculateChecksum pkt) == chk

getPacket :: Socket -> IO (String, Int)
getPacket s = do c <- recvString s 1
                 str <- loop [] c
                 checksum <- getChecksum s
                 return (str, checksum)
    where loop acc "#" = return $ reverse acc
          loop acc "$" = recvString s 1 >>= loop acc
          loop acc [ch] = recvString s 1 >>= loop (ch:acc)
          loop acc "" = return acc -- how should we actually handle this?

handleCommand :: State -> String -> IO State
handleCommand state ('m':pktData) = do
        sendSuccess (conn state)
        sendResponse (conn state) response
        if isDescription address then doSymbol state $ symbolById address
            else return state
        where response = memoryRequest state request
              request = MemoryRequest address len ""
              address = readAddress pktData
              len = readLength pktData

handleCommand state ('M':pktData) = do
        sendSuccess (conn state)
        sendResponse (conn state) "OK"
        putStrLn $ "setting " ++ (show address) ++ " " ++ (show bytes)
        putStrLn $ "dir is " ++ (dir state)
        rstate
        --putStrLn $ (description symbol)
        where rstate = memorySet state symbol request
              request = MemoryRequest address len bytes
              symbol = symbolById address
              address = readAddress pktData
              len = readLength pktData
              bytes = readBytes pktData

handleCommand state ('?':_) = do
        sendSuccess (conn state)
        sendResponse (conn state) "T0505:00000000;04:a057c7bf;08:c017f6b7;"
        return state

handleCommand state _ = sendSuccess (conn state) >> sendResponse (conn state) "" >> return state

readAddress :: String -> Address
readAddress pktData = case readHex addressStr of
                           [(i,_)] -> i
                           _       -> 0
            where addressStr = takeWhile (/=',') pktData

readLength :: String -> Int
readLength pktData = case readHex lenStr of
                          [(i,_)] -> i
                          _       -> 0
           where lenStr = drop 1 $ dropWhile (/=',') pktData

readBytes :: String -> String
readBytes pktData = drop 1 $ dropWhile (/=':') pktData

partialDesc :: Address -> Int -> String
partialDesc addr len = partialStr desc offset len
        where desc = description symbol
              symbol = symbolById addr
              offset = idDescOffset addr


sendFailure sock = sendString sock "-"
sendSuccess sock = sendString sock "+"

sendResponse sock pkt = sendString sock pktData
                        where pktData = "$" ++ pkt ++ "#" ++ chk
                              chk = pad2 hexSum
                              hexSum = showHex (calculateChecksum pkt) ""

-- Is this how RLE should be handled?  Do we need to handle it?
--decodeRLE (x:'*':z:xs) = (take num $ repeat x) ++ decodeRLE xs
--                         where num = Char.digitToInt z
--decodeRLE (x:xs) = x : decodeRLE xs
--decodeRLE x = x

processPacket :: State -> String -> Int -> IO State
processPacket state pkt chk = if verifyPacket pkt chk
                               then handleCommand state pkt
                               else do sendFailure (conn state)
                                       return state

verifyResponse sock = do r <- recvString sock 1
                         if r == "+"
                           then return ()
                           else fail "Server gave error response"

handler state = do st <- state
                   verifyResponse $ conn st
                   (pkt,chk) <- getPacket $ conn st
                   handler $ processPacket st pkt chk

