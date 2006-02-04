module GDBStub where

import Char
import Numeric
import Network.Alt
import Control.Exception as E
import Foreign

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
                     handler csock `E.catch` (\e -> print e >> close csock)
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

handleCommand sock _ = sendSuccess sock >> sendResponse sock ""

sendFailure sock = sendString sock "-"
sendSuccess sock = sendString sock "+"

pad val padValue i = padding ++ val
                     where padding = take len $ repeat padValue
                           len = i - (length val)

sendResponse sock pkt = sendString sock pktData
                        where pktData = "$" ++ pkt ++ "#" ++ chk
                              chk = pad hexSum '0' 2
                              hexSum = showHex (calculateChecksum pkt) ""

-- Is this how RLE should be handled?  Do we need to handle it?
--decodeRLE (x:'*':z:xs) = (take num $ repeat x) ++ decodeRLE xs
--                         where num = Char.digitToInt z
--decodeRLE (x:xs) = x : decodeRLE xs
--decodeRLE x = x

processPacket sock pkt chk = if verifyPacket pkt chk
                             then handleCommand sock pkt
                             else sendFailure sock

handler sock = do s <- recvString sock 1
                  (pkt,chk) <- getPacket sock
                  putStrLn $ show $ calculateChecksum pkt
                  processPacket sock pkt chk
                  handler sock

