{- The handler implementations should go in this file.  Any
   non-implemented commands will cause us to send gdb a message indicating
   that we do not handle the command.  Note that gdb requires
   implementing the following commands: 'g', 'G', 'm', 'M', 'c', 's'
-}

module GDBStubHandlers where

import GDBStub

handleCommand :: Socket -> [Char] -> IO ()
        handleCommand sock ('m':pktData) = do sendSuccess sock
        sendResponse sock "89e0e8b92c000089"
        where address = take 8 pktData
        length = drop 9 pktData

