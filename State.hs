module State where

import Network.Alt

data State = State { conn :: Socket
                   , dir :: String
                   }
