module State where

import Network.Alt
import Room

data State = State { conn :: Socket
                   , dir :: String
                   , room :: Room
                   }
