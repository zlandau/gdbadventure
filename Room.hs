module Room where

data Room = Room { room_desc :: String,
                   next_room :: String -> Room }

room1Dir "03" = room2
room1Dir _ = room1

room1 = Room { room_desc = "starting room",
               next_room = room1Dir }

room2Dir "01" = room1
room2Dir _ = room2

room2 = Room { room_desc = "another room",
               next_room = room2Dir }
               
