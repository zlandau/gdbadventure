
Introduction
------------

This mini project started when I realized that the GDB remote debugging protocol allowed the remote host complete control over what was sent back. There was no reason it had to actually be connected to a process. So why not try to make an oldschool adventure game out of it.

Usage
-----

This is from memory, so excuse me if I get some bits wrong. When the program is
executed it creates a GDB server on port 1234 (see `GDBStub.hs`). From gdb you
then connect to the server:

    (gdb) target remote localhost:1234

At this point you're in the game. So far the only operation that seems to be
implemented is printing symbols. For example, try asking for a description of
an item:

    (gdb) print lamp

Or ask for a description about the current room:

    (gdb) print here

I believe basic support for moving between rooms is there, but I don't quite
remember how that works.

See GDBStubHandlers.hs which is where the commands coming from the client
should be handled. Right now we only handle 'm' (memory) but handlers can
easily be added for the other operations.

