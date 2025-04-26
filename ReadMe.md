The Sinclair QL Bare Metal Diagnostic ROM.

Version 0.00

Copyright 2025 Stephen Usher (and any other contributers listed below).

Contributers:

License: Creative Commons, Attribution.

(Basically, you can copy and modify the code as you wish as long as you keep the list of contributers and don't try to pass it off as all your own work. In other words, "Don't be a Dick!")

This ROM replaces the main system ROM using the same EPROM adapter that you would use with a Minerva ROM.

The design is such that to boot and do something useful only needs a minimully working system. It can run the initial memory test and report back even if no RAM is in the system.

All it need to work is a working CPU, a ZX8301 which can generate the clock for the CPU, address decoding and DTACK generation and a ZX8302 which still has enough functionality to write data out of serial port 1.

In other words, even if the video side of the ZX8301 is dead you can still test the rest of the system using a terminal.

The serial conection on Ser1 runs at 4800 baud. Almost the very first thing the code does is write out a banner message, so look for that.

The ROM will then run a couple of thorough memory tests on the lower 128K of RAM. Firstly an 'own address' test and then a full March test. The combination of these should catch any bad bits or addressing errors.

Once the ROM has determined that RAM is usable it can start running more fully, first checking the IPC for functionality and then bringing up a menu of further tests. This can be accessed either via the serial port or on the main screen if the display is working.

This ROM is just another tool in the diagnostic arsnal. If it doesn't work then that probably says something about the state or the ULAs or the CPU and an oscilloscope would be your best bet at that point.

You will need the vasm assembler built for the 68000 processor with Motorola syntax enabled and make to build the ROM image.
