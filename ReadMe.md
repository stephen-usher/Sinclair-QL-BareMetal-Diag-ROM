The Sinclair QL Bare Metal Diagnostic ROM.

Version 0.00

Copyright 2025 Stephen Usher (and any other contributers listed below).

Contributers:

License: Creative Commons, Attribution.

(Basically, you can copy and modify the code as you wish as long as you keep the list of contributers and don't try to pass it off as all your own work. In other words, "Don't be a Dick!")

This ROM replaces the main system ROM using the same EPROM adapter that you would use with a Minerva ROM.

The design is such that to boot and do something useful only needs a minimully working system. It can run the initial memory test and report back even if no RAM is in the system.

All it need to work is a working CPU, a ZX8301 which can generate the clock for the CPU, address decoding and DTACK generation and a ZX8302 which still has enough functionality to write data out of serial port 2.

In other words, even if the video side of the ZX8301 is dead you can still test the rest of the system using a terminal.

The serial conection on ser2 runs at 4800 baud. Almost the very first thing the code does is write out a banner message, so look for that.

The ROM will then run a couple of thorough memory tests on the lower 128K of RAM. Firstly an 'own address' test and then a full March test. The combination of these should catch any bad bits or addressing errors.

If there is a lower RAM error it will print the error address and bad bits on the serial port and put a series of bands across the screen. The first is white or black showing which memory bank the error occured within, white lower, black higher. This is followed by 8 bands for the 8 bits, 7 -> 0. A green band is good and a red band bad.

Once the base 128K RAM is known to work the ROM will do some initial tests such as reading the clock and checking out the IPC functionality, including reading the keyboard.

The plan is that once these initial tests are complete that the ROM will being up a menu to allow interactive tests to be performed.

This ROM is just another tool in the diagnostic arsnal. If it doesn't work then that probably says something about the state of the ULAs or the CPU and an oscilloscope would be your best bet at that point.

You will need the vasm assembler built for the 68000 processor with Motorola syntax enabled and make to build the ROM image.
