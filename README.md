# SIMPLE-6502-IDE
SIMPLE INTERFACING AN IDE (PATA) DRIVE TO A 6502 CPU

This is the IDE part of an on-going 6502 SBC development.
The IDE part works very well so I thought I would share it.
IDE (PATA) is very simple to impliment as it was designed to interface directly to a CPU bus.
There are two problems when using PATA with an 8-bit CPU:-

1) PATA, by default, is a 16-bit interface so bi-directional latches are required to 'hold' the upper and lower bytes of the data. This greatly increases the complexity of the design.
2)  PATA can be forced into 8-bit mode, but not all devices (drives, Compact Flash, etc) support this so it is not to be depended on.

The simple solution is to ignore the upper byte!

Yes, this is wasteful as it results in only half the storage being used but, so what!

Once you accept this, everything becomes simple.

The PATA interface is well documented, this is my implimentation:-

![Screenshot at 2023-12-06 14-23-08](https://github.com/gcswales/SIMPLE-6502-IDE/assets/50659826/c3b09249-cead-4429-bcd7-a580b498c122)

/IDE_CS - This is provided by my address decode and maps the IDE register to memory.

D0 - D7 - Data lines from the bus.

A0 - A2 - These are the CPU address lines that enable addressing of individual PATA registers.

/RESET - This is tied to the system reset.

/IDE_BUSY - They even provide a busy line which can be used for a 'drive busy' LED.

/WR - This is active low write (see below).

/RD - This is active low read (see below).

![Screenshot at 2023-12-06 14-24-13](https://github.com/gcswales/SIMPLE-6502-IDE/assets/50659826/84b8b11d-b60b-46b1-bcf0-da9986816224)

I always use the above circuit to gate the read & write from a 6502. It prevents spurious writes and provides better compatability with peripherals.

The code to interface to the PATA is equally simple and is in the code section. It is in the form of a BIOS file which works with my minimal operating system... watch this space!

Enjoy!

Geoffrey.
