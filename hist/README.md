THE HISTORY DEVICE
==================

This is a backport of the HISTORY device from SMSQ/E to be used in Minerva, but can also be used by other programs.

The HISTORY device can be used as any other I/O device by OPENing a channel to it and sending strings to it using PRINT or PUT from BASIC. It's a queue-based device similar to PIPEs, but different in that it has only one end (meaning you can INPUT or GET strings from the same channel) and has a LIFO (last-in-first-out) policy rather than FIFO. It's used by Minerva's BASIC command line prompt to implement recall of commands entered earlier, by browsing through the command history using the arrow up/down keys, similar to SMSQ/E and other modern operating systems. It can also be used by other programs, either from BASIC or machine code. A full description can be found in QPC's Concepts manual.

In this implementation, some code has been added to allow the HISTORY device to be used by Minerva. On loading the code (either from the Q68 extension ROM or by LRESPRing hist_rext from a BOOT file), it checks if Minerva's BASIC is already using the HISTORY device. If not, it opens a HISTORY channel and installs it into the BASIC system. If any MultiBASIC interpreters are subsequently started, they will open their own HISTORY channel - thus it is not necessary (and not even possible!) to load another copy of history_rext in their BOOT file.

Note that, in order to use the HISTORY feature in the standalone (non-Q68) version of Minerva, you need to replace your Minerva ROM image by the specially built version 1.98j1 image. This has additional code to use the HISTORY device but is otherwise identical to existing 1.98 builds; however the I2C driver has been removed to make room for it.

Building the HISTORY device
---------------------------

In the **hist** subdirectory, there should be at least the following files:

- **rext** - the binary assembled from SMSQ/E, (see below on how to assemble it)
- **init.asm** - the initialisation code to make it work with Minerva. This merges the above-mentioned **rext** file as a binary blob
- **link** - the linker file. This assembles the **init.asm** file and builds the **hist_bin** complete binary

As with building Minerva, you should at least have the following programs (download from https://dilwyn.theqlforum.com/asm/index.html if necessary):
- the QMAC Assembler from Quanta
- the QLINK Linker from Quanta *or* the QJUMP linker included in the SMSQ/E distribution
- the QMAKE program, suitable configured with locations of the assembler and linker programs

To build a complete version of the HISTORY device for Minerva, use the following steps:

1. Obtain the source distribution of the latest SMSQ/E version from https://www.wlenerz.com/smsqe/ (preferrably the QXL.WIN container version, which can be easily accessed from emulators)
2. Mount this as a device, e.g. WIN7_. Then enter DEV_USE 8,WIN7_ and DATA_USE WIN7_iod_history_
3. Make sure that you have properly configured the path to your assembler and linker programs in QMAKE.
4. Execute QMAKE, specifying DEV8_iod_history_link as the linker command file and build the history_rext file
5. Copy the binary built in step 4: COPY dev8_iod_history_rext TO dos7_hist_rext (assuming dos7_ points to the Min4Q68 source tree)
6. Build Minerva + extrarom using the MAKE_BAS program from the Min4Q68 source tree
7. The HISTORY device is now incorporated in the extrarom image. The binary for use with standalone Minerva (loadable from a BOOT file) is available as dos7_hist_bin (i.e. NOT dos7_hist_rext which contains only the HISTORY device itself!)

Jan Bredenbeek, August 2025.