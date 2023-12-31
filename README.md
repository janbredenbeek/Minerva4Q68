Minerva4Q68
===========

Description
-----------

A port of the Minerva operating system for the Q68 (Sinclair QL clone).

The Minerva operating system was originally designed as a replacement ROM operating system for the Sinclair QL computer, currently licenced under GPLv3. This port is aimed at the Q68, an FPGA-based replacement board for the QL. It is not intended as a replacement for the SMSQ/E OS supplied with the Q68, as SMSQ/E is far more extensive and better suited to support the Q68 hardware than the 48K ROM-based Minerva. We just provide this port to demonstrate the Q68's ability to run 'oldskool' ROM images, give Q68 users the Minerva look and feel, and maybe provide an opportunity to run badly written software that doesn't run on SMSQ/E (but chances are big that this software won't run on Minerva either).

The current Minerva build is based on v1.98, with a few modifications to run successfully on the Q68.

BUILDING:
---------

The complete ROM image and drivers are ready-built, see INSTALLATION below for instructions how to install it. However, if you want to rebuild Minerva and/or the extrarom drivers, you will need the following:

- a QDOS or (preferrably) SMSQ/E system on suitable hardware. A bare QL with 128K RAM and floppies won't do, you will need several megabytes of storage to assemble and link the system together. The Q68 itself has enough storage but you will need some patience. Use QPC2 or another emulator if you want more speed.
- The QMAC and QLINK macro-assembler and linker. You can download them from https://dilwyn.qlforum.co.uk/asm/index.html. These are enhanced versions of the GST Macro Assembler and linker, now freeware for non-commercial purposes.
- The QMake program, also available from the download link mentioned above. This program requires the Pointer Environment (already included in SMSQ/E) and the QMenu extension (https://dilwyn.qlforum.co.uk/tk/qmenu805.zip) installed.
- The Make.bas SBASIC program in this repository for easy building.

You might need to do some configuration on the QMAC, QLINK, and QMake programs using the menuconfig program to get the device and directory path right. The original build used 'win1_' as base device for the whole repository, which was hardcoded in all .asm and cct files. I have stripped the base device from all 'include' directives in the .asm files, which should simplify building on any platform provided that you set the default data directory (DATA_USE statement) correctly.

Unfortunately, the cct files in each subdirectory of the M_ directory which contain the names of the individual files to be assembled and concatenated to 'lib' files must contain the full path to all '_rel' files, including the base device. Currently, this base device is set to 'dos7_' as I use QPC2 for building on a Windows file system. If you want to build on a different device, you will have to change the device name in all cct files, or rename your device to 'dos'.

Note that it is not recommended to use a QDOS/SMSQE subdirectory as base directory, as some utilities (notably make_clean) might fail when traversing the subdirectories. It is best to either point the DOS7_ device to the base (Windows) directory of the repository, or use a separate virtual WIN drive for the repository so you can use 'WINx_' as base directory.

Using the Make.bas program
--------------------------

LRUN the program first (this will set some variables right and display the usage screen). The program contains some procedures which can be invoked separately to build parts of the package, or simply enter 'Make' to build the complete package.

- make_minerva: This builds the 48K Minerva ROM (no drivers ROM)
- make_extrarom: This builds the drivers ROM
- make_rom: This builds the complete system (Minerva, drivers ROM and any additional extension ROMs named xc000_rom or x14000_rom) and places the result as Q68_ROM.SYS in the base directory. 
- make_lrespr: As make_rom, but generates a Min4Q68_rext file which can be loaded into the Q68 using the LRESPR command.
- make: build all of the above
- make_clean: Cleanup all generated '_REL' and 'LIB' files (not the Q68_ROM.SYS or Min4Q68_rext file). It creates a log file in ram1_make_log.

The mincf configuration file
----------------------------

This text file in the M_ subdirectory is used to configure certain hardware features of Minerva while building. It's currently configured for the Q68, further details are not ducumented yet so it's best to leave it alone...

INSTALLATION:
-------------

The Q68_ROM.SYS file should copied to the root directory of a FAT32-formatted SDHC card. The Q68 will then load this image and boot into the Minerva operating system.

The 80K ROM images contain the Minerva operating system, a keyboard driver for US, UK and DE (German) keyboard layouts, and a SDHC card driver. Note that in the current build the MDV driver is still present but disabled since there is no MDV hardware in the Q68.

The keyboard language may be set using the KBTABLE command, which has the telephone country code as parameter. Currently, the supported codes are US (1), UK (44), and German (49). The default is US; you may change this by editing the userdef file in the extrarom directory and rebuilding it.

By default, the devices win1_ and win2_ will be mapped to container files QLWA.WIN on SDHC drives 1 and 2 respectively. If present, the devices qub1_ and qub2_ will be mapped to Qubide container files QL_BDI.BIN on SDHC drives 1 and 2 respectively. This can be changed by configuring the Q68_ROM.SYS file (see below).

By combining the keyboard and SD-card driver, the size of the ROM image has been reduced from 96K to 80K. The remaining 16K may be used to add another 16K extension ROM image, e.g. Toolkit II. This additional extension ROM will be placed at location $14000, after the Q68_ROM.SYS image. To build a complete 96K image, the extension ROM should be placed in the build directory under the name x14000_rom and a 'make_rom' or 'make_lrespr' done (see BUILDING). Alternatively, the Q68_ROM.SYS image may be extended by issuing the following commands in an emulated QDOS or SMSQ/E environment:
~~~
base=RESPR(96*1024)
LBYTES Q68_ROM.SYS,base
LBYTES extension_rom_image,base+80*1024
RENAME Q68_ROM.SYS,Q68_ROM.ORG
SBYTES Q68_ROM.SYS,base,96*1024
~~~
and then the Q68_ROM.SYS file must be copied to a FAT32-formatted SDHC card. Please note that the Q68 requires the files on this card to lie in *contiguous* sectors, so if there are already any files on the card it's strongly recommended to save these, then reformat the card, and then write all files back at once.

If you have extension ROMs that insist on being placed in the $C000 slot, you may include them by placing the image in the build directory under the name xc000_rom. The keyboard and SD-card driver images will then be relocated to the $10000-$17FFF area and linked in after the xc000_rom extension. Note that you may include an extension ROM either at $C000 or $14000, but not at both locations as the total size of Minerva plus Q68 drivers plus extension ROM is limited to 96K!

As an alternative to loading the ROM image at startup, we now provide a boot loader which allows the Minerva system to be loaded from within a running SMSQ/E system. This avoids the need to use a separate FAT32-formatted SDHC card, and allows you to boot Minerva with a single LRESPR command. The boot loader Min4Q68ldr.bin is just 32 bytes and must be followed by the Q68_ROM.SYS image itself. Using 'make lrespr' after building the Q68_ROM.SYS image will create a Min4Q68_rext file which may be copied to a QDOS WIN container. Alternatively, you may create this file from within a QDOS-compatible system itself by issuing the following commands:
~~~
size=96*1024+32
base=RESPR(size)
LBYTES Min4Q68ldr.bin,base
LBYTES Q68_ROM.SYS,base+32
SBYTES Min4Q68_rext,base,size
~~~
CONFIGURATION:
--------------

The devices win1_ to win8_ and qub1_ to qub8_ can be configured to be mapped to any \*.WIN (QLWA format) or \*.BIN (Qubide format) container file by using the CONFIG or MENUCONFIG program on the Q68_ROM.SYS file. You MUST use a V2 capable version of these programs. Suitable CONFIG programs can be found on https://dilwyn.qlforum.co.uk/config/index.html.

HIGH RESOLUTION MODE 1024x768x4
-------------------------------

From v1.4 onwards, the Q68's 1024x768x4 mode is supported. Note that this mode has not been tested extensively so please use it with caution. 

The 1024x768x4 mode is implemented using the DISP_MODE command with a subset of the SMSQ/E version. Currently, modes 0 (256x256x8), 1 (512x256x4), and 4 (1024x768x4) are supported. Implementing the full range, including 65536-colour modes, would require a total rewrite of the screen drivers, including implementation of the GD2 colour schemes, which is far beyond the scope of this project and already available within SMSQ/E.

That said, it would be nice if the original QL's 8-colour mode could be made available at the higher resolutions offered by the Q68. Also, 1024x768 might be difficult to read on modern LCD-type monitors where the native resolution is not an exact multiple of 1024x768. I personally like the Q68's 512x384 mode which offers good readability (with better aspect ratio than 512x256!) at reasonable speed, with the possibility for more than the boring old 4 colours. So this remains an item on my to-do list...

Note that Minerva's dual screen feature is not supported in 1024x768 mode, and trying to switch to DISP_MODE 4 with dual screen enabled will produce a 'not complete' error. Please reboot first with dual screen disabled.

If you use the Pointer Interface in 1024x768 mode, then some caution is required. The ptr_gen program needs to be patched to support the extended screen size and different screen buffer address. Thus, you must load it with some code like this (Toolkit II extensions required):
~~~
200 DEFine PROCedure patch_ptr
210 LOCal a,p,s
220   a=RESPR(FLEN(\ptr_gen))
230   LBYTES ptr_gen,a
240   s=0: PRINT "Patching pointer interface...";
250   FOR p=a TO a+FLEN(\ptr_gen) STEP 2
260     IF PEEK_L(p)=32768 AND PEEK_W(p+4)=128 AND PEEK_W(p+6)=512 AND PEEK_W(p+8)=256 THEN
270       POKE_L p-4,HEX('fe800000'): POKE_L p,HEX('30000'): REMark buffer address and size
280       POKE_W p+4,256: POKE_W p+6,1024: POKE_W p+8,768: REMark line length, X size, Y size
290       s=1: EXIT p
300     END IF
310   END FOR p
320   IF s=1 THEN
330     PRINT "Success!": CALL a: LRESPR wman: LRESPR hot_rext
340   ELSE
350     PRINT "Failed!"
360   END IF
370 END DEFine patch_ptr
~~~
Note that you must switch to 1024x768 mode *BEFORE* activating the Pointer Interface. After this, you cannot switch back to the lower-resolution modes.

The functions SCR_BASE, SCR_LLEN, SCR_XLIM and SCR_YLIM return the base address, pixel line length in bytes, and X and Y limits of the current screen mode, like their SMSQ/E counterparts. In the current version, any parameters are ignored.

SERIAL PORT SUPPORT
-------------------

From v1.6 onwards, the Q68's serial port is supported using a new driver in the ROM image. It offers the following features:

- Configurable port name; default SER1 but can be changed using the SER_USE command
- Alternative port names for transmit- and receive-only channels (STXx/SRXx), for use with SERnet
- Baud rate configurable from 1200 to 230400 bits per second (using normal BAUD command)
- Flow control using XON/XOFF protocol with optional data transparency (between two Q68s or Q68 and QIMSI)
- Configurable transmit- and receive buffers using SER_BUFF command

The Q68's serial port is much faster than the original QL's SER ports, but unfortunately lacks CTS/RTS lines so all flow control has to be done in software using XON/XOFF handshake. The original QDOS/Minerva driver has only fixed-size buffers of 81 bytes, which is not adequate for handling high speeds. SMSQ/E, by contrast, has buffers of configurable size, and by default uses dynamic-size transmit buffers which can grow to insane size (probably designed to send files in quick succession to a printer). Unfortunately, all current versions of SMSQ/E do not support the XON/XOFF protocol even though the driver accepts 'X' as option on channel opens or as parameter to the SER_FLOW command, so sending or receiving files from or to the Q68 at full speeds will more or less lead to data corruption. 

Reliable transfers are possible using SERnet (https://dilwyn.qlforum.co.uk/tk/sernet.zip; please use v2.25 as v3 will not work with Minerva). When using default buffer size, it is not necessary to enable XON/XOFF flow control, so specifying SRX1I/STX1I as device name will be sufficient. Using SERnet, I was able to achieve througputs up to 8.5K bytes at 115200 bps, which is twice as fast as the original QLAN network.

Commands available are:

- SER_USE *name*, where *name* should be a four-character device name. It main use is to substitute an existing SER port such as ser2 or ser3, so existing software using these names will be able use the port. In addition, the STX and SRX transmit-only and receive-only devices will have their last character modified as well, so entering SER_USE SER2 will change these names to STX2 and SRX2 respectively.
- SER_FLOW takes a one-letter parameter *I*, *H*, or *X*, where
  - *I*: stands for no flow-control (i.e. send at full speed, ignore XON/XOFFs sent by the remote)
  - *X*: use XON/XOFF flow control when sending and receiving. This is not transparent to the data; if your data contains either of these characters (11H and 13H) this will disrupt transfers. Only use it when sending plain text data.
  - *H*: Use XON/XOFF flow control but escape these characters in the data stream (using DLE, so XON will be sent as 10H followed by 'Q' and XOFF as 10H followed by 'S'). Using this technique, full 8-bit data transfers will be possible whilst still providing flow control (equivalent to using the *H* option with DTR/CTS handshake on the original QL's SER ports). This is an implementation-specific extension to the protocol and will only work between two Q68s using the SER driver, or a QIMSI serial port using SER4.
  - Note that these options may also be specified when opening a channel to the port; e.g. OPEN#3,ser1x will open the serial port and use XON/XOFF flow control.
- SER_BUFF *txbuffer*,*rxbuffer* sets the size of the transmit buffer and (optionally) the receive buffer. This allows you to configure the transmit and receive buffers, like SMSQ/E's SER_BUFF command. Its behaviour is somewhat different in that the default sizes are 16384 bytes for the receive buffer and 1024 bytes for the transmit buffer, and that dynamic-sized transmit buffers are not possible (they will usually fail when flow control has to be asserted, e.g. with Zmodem transfers).
- SER_ROOM *threshold* sets the receive buffer's threshold for asserting flow control. When the receive buffer has been filled up to the point where there are less than *threshold* bytes free, a XOFF is sent to the remote. This threshold is also affected by SER_BUFF, which sets it to 1/4th of the receive buffer size. When the receive buffer has been emptied for 75 percent of its size, a XON character will be sent to the remote.
- SER_CLEAR clears both input and output buffers.

Current issues:
---------------

- The maximum amount of RAM supported is limited to 16MB, as the slave block system's structure currently prevents supporting more RAM. If you can do with less, I even recommend to cut RAM to a lower value using CALL 390,<RAMTOP value in bytes> to limit the size of the slave block table and speed up file access (see Minerva manual for more CALL 390 boot-time options).
- The SD-card driver requires a CARD_INIT 2 command to use the SD card in slot 2; other SD-card related commands are presently not implemented.
- The QLNET and Ethernet interfaces are supported using external utilities, see https://dilwyn.qlforum.co.uk/q68/index.html for more information.
- Some users of Q68 boards with newer firmware (v1.05) have reported problems with the keyboard and the Q68 'freezing' after the F1/F2 prompt. These are currently under investigation. Please use the Issues section to report any problems, stating as much information as possible (including the firmware version, which can be read from the Q68's initial boot screen; temporary removal of the SD card will give you enough time to read it).

Contributors:
-------------

- Minerva operating system by Laurence Reeves;
- Keyboard driver: Richard Zidlicky, Jan Bredenbeek
- Mouse driver: Peter Graf
- SDHC device driver: Peter Graf, Wolfgang Lenerz
- Serial driver: Jan Bredenbeek

Version history:
----------------

- 31 December 2023: v1.61 released
  - Register A0 now properly preserved on initialisation; avoid issues with F1/F2 startup prompt
- 28 December 2023: v1.6 released. 
  - Added serial port support (see above)
  - Q68 hardware is now properly reset when loading using LRESPR
  - Added SLUG command (SLUG 0 is maximum speed, SLUG 255 slows down to a crawl)
  - KEYROW emulation improved and now works even if interrupts are disabled, as many games do.
- 5 November 2023: Added mouse support via external driver from Peter Graf (see mouse subdirectory)
- 2 August 2023: extrarom v1.5 released. Included ALFM and FREE_FM keywords (allocate fast memory, see Q68 manual). Building environment moved from Linux to SMSQ/E. Credits added for the SDHC driver to Peter Graf.
- 9 July 2023: v1.4 released. Implemented 1024x768x4 mode (beta)
- 29 June 2023: v1.3 released. Support for external interrupts on all Q68 firmware versions, support for keyboard interrupt on newer firmware versions, one language version now for all three keyboard layouts.
- June 2021: Combined keyboard and SD-card drivers in one single ROM image, leaving 16K available for other extension ROMs. Patched SD-card driver for stale TRAP #14 instruction left over (from debugging?)
- May 2019: Patch included for LBYTES bug over network (contributed by Marcel Kilgus)
- April 2019: improved keyboard driver, now only compatible with Minerva
- May 2018: support for US and UK keyboards, RAM test limited to 16MB to avoid problems with slave block system
