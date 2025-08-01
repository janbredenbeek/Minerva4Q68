100 Min4Q68_dev$=DATAD$
110 QMake_out$="ram8_"
120 dv3_driver$="wl_minv.dv3"
130 :
140 DEFine PROCedure make_minerva
150   PRINT#0;"Building Minerva..."
160   EW QMake;"\C " & Min4Q68_dev$ & "m_rom \B"
170   IF FTEST(QMake_out$ & "M_ROM_bin")=0 THEN
180     COPY_O QMake_out$ & "M_ROM_bin" TO Min4Q68_dev$ & "M_ROM_bin"
190   END IF
200   IF FTEST(QMake_out$ & "M_ROM_map")=0 THEN
210     COPY_O QMake_out$ & "M_ROM_map" TO Min4Q68_dev$ & "M_ROM_map"
220   END IF
230   PRINT\"Minerva ROM image is ";FLEN(\Min4Q68_dev$ & "M_ROM_bin");" bytes"
240   IF FLEN(\Min4Q68_dev$ & "M_ROM_bin") > 48*1024 THEN
250     PRINT\"*** ERROR: Minerva ROM image too large (>48K)": STOP
260   END IF
270 END DEFine make_minerva
280 :
290 DEFine PROCedure make_standalone
300   make_minerva
310   IF FTEST(Min4Q68_dev$ & "M_ROM_bin")=0 THEN
320     base=ALCHP(48*1024): LBYTES Min4Q68_dev$ & "M_ROM_bin",base
330     SBYTES_O Min4Q68_dev$ & "Minerva_bin",base,48*1024
340     PRINT\"Saved 48K ROM image ";Min4Q68_dev$;"Minerva_bin": RECHP base
350   ELSE
360     PRINT\"*** ERROR: Assembled ROM image not found"
370   END IF
380 END DEFine make_standalone
390 :
400 DEFine PROCedure make_extrarom
410   PRINT#0;"Building extrarom..."
420   IF FTEST(Min4Q68_dev$ & "hist_bin") < 0: make_history
430   EW QMake;"\C " & Min4Q68_dev$ & "extrarom \B"
440   IF FTEST(QMake_out$ & "extrarom_bin")=0 THEN
450     COPY_O QMake_out$ & "extrarom_bin" TO Min4Q68_dev$ & "extrarom_bin"
460   END IF
470   IF FTEST(QMake_out$ & "extrarom_map")=0 THEN
480     COPY_O QMake_out$ & "extrarom_map" TO Min4Q68_dev$ & "extrarom_map"
490   END IF
500   PRINT\"ExtraROM image is ";FLEN(\Min4Q68_dev$ & "extrarom_bin");" bytes"
510 END DEFine make_extrarom
520 :
530 DEFine PROCedure make_history
540   IF FTEST(Min4Q68_dev$ & "hist_rext") <> 0 THEN
550     PRINT\"*** ERROR: file hist_rext not found, please copy it first from SMSQ/E source!": STOP
560   END IF
570   PRINT#0;"Building HISTORY..."
580   EW QMake;"\C " & Min4Q68_dev$ & "hist \B"
590   IF FTEST(QMake_out$ & "hist_bin")=0 THEN
600     COPY_O QMake_out$ & "hist_bin" TO Min4Q68_dev$ & "hist_bin"
610   END IF
620   IF FTEST(QMake_out$ & "hist_map")=0 THEN
630     COPY_O QMake_out$ & "hist_map" TO Min4Q68_dev$ & "hist_map"
640   END IF
650 END DEFine make_history
660 :
670 DEFine PROCedure make_rom
680   IF FTEST(Min4Q68_dev$ & "M_ROM_bin") < 0: make_minerva
690   IF FTEST(Min4Q68_dev$ & "extrarom_bin") < 0: make_extrarom
700   base=ALCHP(96*1024)
710   LBYTES Min4Q68_dev$ & "M_ROM_bin",base
720   a=base+48*1024: IF FTEST(Min4Q68_dev$ & "xc000_rom") = 0 THEN
730     LBYTES Min4Q68_dev$ & "xc000_rom",a
740     a=a+16*1024
750   END IF
760   rom_end=a-base+FLEN(\Min4Q68_dev$ & "extrarom_bin")+FLEN(\Min4Q68_dev$ & dv3_driver$)
770   rom_len=rom_end
780   IF rom_end > HEX("18000") THEN PRINT "*** ERROR: Total ROM size too large (>96K)": STOP
790   LBYTES Min4Q68_dev$ & "extrarom_bin",a
800   LBYTES Min4Q68_dev$ & dv3_driver$,a+FLEN(\Min4Q68_dev$ & "extrarom_bin")
810   IF rom_end < HEX("14000") THEN
820     IF FTEST(Min4Q68_dev$ & "x14000_rom") = 0 THEN
830       LBYTES Min4Q68_dev$ & "x14000_rom",base+HEX('14000')
840       rom_end=HEX('18000')
850       rom_len=rom_len+HEX('4000')
860     ELSE
870       rom_end=HEX('14000')
880     END IF
890   ELSE
900     rom_end=HEX("18000")
910   END IF
920   SBYTES_O Min4Q68_dev$ & "Q68_ROM.SYS",base,rom_end
930   PRINT\"Q68_ROM.SYS is ";rom_len;" bytes (padded to ";rom_end;" bytes)"
940   RECHP base
950 END DEFine make_rom
960 :
970 DEFine PROCedure make_lrespr
980   IF FTEST(Min4Q68_dev$ & "M_ROM_bin") < 0: make_minerva
990   IF FTEST(Min4Q68_dev$ & "extrarom_bin") < 0: make_extrarom
1000   IF FLEN(\Min4Q68_dev$ & "Minerva_bin") > 48*1024 THEN
1010     PRINT "*** ERROR: Minerva image too large (>48K)"
1020     STOP
1030   END IF
1040   base=ALCHP(96*1024+32)
1050   LBYTES Min4Q68_dev$ & "Min4Q68ldr.bin",base
1060   LBYTES Min4Q68_dev$ & "M_ROM_bin",base+32
1070   a=base+48*1024+32: IF FTEST(Min4Q68_dev$ & "xc000_rom") = 0 THEN
1080     LBYTES Min4Q68_dev$ & "xc000_rom",a
1090     a=a+16*1024
1100   END IF
1110   rom_end=a-base+FLEN(\Min4Q68_dev$ & "extrarom_bin")+FLEN(\Min4Q68_dev$ & dv3_driver$)
1120   rom_len=rom_end
1130   IF rom_end > HEX("18020") THEN PRINT "*** ERROR: Total ROM size too large (>96K)": STOP
1140   LBYTES Min4Q68_dev$ & "extrarom_bin",a
1150   LBYTES Min4Q68_dev$ & dv3_driver$,a+FLEN(\Min4Q68_dev$ & "extrarom_bin")
1160   IF rom_end < HEX("14020") THEN
1170     IF FTEST(Min4Q68_dev$ & "x14000_rom") = 0 THEN
1180       LBYTES Min4Q68_dev$ & "x14000_rom",base+HEX('14020')
1190       rom_end=HEX('18020')
1200       rom_len=rom_len+HEX('4000')
1210     ELSE
1220       rom_end=HEX('14020')
1230     END IF
1240   ELSE
1250     rom_end=HEX("18020")
1260   END IF
1270   SBYTES_O Min4Q68_dev$ & "Min4Q68_rext",base,rom_end
1280   PRINT\"Min4Q68_rext is ";rom_len;" bytes (padded to ";rom_end;" bytes)"
1290   RECHP base
1300 END DEFine make_lrespr
1310 :
1320 DEFine PROCedure del(f$)
1330   f$=Min4Q68_dev$ & f$
1340   PRINT#logchan;"Deleting ";f$
1350   DELETE f$
1360 END DEFine del
1370 :
1380 DEFine PROCedure make_clean
1390 LOCal d$,fnr,fnm$
1400   logchan=FOP_OVER("ram1_make_log"): IF logchan < 0 THEN REPORT#0;logchan: RETurn
1410   RESTORE
1420   REPeat loop
1430     IF EOF THEN EXIT loop
1440     READ d$: d$=d$ & "_"
1450     dirch=FOP_DIR(d$): IF dirch < 0 THEN PRINT#0;"Error opening ";d$: REPORT#0;dirch: RETurn
1460     fnr=-1
1470     REPeat file_lp
1480       fnr=fnr+1
1490       GET#dirch\fnr*64: IF EOF(#dirch) THEN EXIT file_lp
1500       GET#dirch\fnr*64+14;fnm$: IF fnm$="" THEN NEXT file_lp
1510       IF fnm$(LEN(fnm$)-3 TO) == "_REL" THEN
1520         del fnm$
1530       END IF
1540     END REPeat file_lp
1550     CLOSE#dirch
1560     del d$ & "lib"
1570   END REPeat loop
1580   del "M_ROM_bin": del "extrarom_bin": del "hist_bin"
1590   CLOSE#logchan
1600 END DEFine make_clean
1610 :
1620 DEFine PROCedure Make
1630   make_minerva: make_extrarom: make_rom: make_lrespr
1640 END DEFine Make
1650 :
1660 CLS: PRINT "*** Minerva4Q68 Make program ***"
1670 PRINT\"Options:"
1680 PRINT\"make_minerva   : Build Minerva (";Min4Q68_dev$;"M_ROM_bin)"
1685 PRINT "make_standalone: Build standalone Minerva (";Min4Q68_dev$;"Minerva_bin; padded to 48K)"
1690 PRINT "make_extrarom  : Build ROM drivers (";Min4Q68_dev$;"extrarom_bin)"
1700 PRINT "make_history   : Build HISTORY device for Minerva (";Min4Q68_dev$;"hist_bin)"
1710 PRINT "make_rom       : Build complete Q68 ROM image (";Min4Q68_dev$;"Q68_ROM.SYS)"
1720 PRINT "make_lrespr    : Build LRESPRable ROM image (";Min4Q68_dev$;"Min4Q68_rext)"
1730 PRINT "Make           : Build all except standalone"
1740 PRINT "make_clean     : Cleanup assembled files (_REL, lib etc.)"
1750 PRINT\"Required programs: Quanta assembler and linker (QMAC, QLINK or QJump Linker), QMake"
1760 PRINT "(see https://dilwyn.theqlforum.com/asm/index.html)"
1770 IF Min4Q68_dev$ = "" OR FTEST(Min4Q68_dev$ & "M_") <> 0 OR FTEST(Min4Q68_dev$ & "extrarom_") <>0 THEN
1780   PRINT\"** ERROR: Source directory missing; have you set Min4Q68_dev$ correctly?"
1790 END IF
1800 IF FTEST(PROGD$ & "QMake") <> 0: PRINT\"** ERROR: QMake not found in your PROGD$ (";PROGD$;")!"
1810 DATA "m_bf","m_bp","m_bv","m_ca","m_cn","m_cs","m_dd","m_gw","m_ib","m_ii","m_io","m_ip","m_md"
1820 DATA "m_mm","m_mt","m_nd","m_od","m_pa","m_pf","m_q68","m_ri","m_sb","m_sd","m_ss","m_tb","m_ut"
1830 DATA "extrarom","hist"
