100 Min4Q68_dev$=DATAD$
110 QMake_out$="ram8_"
120 :
130 DEFine PROCedure make_minerva
140   PRINT#0;"Building Minerva..."
150   EW QMake;"\C " & Min4Q68_dev$ & "m_rom \B"
160   IF FTEST(QMake_out$ & "M_ROM_bin")=0 THEN
170     COPY_O QMake_out$ & "M_ROM_bin" TO Min4Q68_dev$ & "Minerva_bin"
180     COPY_O QMake_out$ & "M_ROM_map" TO Min4Q68_dev$ & "Minerva_map"
190   END IF
200 END DEFine make_minerva
210 :
220 DEFine PROCedure make_extrarom
230   PRINT#0;"Building extrarom..."
240   EW QMake;"\C " & Min4Q68_dev$ & "extrarom \B"
250   IF FTEST(QMake_out$ & "extrarom_bin")=0 THEN
260     COPY_O QMake_out$ & "extrarom_bin" TO Min4Q68_dev$ & "extrarom_bin"
270     COPY_O QMake_out$ & "extrarom_map" TO Min4Q68_dev$ & "extrarom_map"
280   END IF
290 END DEFine make_extrarom
300 :
310 DEFine PROCedure make_rom
320   IF FTEST(Min4Q68_dev$ & "Minerva_bin") < 0: make_minerva
330   IF FTEST(Min4Q68_dev$ & "extrarom_bin") < 0: make_extrarom
340   base=ALCHP(96*1024)
350   LBYTES Min4Q68_dev$ & "Minerva_bin",base
360   a=base+48*1024: IF FTEST(Min4Q68_dev$ & "xc000_rom") = 0 THEN
370     LBYTES Min4Q68_dev$ & "xc000_rom",a
380     a=a+16*1024
390   END IF
400   LBYTES Min4Q68_dev$ & "extrarom_bin",a
410   LBYTES Min4Q68_dev$ & "wl_minv.dv3",a+FLEN(\Min4Q68_dev$ & "extrarom_bin")
420   a=a+32*1024: IF a=HEX('14000') AND FTEST(Min4Q68_dev$ & "x14000_rom") = 0 THEN
430     LBYTES Min4Q68_dev$ & "x14000_rom",a
440     a=a+16*1024
450   END IF
460   SBYTES_O Min4Q68_dev$ & "Q68_ROM.SYS",base,a-base
470   RECHP base
480 END DEFine make_rom
490 :
500 DEFine PROCedure make_lrespr
510   IF FTEST(Min4Q68_dev$ & "Minerva_bin") < 0: make_minerva
520   IF FTEST(Min4Q68_dev$ & "extrarom_bin") < 0: make_extrarom
530   base=ALCHP(96*1024+32)
540   LBYTES Min4Q68_dev$ & "Min4Q68ldr.bin",base
550   LBYTES Min4Q68_dev$ & "Minerva_bin",base+32
560   a=base+48*1024+32: IF FTEST(Min4Q68_dev$ & "xc000_rom") = 0 THEN
570     LBYTES Min4Q68_dev$ & "xc000_rom",a
580     a=a+16*1024
590   END IF
600   LBYTES Min4Q68_dev$ & "extrarom_bin",a
610   LBYTES Min4Q68_dev$ & "wl_minv.dv3",a+FLEN(\Min4Q68_dev$ & "extrarom_bin")
620   a=a+32*1024: IF a=HEX('14020') AND FTEST(Min4Q68_dev$ & "x14000_rom") = 0 THEN
630     LBYTES Min4Q68_dev$ & "x14000_rom",a
640     a=a+16*1024
650   END IF
660   SBYTES_O Min4Q68_dev$ & "Min4Q68_rext",base,a-base
670   RECHP base
680 END DEFine make_lrespr
690 :
700 DEFine PROCedure make_clean
710 LOCal d$,fnr,fnm$
720   logchan=FOP_OVER("ram1_make_log"): IF logchan < 0 THEN REPORT#0;logchan: RETurn
730   RESTORE
740   REPeat loop
750     IF EOF THEN EXIT loop
760     READ d$: d$=Min4Q68_dev$ & d$ & "_"
770     dirch=FOP_DIR(d$): IF dirch < 0 THEN PRINT#0;"Error opening ";d$: REPORT#0;dirch: RETurn
780     fnr=-1
790     REPeat file_lp
800       fnr=fnr+1
810       GET#dirch\fnr*64: IF EOF(#dirch) THEN EXIT file_lp
820       GET#dirch\fnr*64+14;fnm$: IF fnm$="" THEN NEXT file_lp
830       IF fnm$(LEN(fnm$)-3 TO) == "_REL" THEN
840         PRINT#logchan;"Deleting ";Min4Q68_dev$ & fnm$: DELETE Min4Q68_dev$ & fnm$
850       END IF
860     END REPeat file_lp
870     CLOSE#dirch
880     PRINT#logchan;"Deleting ";d$ & "lib": DELETE d$ & "lib"
890   END REPeat loop
900   CLOSE#logchan
910 END DEFine make_clean
920 :
930 DEFine PROCedure Make
940   make_minerva: make_extrarom: make_rom: make_lrespr
950 END DEFine Make
960 :
970 CLS: PRINT "*** Minerva4Q68 Make program ***"
980 PRINT\"Options:"
990 PRINT\"make_minerva : Build Minerva (";Min4Q68_dev$;"Minerva_bin)"
1000 PRINT "make_extrarom: Build ROM drivers (";Min4Q68_dev$;"extrarom_bin)"
1010 PRINT "make_rom     : Build complete Q68 ROM image (";Min4Q68_dev$;"Q68_ROM.SYS)"
1020 PRINT "make_lrespr  : Build LRESPRable ROM image (";Min4Q68_dev$;"Min4Q68_rext)"
1030 PRINT "Make         : Build all"
1040 PRINT "make_clean   : Cleanup assembled files (_REL, lib etc.)"
1050 PRINT\"Required programs: Quanta assembler and linker (QMAC, QLINK), QMake"
1060 PRINT "(see https://dilwyn.qlforum.co.uk/asm/index.html)"
1070 IF Min4Q68_dev$ = "" OR FTEST(Min4Q68_dev$ & "M_") <> 0 OR FTEST(Min4Q68_dev$ & "extrarom_") <>0 THEN
1080   PRINT\"** ERROR: Source directory missing; have you set Min4Q68_dev$ correctly?"
1090 END IF
1100 IF FTEST(PROGD$ & "QMake") <> 0: PRINT\"** ERROR: QMake not found in your PROGD$ (";PROGD$;")!"
1110 DATA "m_bf","m_bp","m_bv","m_ca","m_cn","m_cs","m_dd","m_gw","m_ib","m_ii","m_io","m_ip","m_md"
1120 DATA "m_mm","m_mt","m_nd","m_od","m_pa","m_pf","m_q68","m_ri","m_sb","m_sd","m_ss","m_tb","m_ut"
1130 DATA "extrarom"
