;q68 hardware drivers                                                  
        include 'm_mincf'
        include 'm_inc_q68'

    GENIF Q68_KEY <> 0
	xdef q68kbd_init
    ENDGEN
    GENIF  0 <> 0
	xdef	q68_skeyb
    ENDGEN
    GENIF SDEBUG <> 0
	xdef	tq68_dumpregs
    ENDGEN

	xref	io_qin,io_qtest
	xref	io_relio
	xref	sd_cure

	include m_inc_bv
	include m_inc_ch
	include m_inc_jb
	include m_inc_mc
	include m_inc_q
	include m_inc_sd
	include m_inc_sv
	include m_inc_sx

        section q68_q68hw

************************************************
* send debugging stuff to serial
	
    GENIF SDEBUG <> 0

* trap entry point to dump registers to serial line
* output format (hex):
* pc.l sr.w, a6 - a0, d7 - d0, a7		
* 0000B920 2200 00028000 0002BC60 000FF568 0002C510 000FF400
* 00000100 0000BBD8 00028080 00000000 00000080 FFFFFFF8 00000001 00000001
* 00003100 0002C216 000283B4

tq68_dumpregs:
	movem.l d0-d7/a0-a6,-(a7)
	lea	66(a7),a1 	; 15*4+2+4
	move.l	-(a1),d0	; pc
	bsr.s	q68_ssendl
	move.w	-(a1),d0
	bsr.s	q68_ssendw	; sr
	moveq	#14,d2
tq6dl:	move.l	-(a1),d0
	bsr.s	q68_ssendl
	dbra	d2,tq6dl
	lea	66(a7),a1	; reconstruct a7 value
	move.l	a7,d0	** TYPO? ** should probably be a1
	bsr.s	q68_ssendl
	moveq	#10,d0		; extra line feed
	bsr.s 	q68_ssendc
	moveq	#10,d0		; extra line feed
	bsr.s 	q68_ssendc	; .. and yet another
	movem.l	(a7)+,d0-d7/a0-a6
	rte
	
q68_ssinit:
*	move.b XXX,UART_PRESCALE
	rts
*void Ser1CharOut(char c)
*{
*  /* Wait until Transmitter Buffer empty */
*  while (!(UART_STATUS & 0x01));
*  UART_TXDATA = c;
*}

**** send convert to HEX and send to serial ****	
q68_snib:
	cmp.b	#10,d0
	blt	qnm	* <10 -> add $30
	add.b	#$41-10,d0
	bra.s	q68_ssendc
qnm:	add.b	#$30,d0
* FALLTHRU
q68_ssendc
	btst	#0,UART_STATUS
	beq.s	q68_ssendc
	move.b	d0,UART_TXDATA
	rts

* send byte as HEX
q68_sxb:
	movem.l	d0-d1,-(a7)
	move.b	d0,d1
	asr.b	#4,d0
	and.b	#$f,d0
	bsr.s	q68_snib
	move.b	d1,d0
	and.b	#$f,d0
	bsr.s	q68_snib
	movem.l	(a7)+,d0-d1
	rts
* send longword in D0 out to serial line
q68_ssendl:
	rol.l	#8,d0
	bsr.s 	q68_sxb
	rol.l	#8,d0
	bsr.s 	q68_sxb

	rol.l	#8,d0
	bsr.s 	q68_sxb
	rol.l	#8,d0
	bsr.s 	q68_sxb

	moveq	#$20,d0		; space
	bsr.s 	q68_ssendc
	rts

q68_ssendw:
	rol.w	#8,d0
	bsr.s 	q68_sxb
	rol.w	#8,d0
	bsr.s 	q68_sxb

	moveq	#$20,d0		; space
	bsr.s 	q68_ssendc
	rts
    ENDGEN
	

****** q68 PS2 keyboard interface ********
    GENIF Q68_KEY <> 0
*  linked in as poll routine, all variables stored rel a3
*  KEYBOARD variables

;SV_LXINT	EQU	$00	; (long) ptr to next link
				; (long) address of EXT INT routine

;SV_LPOLL	EQU	$08	; (long) ptr to next link
				; (long) address of POLLed int routine

VAR.KEYtab   EQU	$44	; (long) ptr to ASCII table

VAR.KEYraw   EQU	$48	; (8xbyte) used to emulate KEYROW

VAR.CTLflg   EQU	$50	; (byte) CTRL key is down
VAR.SHFflg   EQU	$51	; (byte) SHIFT key is down
VAR.ALTflg   EQU	$52	; (byte) ALT key is down
VAR.NLKflg   EQU	$53	; (byte) status of NUMLOCK

VAR.RLSflg   EQU	$54	; (byte) next key is to be released
VAR.MODflg   EQU	$55	; (byte) next key is 'special'

VAR.LEDflg   EQU	$56	; (byte) status of LEDs

VAR.ACTkey   EQU	$58	; (byte) value gotten from keyboard
VAR.ASCkey   EQU	$59	; (byte) value converted to ASCII

VAR.GFXflg   EQU	$5A	; (byte) status ALT-Gr key

VAR.KEYdwc   EQU	$5C	; (byte) count of keys held down
VAR.KEYdwk   EQU	$5E	; (16 x byte) ACTUAL key-down list
VAR.KEYdwa   EQU	$6E	; (16 x byte) ASCII key-down list

VAR.LEN	    EQU	$7E	; length of vars

*workaround broken gwass	
vdwak	equ	VAR.KEYdwa-VAR.KEYdwk	

	
* get scancodes and send to serial for analysis
*q68_ps2keyb:
*	nop			;  patch with rts to disable
*	btst.b	#0,PS2_STATUS
*	beq.s	pskeyend
*	move.l	#'KEY:',d0
*	bsr.s	q68_ssendl

*kbl	move.b	KEY_CODE,d0
*	bsr	q68_sxb
*	moveq	#$20,d0		; space
*	bsr.s 	q68_ssendc
*	move.b	#1,KEY_UNLOCK

*	btst.b	#0,PS2_STATUS
*	bne.s	kbl
*	moveq	#10,d0		; extra line feed
*	bsr.s 	q68_ssendc
	
*pskeyend
*	rts
 

* keytables from QDOS Classic
* /home/rz/qdos/qdos-classic/QZ-net/CLSC/SRC/CORE/KBD_asm - main kbd driver 
*	+ gets decoded keycodes/vars (bsr HW_KEY_read)
*	+ KEY_conv: convert to ASCII
* /home/rz/qdos/qdos-classic/QZ-net/CLSC/SRC/Q40/KBD_asm
*	+ HW_KEY_read: read HW, call KEY_decode
* /home/rz/qdos/qdos-classic/QZ-net/CLSC/SRC/ISA/804Xa_asm
*	+ KEY_decode: press/relse, modifier and weird keys
* /home/rz/qdos/qdos-classic/QZ-net/CLSC/SRC/ISA/804Xd_asm - keytable-de


* Scheduler list routine for handling serial output
* nothing at all right now for the q68
* d3 -ip - count of missed poll interrupts
* d0-d2/d4-d6/a0-a5 destroyed.
*ip_sched
*        rts



***************************************************************
** Q40 KBD_asm

*	INCLUDE	'CLSC_SRC_CORE_KBD_inc'
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;  start of ROM code

q68kbd_init:

	movem.l	d0-d3/d6-d7/a0-a4/a6,-(a7)

* driver memory
	moveq	#0,d2		; owner is superBASIC
* VAR.LEN should be enough	
	move.l	#18+VAR.LEN,d1  ; length
	moveq	#$18,d0		;  MT.ALCHP
	trap	#1		; allocate space

	tst.l	d0
	bne.s	ROM_EXIT 	; exit if error

	move.l	a0,a3
; --------------------------------------------------------------
;  set ASCII table and clear actual key.

	lea	LNG_KTAB(pc),a0
	move.l	a0,VAR.KEYtab(a3)

	clr.b	VAR.KEYdwc(a3)	; clear held down key count

	lea	VAR.KEYraw(a3),a0
	clr.l	(a0)+
	clr.l	(a0)+		; invalidate KEYROW bits

	lea	VAR.CTLflg(a3),a0

	clr.w	(a0)+
	move.w	#$00FF,(a0)+
	clr.l	(a0)+		; clear/set the flags

	lea	VAR.ACTkey(a3),a0
	clr.w	(a0)+		; clear keycodes

	clr.w	sv_arcnt(a6)		; disable key repeat
; --------------------------------------------------------------
;  link in polled task routine to handle keyboard

	lea	POLL_SERver(pc),a1 ; address of routine
	lea	SV_LPOLL(a3),a0
	move.l	a1,4(a0) 	; address of polled task
	moveq	#$1c,d0		;  MT.LPOLL
	trap	#1
	bsr.s	boot_init
	
ROM_EXIT:
	movem.l	(a7)+,d0-d3/d6-d7/a0-a4/a6
	rts

*****************************************************	
* BOOT driver for debugging purposes *
bpos	equ	$18		;  store pos at this offset in chdef block
	
boot_init	
        moveq    #0,d2                      ; owner
	moveq	#$18,d0
	trap	#1
        tst.l    d0
        bne.s    ret_boot

	move.l   a0,a2                      ; Adresse retten

        lea      4(a2),a1 
        lea      boot_io,a0
        move.l   a0,(a1)+
        lea      boot_open,a0
        move.l   a0,(a1)+
        lea      boot_close,a0
        move.l   a0,(a1)

        move.l  a2,a0
        moveq	#$20,d0		; mt.liod
	trap	#1		
ret_boot
	rts

boot_open
        move.l   a7,a3		; no pars
        move.w   $122,a2	; io.name
        jsr      (a2)         
        bra.s    wrong_nm
        bra.s    wrong_nm
        bra.s    ok_nm
        dc.w     4
	dc.b	'BOOT'
        dc.w     0		; 0 pars

wrong_nm
	moveq	#-7,d0 
	rts

ok_nm
*       cmp.l    #1,d3		
*	bne.s	exit_bn
        move.l   #$18+4,d1	;  just one long

        move.w   $c0,a2	;  mm.alchp
        jsr      (a2)
        bne.s    exit

	clr.l	bpos(a0)	;  pos at start
        moveq    #0,d0
ret     rts

exit_iu  moveq    #-9,d0
exit     rts
exit_bn  moveq    #-12,d0
         rts
exit_nc  moveq    #-1,d0
         rts


boot_io
	move.w	$ea,a2		;  serio
        jsr    2(a2)		;  HACK serio+2=relio
	dc.w	bfpend-*
	dc.w	bfbyte-*
	dc.w	bsbyte-*

bfpend:
	move.l	boot_len,d0
	sub.l	bpos(a0),d0
	ble.s	fend
	clr.l	d0
	rts
bfbyte:	
	move.l	boot_len,d0
	sub.l	bpos(a0),d0
	ble.s	fend
	move.l	bpos(a0),d0
	move.b	boot_buf(pc,d0.l),d1
	addq.l	#1,d0
	move.l	d0,bpos(a0)
	clr.l	d0
	rts
	
fend    moveq    #-10,d0
        rts
bsbyte
	moveq	#-20,d0
	rts		

BOOT_CLOSE
         move.w   $c2,a2                        MM_RECHP
         move.l   a3,a4
         jsr      (a2)
  GENIF 0 <> 0
         lea      $18(a4),a0
         moveq    #mt.riod,d0
         trap     #1
         moveq    #mt.rechp,d0
         trap     #1
  ENDGEN
         rts

boot_len
	dc.l	5
boot_buf	
	dc.b	'LIST',10
		

*****************************************************		


; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Conversion tables for translating ASCII to KEYROW
;
; The organization is (in ASCII order):
;  CTRL(bit7) SHFT(bit6) ROWnumber(bits5-3) COLnumber(bits2-0)
   GENIF 0 <> 0
QLRAWKEY:
 dc.b @225,@244,@224,@223,@246,@264,@234,@236
 dc.b @242,@053,@010,@232,@240,@226,@276,@257
 dc.b @245,@263,@254,@233,@266,@267,@274,@251
 dc.b @273,@256,@221,@013,@315,@320,@325,@313
 dc.b @016,@143,@127,@141,@106,@102,@107,@027
 dc.b @150,@165,@160,@135,@077,@055,@022,@075
 dc.b @065,@043,@061,@041,@006,@002,@062,@007
 dc.b @060,@050,@137,@037,@177,@035,@122,@175
 dc.b @161,@144,@124,@123,@146,@164,@134,@136
 dc.b @142,@152,@147,@132,@140,@126,@176,@157
 dc.b @145,@163,@154,@133,@166,@167,@174,@151
 dc.b @173,@156,@121,@030,@015,@020,@162,@155
 dc.b @025,@044,@024,@023,@046,@064,@034,@036
 dc.b @042,@052,@047,@032,@040,@026,@076,@057
 dc.b @045,@063,@054,@033,@066,@067,@074,@051
 dc.b @073,@056,@021,@130,@115,@120,@125,@113
 dc.b @213,@343,@327,@341,@306,@302,@307,@227
 dc.b @350,@365,@360,@335,@277,@255,@222,@275
 dc.b @265,@243,@261,@241,@206,@202,@262,@207
 dc.b @260,@250,@337,@237,@377,@235,@322,@375
 dc.b @361,@344,@324,@323,@346,@364,@334,@336
 dc.b @342,@352,@347,@332,@340,@326,@376,@357
 dc.b @345,@363,@354,@333,@366,@367,@374,@351
 dc.b @373,@356,@321,@230,@215,@220,@362,@355
 dc.b @011,@011,@211,@211,@111,@111,@311,@311
 dc.b @014,@014,@214,@214,@114,@114,@314,@314
 dc.b @012,@012,@212,@212,@112,@112,@312,@312
 dc.b @017,@017,@217,@217,@117,@106,@317,@317
 dc.b @031,@031,@231,@231,@131,@131,@331,@331
 dc.b @001,@201,@101,@301,@003,@203,@103,@303
 dc.b @004,@204,@104,@304,@000,@200,@100,@300
 dc.b @005,@205,@105,@305,@116,@153,@110,@072

*QLRAWEND:
   ENDGEN
QLRAWKEY:
 dc.b 149,164,148,147,166,180,156,158
 dc.b 162,43,8,154,160,150,190,175
 dc.b 165,179,172,155,182,183,188,169
 dc.b 187,174,145,11,205,208,213,203
 dc.b 14,99,87,97,70,66,71,23
 dc.b 104,117,112,93,63,45,18,61
 dc.b 53,35,49,33,6,2,50,7
 dc.b 48,40,95,31,127,29,82,125
 dc.b 113,100,84,83,102,116,92,94
 dc.b 98,106,103,90,96,86,126,111
 dc.b 101,115,108,91,118,119,124,105
 dc.b 123,110,81,24,13,16,114,109
 dc.b 21,36,20,19,38,52,28,30
 dc.b 34,42,39,26,32,22,62,47
 dc.b 37,51,44,27,54,55,60,41
 dc.b 59,46,17,88,77,80,85,75
 dc.b 139,227,215,225,198,194,199,151
 dc.b 232,245,240,221,191,173,146,189
 dc.b 181,163,177,161,134,130,178,135
 dc.b 176,168,223,159,255,157,210,253
 dc.b 241,228,212,211,230,244,220,222
 dc.b 226,234,231,218,224,214,254,239
 dc.b 229,243,236,219,246,247,252,233
 dc.b 251,238,209,152,141,144,242,237
 dc.b 9,9,137,137,73,73,201,201
 dc.b 12,12,140,140,76,76,204,204
 dc.b 10,10,138,138,74,74,202,202
 dc.b 15,15,143,143,79,70,207,207
 dc.b 25,25,153,153,89,89,217,217
 dc.b 1,129,65,193,3,131,67,195
 dc.b 4,132,68,196,0,128,64,192
 dc.b 5,133,69,197,78,107,72,58

QLRAWEND:


;**************************************************************
;* code verbatim from CLSC/SRC/ISA/804Xa_asm
;*******************************************************************
;*
;* KBD_asm - Keyboard routines
;*	 - for hardware that is compatible with 804X driver
;*	 - originated July 98 - Mark Swift
;*	 - last modified 22/09/99 (MSW)

AWKCOD	EQU	74		; awkward key that doesn't fit
AWKASC	EQU	'/'		; into scheme if NUMLOCK is on

*******************************************************************
*
*  Subroutine to decode raw keyboard value

KEY_decode:

; --------------------------------------------------------------
; first test for SHIFT, CTRL, ALT, etc...

	cmp.b	#224,d0		; modify next keycode?
	beq.s	KEY_mSTO

	cmp.b	#225,d0		; modify next keycode?
	bne.s	KEY_rTST

KEY_mSTO
	move.b	d0,VAR.MODflg(a3)
	bra	KEY_none

KEY_rTST:
	cmp.b	#240,d0		; key release?
	bne.s	KEY_sTST

	move.b	d0,VAR.RLSflg(a3)
	bra.s	KEY_none

KEY_sTST:
	cmp.b	#18,d0		; left shift?
	beq.s	KEY_sDO

	cmp.b	#89,d0		; right shift?
	bne.s	KEY_cTST

KEY_sDO:
	tst.b	VAR.RLSflg(a3)
	beq.s	KEY_sSTO

	moveq	#0,d0

KEY_sSTO
	move.b	d0,VAR.SHFflg(a3)
	bra.s	KEY_done

KEY_cTST:
	cmp.b	#20,d0		; control?
	bne.s	KEY_aTST

	tst.b	VAR.RLSflg(a3)
	beq.s	KEY_cSTO

	clr.b	VAR.MODflg(a3)	; clear the weird flag
	moveq	#0,d0

KEY_cSTO
	move.b	d0,VAR.CTLflg(a3)
	bra.s	KEY_done

KEY_aTST:
	cmp.b	#17,d0		; alt?
	bne.s	KEY_nTST

	tst.b	VAR.RLSflg(a3)
	beq.s	KEY_aSTO

KEY_aCLR
	clr.b	VAR.MODflg(a3)	; clear the weird flag
	clr.b	VAR.GFXflg(a3)	; clear ALT-Gr flag
	moveq	#0,d0

KEY_aSTO
	move.b	d0,VAR.ALTflg(a3)

	tst.b	VAR.MODflg(a3)	; test the weird flag
	beq.s	KEY_done

	move.b	d0,VAR.GFXflg(a3) ; possible ALT-Gr character
	bra.s	KEY_done

KEY_nTST:
	cmp.b	#119,d0		; NUMLOCK?
	bne.s	KEY_doKEY

	tst.b	VAR.RLSflg(a3)
	bne.s	KEY_done

	not.b	VAR.NLKflg(a3)	; set NUMLOCK flag
*	bsr	HW_DO_LEDS
	bra.s	KEY_done

KEY_doKEY:
	move.b	d0,VAR.ACTkey(a3) ; store keycode
	bra.s	KEY_exit

KEY_done:
	clr.b	VAR.MODflg(a3)	; clear the weird flag
	clr.b	VAR.RLSflg(a3)	; and the release flag

KEY_none:
	clr.b	VAR.ACTkey(a3)	; clear the ACTUAL keycode
	clr.b	VAR.ASCkey(a3)	; clear the ASCII keycode

KEY_exit:
	rts

******************************************************************
* VERBATIM from CLSC/SRC/ISA/804Xd_asm
*
* KBD_asm - German language keyboard tables
*	 - for hardware that is compatible with 804X driver
*	 - originated July 98 - Mark Swift
*	 - last modified 22/09/99 (MSW)


*******************************************************************
*
*  conversion tables for translating keycode to ASCII

LNG_MODULE:

 DC.W 1		; keyboard table
 DC.W 0		; no group
 DC.W 49 	; language number (german)
 DC.W 0		; relative ptr to next module or 0
 DC.W LNG_KBD-*	; relative ptr to keyboard table

LNG_KBD:

 DC.W 49 	; language (german)
 DC.W LNG_KTAB-*	; relative ptr to key table
 DC.W 0		; relative ptr to non-spacing char table

LNG_KTAB:
 DC.B 0,246,0,248,240,232,236,0,0,250,242,234,244,9,94,0
 DC.B 0,0,0,0,0,'q','1',0,0,0,'y','s','a','w','2',0
 DC.B 0,'c','x','d','e','4','3',0,0,' ','v','f','t','r','5',0
 DC.B 0,'n','b','h','g','z','6',0,0,0,'m','j','u','7','8',0
 DC.B 0,',','k','i','o','0','9',0,0,'.','-','l',132,'p',156,0
 DC.B 0,0,128,0,135,39,0,0,224,0,10,'+',0,35,0,0
 DC.B 0,'<',0,0,0,0,194,0,0,204,0,192,196,0,0,0
 DC.B 0,202,216,0,200,208,27,0,0,'+',220,'-','*',212,249,0
 DC.B 0,0,0,238

LNG_KTAB_CT:
 DC.B 0,247,0,249,241,233,237,0,0,251,243,235,245,0,0,0
 DC.B 0,0,0,0,0,17,145,0,0,0,25,19,1,23,146,0
 DC.B 0,3,24,4,5,148,147,0,0,32,22,6,20,18,149,0
 DC.B 0,14,2,8,7,26,150,0,0,0,13,10,21,151,152,0
 DC.B 0,140,11,9,15,144,153,0,0,142,141,12,0,16,141,0
 DC.B 0,0,0,0,0,157,0,0,226,0,0,139,0,131,0,0
 DC.B 0,156,0,0,0,0,0,0,0,206,0,194,198,0,0,0
 DC.B 0,202,218,0,202,210,128,0,0,139,222,141,138,214,0,0
 DC.B 0,0,0,239

LNG_KTAB_SH:
 DC.B 0,0,0,250,242,234,238,0,0,0,0,0,246,253,186,0
 DC.B 0,0,0,0,0,'Q','!',0,0,0,'Y','S','A','W',34,0
 DC.B 0,'C','X','D','E','$',182,0,0,252,'V','F','T','R','%',0
 DC.B 0,'N','B','H','G','Z','&',0,0,0,'M','J','U','/','(',0
 DC.B 0,';','K','I','O','=',')',0,0,':','_','L',164,'P','?',0
 DC.B 0,0,160,0,167,159,0,0,228,0,254,'*',0,39,0,0
 DC.B 0,'>',0,0,0,0,198,0,0,'1',0,'4','7',0,0,0
 DC.B '0','.','2','5','6','8',127,0,0,'+','3','-','*','9',250,0
 DC.B 0,0,0,0

LNG_KTAB_SC:
 DC.B 0,0,0,251,243,235,239,0,0,0,0,0,247,0,0,0
 DC.B 0,0,0,0,0,177,129,0,0,0,185,179,161,183,130,0
 DC.B 0,163,184,164,165,132,0,0,0,32,96,166,180,178,133,0
 DC.B 0,174,162,168,167,186,190,0,0,0,173,170,181,134,138,0
 DC.B 0,155,171,169,175,137,136,0,0,154,191,172,0,176,191,0
 DC.B 0,0,0,0,0,139,0,0,230,0,0,138,0,135,0,0
 DC.B 0,158,0,0,0,0,0,0,0,145,0,148,151,0,0,0
 DC.B 144,142,146,149,150,152,31,0,0,139,147,141,138,153,0,0
 DC.B 0,0,0,0

LNG_KTAB_GR:
 DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DC.B 0,0,0,0,0,'@',0,0,0,0,0,0,0,0,0,0
 DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DC.B 0,0,0,0,0,0,0,0,0,0,176,0,0,'{',91,0
 DC.B 0,0,0,0,0,'}',']',0,0,0,0,0,0,0,'\',0
 DC.B 0,0,0,0,0,0,0,0,0,0,0,126,0,0,0,0
 DC.B 0,'|',0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DC.B 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DC.B 0,0,0,0

LNG_NSTAB:

KTB_OFFS_CT EQU	(LNG_KTAB_CT-LNG_KTAB)
KTB_OFFS_SH EQU	(LNG_KTAB_SH-LNG_KTAB)
KTB_OFFS_GR EQU	(LNG_KTAB_GR-LNG_KTAB)

******************************************************************		
	
; --------------------------------------------------------------
;  Handle key event - response to a keyboard interrupt
*  called inside poll routine	

RDKEYB:
	movem.l	d0/d1/a3/a4/a6,-(a7)

; read keyboard
	btst.b	#0,PS2_STATUS
	beq.s	RDKEYBX		; exit - should in fact do key repeat proc
kbl	move.b	KEY_CODE,d0
	move.b	#1,KEY_UNLOCK
*	trap	#12
		
	bsr	KEY_decode
		
	tst.b	VAR.ACTkey(a3)
	bne.s	RDKEYB0		; branch if alpha-char

	bsr	KR_DOIT		; else keyrow for SHF/CTL/ALT
	bra.s	RDKEYBXL	; ...and next/exit

RDKEYB0:
	bsr	KEY_conv 	; convert to ASCII

	tst.b	VAR.ASCkey(a3)
	bne.s	RDKEYB1		; branch if valid key-stroke

	bsr	KR_DOIT		; else keyrow for SHF/CTL/ALT
	bra.s	RDKEYB3

RDKEYB1:
	tst.b	VAR.RLSflg(a3)
	beq.s	RDKEYB2		; branch if key-down event

	bsr	KR_REMV		; remove key from key-down-list
	bra.s	RDKEYB3

RDKEYB2:
	bsr	KR_ENTR		; enter key into key-down-list # keyrow???
	clr.w	sv_arcnt(a6)		; disable key repeat
;* this is the polled int	
;*	tst.b	VAR.ALTflg(a3)	; if part of ALT combination
;*	bne.s	RDKEYBX		; exit now & let polled int
				; put key into Q

;*	bsr	POLL_K		; otherwise put into Q
	tst.b	VAR.ALTflg(a3)
	sne.b	d1
	ror.w	#8,d1
	move.b	VAR.ASCkey(a3),d1
	ror.w	#8,d1
	cmp.w	sv_arbuf(a6),d1
	beq.s	RDKEYBXL	; ignore HW key repeat, want own
;*	trap	#12
	bsr.s	q68kbinch
;*	trap	#12
	bra.s	RDKEYBXL

RDKEYB3:
	clr.b	VAR.RLSflg(a3)	; clear the release flag
	clr.b	VAR.ASCkey(a3)	; clear the ASCII keycode
;RDKEYB4:			unused label
	clr.b	VAR.ACTkey(a3)	; clear the ACTUAL keycode
	CLR.W	sv_arbuf(A6)	; reset Autorepeat buffer

RDKEYBXL:
	btst.b	#0,PS2_STATUS	;  more chars to read?
	bne.s	kbl
RDKEYBX:	
	movem.l	(a7)+,d0/d1/a3/a4/a6
	rts

; key ind d1.w, check for special keys, insert into keyq
; unlike Minerva d1 is always word=code:8,ALT:8
q68kbinch:		
	MOVEA.L	$4C(A6),A2	; SV.KEYQ
	cmp.w	sv_cqch(a6),d1  ***cant work, swapped
;**	beq.s	ctlc	*** needs adapting
	sf	sv_scrst(a6)	  unfreeze screen
; tests for special cases
	cmpi.w	#$E000,d1	; CAPS?
;**	beq	caps    *** needs fixing
	cmpi.w	#$F900,d1	; <CTL><F5>? (scroll lock)
	beq.s	frez
; 
	move.w	d1,sv_arbuf(a6) store char in the autorepeat buffer
	
	cmp.b	#255,d1   is it a two-byte code?
	bne.s	in1
	move.w	$de,a3		;  io.qtest
	jsr	(a3)	  how many bytes are left? (nb only d1.b zapped)
	subq.w	#2,d2	  are they enough?
	blt.s	autorld   no, don't put the character in
	st	d1		  reset the alt code
	
	bsr.s	ins2	  put the escape in the queue
in1	lsr.w	#8,d1	  get the second code

ins2	move.w	$e0,a3		; io.qin
	jsr	(a3)	  put it in the queue and return
autorld
	move.w	sv_ardel(a6),sv_arcnt(a6) reload the auto-rept counter
	rts

frez:
	not.b	sv_scrst(a6)	  toggle freeze flag
rts0
	rts

caps:
	not.b	sv_caps(a6)	  toggle caps lock flag byte
	lea	sv_csub(a6),a4	get capslock user routine address
isprog
	tst.l	(a4)	  is there some code there?
	beq.s	rts0	  no - not a good idea to call it...
	jmp	(a4)	  yes, call it and get out


* a2 keyboard q addr	
*ctlc
*	move.l	a0,-(sp)	  save a0
*	lea	-sd_end(a2),a0	find start of io definition block
*	tst.b	sd_curf(a0)	  should cursor in old wdw be visible?
*	bge.s	switch_q
*	jsr	sd_cure(pc)	  ensure cursor in old window is visible
*	lea	sd_end(a0),a2	restore a2
*switch_q
*	move.l	(a2),a2   switch to next queue
*	cmp.l	sv_keyq(a6),a2	is this the original queue?
*	beq.s	end_swit
*	tst.b	sd_curf-sd_end(a2) is this cursor active?
*	beq.s	switch_q	  no...
*	move.l	a2,sv_keyq(a6)	set new key queue pointer
*	clr.w	sv_fstat(a6)	  reset cursor flash cycle
*end_swit
*	move.l	sd_scrb-sd_end(a2),d1 have a look at the screen base here
*	add.w	d1,d1	  does it end with $0000 or $8000?
*	bne.s	offscr	  no - forget it
*	roxr.b	#1,d1
*	add.b	sv_mcsta(a6),d1 are we already on the indicated screen?
*	bpl.s	offscr	  yes - forget it
*	swap	d1
*	subq.b	#2,d1	  is it $xx020000 or $xx028000?
*	bne.s	offscr	  no - forget it
*****	bsr.s	ctlt	  switch over to that screen
*	move.b	d0,sv_mcsta(a6) and say that's what we're on
*offscr
*	move.l	(sp)+,a0	  restore a0
*	rts
	
; --------------------------------------------------------------
;  convert key-stroke to ASCII

KEY_conv:
	movem.l	d0/a0,-(a7)

	clr.b	VAR.ASCkey(a3)	; clear the ASCII keycode

	moveq	#0,d0
	move.b	VAR.ACTkey(a3),d0 ; get keycode key
	beq	KEY_convX	; exit if not alpha key

	cmpi.l	#KTB_OFFS_CT,d0
	bcc	KEY_convX	; exit if out-of-bounds

	tst.b	VAR.RLSflg(a3)
	bne.s	KEY_conv0	; skip if a key-up event

; check for special-action non-ascii key-combinations

	tst.b	VAR.CTLflg(a3)
	sne.b	d1
	lsl.l	#8,d1

	tst.b	VAR.SHFflg(a3)
	sne.b	d1
	lsl.l	#8,d1

	tst.b	VAR.ALTflg(a3)
	sne.b	d1
	lsl.l	#8,d1

	move.l	VAR.KEYtab(a3),a0 ; KEYtab defaults
	move.b	0(a0,d0.w),d1	; get "unshifted" ASCII value

	cmpi.l	#$FF000020,d1	; try <CTL><SPC>
; not sure if a4 is setup at this point, make it by hand and use a0	
        move.l  sv_chtop(a6),a0 
	beq	DO_BREAK
	
*	cmpi.l	#$FF000009,d1	; try <CTL><TAB>
*	beq	DO_FLIP
* needs to do ctlt safely..	

*	cmpi.l	#$FFFFFF09,d1	; try <CTL><SHF><ALT><TAB>
*	beq	DO_RESET

; --------------------------------------------------------------
; convert to ASCII

KEY_conv0:
	tst.b	VAR.GFXflg(a3)	; try gfx
	beq.s	KEY_conv1

	move.l	VAR.KEYtab(a3),a0 ; KEYtab defaults
	lea	KTB_OFFS_GR(a0),a0 ; adjust for ALT-Gr chars

	moveq	#0,d0
	move.b	VAR.ACTkey(a3),d0 ; get keycode key
	move.b	0(a0,d0.w),d0	; convert to ASCII value
	bne.s	KEY_conv6	; branch if an OK char

	clr.b	VAR.GFXflg(a3)

KEY_conv1
	move.l	VAR.KEYtab(a3),a0 ; KEYtab defaults

	tst.b	VAR.CTLflg(a3)	; try control
	beq.s	KEY_conv2

	lea	KTB_OFFS_CT(a0),a0 ; adjust for control chars

KEY_conv2:
	tst.b	VAR.MODflg(a3)	; test the weird flag
	beq.s	KEY_conv2a	; nope...

	cmpi.l	#AWKCOD,d0	; the weird awkward key?
	bne.s	KEY_conv5	; nope... ignore shift & numlock

	move.l	#AWKASC,d0	; be specific with awkward key
	bra.s	KEY_conv8

KEY_conv2a:
	moveq	#0,d0
	move.b	VAR.ACTkey(a3),d0 ; get keycode key
	lea	KTB_OFFS_SH(a0),a0 ; pre-adjust for shifted chars
	move.b	0(a0,d0.w),d0	; convert to ASCII value

	tst.b	VAR.SHFflg(a3)
	sne.b	d1

	cmpi.b	#'.',d0
	beq.s	KEY_conv3	; numeric

	cmpi.b	#'0',d0
	blt.s	KEY_conv4	; not numeric

	cmpi.b	#'9',d0
	bgt.s	KEY_conv4	; not numeric

KEY_conv3:
	tst.b	VAR.NLKflg(a3)	; try numlock
	beq.s	KEY_conv4	; nope...

	not.b	d1

KEY_conv4:
	tst.b	d1
	bne.s	KEY_conv5

	suba.l	#KTB_OFFS_SH,a0	; unadjust for shifted chars

KEY_conv5:
	moveq	#0,d0
	move.b	VAR.ACTkey(a3),d0 ; get keycode key
	move.b	0(a0,d0.w),d0	; convert to ASCII value

KEY_conv6:
	tst.b	SV_CAPS(a6)	; check for CAPS lock
	beq.s	KEY_conv8

	cmp.b	#'a',d0		; check for lower case
	blt.s	KEY_conv7

	cmp.b	#'z',d0
	bgt.s	KEY_conv7

	sub.b	#32,d0		; change to upper case
	bra.s	KEY_conv8

KEY_conv7:
	cmp.b	#128,d0		; check lower case accented
	blt.s	KEY_conv8

	cmp.b	#139,d0
	bgt.s	KEY_conv8

	add.b	#32,d0		; change to upper case

KEY_conv8:
	tst.b	VAR.ALTflg(a3)	; check alt flag
	beq.s	KEY_conv9

	cmpi.b	#$C0,d0		; test for cursor/caps keys
	blt.s	KEY_conv9

	cmpi.b	#$e8,d0		; test for cursor/caps keys
	bge.s	KEY_conv9

	andi.b	#$FE,d0
	add.b	#$01,d0

KEY_conv9:
	move.b	d0,VAR.ASCkey(a3) ; store new key

KEY_convA:
	clr.b	VAR.MODflg(a3)	; clear the weird flag

KEY_convX:
	movem.l	(a7)+,d0/a0
	rts

; --------------------------------------------------------------
;  enter key into keydown list

KR_ENTR:
	movem.l	d0-d3/a0-a1,-(a7)

	move.b	VAR.ACTkey(a3),d1

	moveq	#0,d0
	move.b	VAR.KEYdwc(a3),d0
	beq.s	KR_EADD

	cmpi.b	#16,d0
	beq.s	KR_EXIT

	lea	VAR.KEYdwk(a3,d0.w),a0
	bra.s	KR_EBEG

KR_ELUP:
	cmp.b	-(a0),d1
	beq.s	KR_EXIT		; exit if already in list

KR_EBEG
	dbra	d0,KR_ELUP

KR_EADD:
	moveq	#0,d0
	move.b	VAR.KEYdwc(a3),d0
	move.b	d1,VAR.KEYdwk(a3,d0.w)	; put in list
	move.b	VAR.ASCkey(a3),d1
	move.b	d1,VAR.KEYdwa(a3,d0.w)	; put in list
	addi.b	#1,VAR.KEYdwc(a3) 	; increment count

	bsr.s	KR_DOIT

KR_EXIT:
	movem.l	(a7)+,d0-d3/a0-a1
	rts

; --------------------------------------------------------------
;  remove key from keydown list

KR_REMV:
	movem.l	d0-d3/a0-a1,-(a7)

	move.b	VAR.ACTkey(a3),d1

	moveq	#0,d0
	move.b	VAR.KEYdwc(a3),d0
	beq.s	KR_RXIT

	lea	VAR.KEYdwk(a3,d0.w),a0
	bra.s	KR_RBEG

KR_RLUP:
	cmp.b	-(a0),d1
	beq.s	KR_RDEL			; found entry

KR_RBEG
	dbra	d0,KR_RLUP
	bra.s	KR_RXIT

KR_RDEL:
	subi.b	#1,VAR.KEYdwc(a3) 	; decrement count
	moveq	#0,d0
	move.b	VAR.KEYdwc(a3),d0
	move.b	VAR.KEYdwk(a3,d0.w),(a0)	; move last entry
* gwass does not like this, use workaround	
*	move.b	VAR.KEYdwa(a3,d0.w),(VAR.KEYdwa-VAR.KEYdwk)(a0)
	move.b	VAR.KEYdwa(a3,d0.w),vdwak(a0)
	clr.b	VAR.KEYdwk(a3,d0.w)	; delete last entry
	clr.b	VAR.KEYdwa(a3,d0.w)

	bsr.s	KR_DOIT

KR_RXIT:
	movem.l	(a7)+,d0-d3/a0-a1
	rts

; --------------------------------------------------------------
;  set keyrow for all keys in keydown list

KR_DOIT:
	movem.l	d0-d3/a0-a1,-(a7)

	lea	VAR.KEYraw(a3),a1
	clr.l	(a1)+		; clear KEYROW entries
	clr.l	(a1)+

	moveq	#0,d0
	move.b	VAR.KEYdwc(a3),d0
	beq.s	KEY_KR1

	lea	VAR.KEYdwa(a3,d0.w),a0
	bra.s	KR_DBEG

KR_DLUP:
	moveq	#0,d1
	move.b	-(a0),d1

	lea	QLRAWKEY(pc),a1
	moveq	#0,d2
	move.b	0(a1,d1.w),d2	; get row and bit number

	move.l	d2,d1		; save for later

	move.l	d2,d3
	lsr.l	#3,d3		; extract row number -> D3
	and.w	#$7,d3
	and.b	#$07,d2		; extract bit number -> D2

	lea	VAR.KEYraw(a3),a1
	bset	d2,0(a1,d3.w)	; set the bit in KEYROW

	lsr.b	#6,d1		; set SHFT/CTL from table
	move.b	VAR.KEYraw+7(a3),d3
	andi.b	#$F8,d3
	or.b	d3,d1
	move.b	d1,VAR.KEYraw+7(a3)

KR_DBEG
	dbra	d0,KR_DLUP

	bra.s	KEY_KR3		; set ALT from flag value

; if keydown list empty set KEYROW for SHF/CTL/ALT keys from flags

KEY_KR1:
	move.b	VAR.KEYraw+7(a3),d1
	andi.b	#$F8,d1

	tst.b	VAR.SHFflg(a3)
	beq.s	KEY_KR2

	bset	#0,d1

KEY_KR2:
	tst.b	VAR.CTLflg(a3)
	beq.s	KEY_KR3

	bset	#1,d1

KEY_KR3:
	tst.b	VAR.ALTflg(a3)
	beq.s	KEY_KR4

	bset	#2,d1

KEY_KR4:
	move.b	d1,VAR.KEYraw+7(a3)

KR_DXIT:
	movem.l	(a7)+,d0-d3/a0-a1
	rts

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DO_BREAK:
* Minerva specific.... 
	bset	#4,sx_event(a0)
	CLR.B	VAR.ASCkey(A3)	; reset key event
	bra	KEY_convA

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;  Polled interrupt routine to read the keyboard
;  enters with a6=sys vars, a3=our (keyboard) vars

POLL_SERver:
        move.l  sv_keyq(a6),d0   fetch ptr to current keyboard queue
        beq.s   POLL_EXIT        no queue, don't bother reading the IPC 
	bsr	RDKEYB		;  read keys, translate, stuff to q
POLL_EXIT:
	rts

   ENDGEN

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;  BASIC extensions not fully implemented yet

    GENIF 0 <> 0
PROC_DEF:
	dc.w	2
	dc.w	B_KEYDT-*
	dc.b	5,'KEYDT'
	dc.w	B_KEYDT-*
	dc.b	5,'KEZDT'
	dc.w	0

	dc.w	0
	dc.w	0

;  BASIC proc to link in German keymap again, should it become
;  dislocated for some reason.

B_KEYDT:

	movem.l	a0-a4/a6,-(a7)

	bsr	FIND_DRV	;  return addr in a3
	bne.s	B_KEYDTX 	; exit if THING not found

	lea	LNG_KTAB(pc),a0
	move.l	a0,VAR.KEYtab(a3)

	moveq	#0,d0

B_KEYDTX:
	movem.l	(a7)+,a0-a4/a6
	rts

FIND_DRV
	moveq	#MT.INF,d0	; get sys vars
	trap	#1
	move.l	a0,a6
	lea	sv_pollst(a0),a3  poll list
        lea     POLL_SERVER,a0    this is our routine
fnd_lp
        move.l  (a3),d1         end of list?
        beq.s   err_bp
        move.l  d1,a3
        cmp.l   4(a3),a0        is this entry us?
        bne.s   fnd_lp          no - keep looking
	adjust a3???
	moveq	#ERR.OK,d0
        rts

err_bp:	
	moveq	#ERR.NF,d0
	rts		

			
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;  Handle KEYROW command: TRAP #1 with D0=$11, 0(a3)=9
IPC_KROW:
	lea	VCTR_BEG-THNG.VCTR(pc),a3

	MOVE.B	VAR.KEYraw(A3,D7.W),D1

IPCOM_EX:
	MOVEM.L	(A7)+,D7/A3

	moveq	#0,d0		; no errors

	somehow end

	
   ENDGEN

*** get key codes from serial, never fully implemented ***	
    GENIF 0 <> 0

q68_skeyb
	nop			; patch rts when not wanted
*    while (UART_STATUS & 0x02); UART_STATUS & 0x02 -> no char available"""""
*    c = UART_RXDATA;
*    put keycode c into keyqueue
        move.l  sv_keyq(a6),d0  fetch ptr to current keyboard queue
	move.l	d0,a2
        beq.s   kbdone	  no queue, don't bother reading the IPC
tkbd
	btst	#1,UART_STATUS
	bne	kbdone
* read char from serial line, almost ASCII, see xqlkey.c:iso2ql for complete
* translation table
	move.b	UART_RXDATA,d1
	clr	d2		  here should go alt/ctrl/shift
        jsr     ip_kbrd(pc)	  go process it
	bra 	tkbd
	clr.l	d5		  no key repeat
	jsr     ip_kbend(pc)	  go finish off autorepeat setting
kbdone
	rts

    ENDGEN

	
********************************
*** CODE EXAMPLES/FRAGMENTS BELLOW

    GENIF 0 <> 0
* Main keyboard read routine. Can be called by replacement keyboard code.

* d1 -i  - keyrow data in 6 lsb
* d2 -i  - ctrl/shift/alt in 3 lsb
* a2 -ip - current keyboard queue address
* d0/d6/a0-a1/a3-a4 destroyed

* sx_kbenc is predefind to tb/kbenc.asm (extvar defs def in ss/ext.asm)

ip_kbrd
	clr.w	sv_arbuf(a6)	  get rid of previous auto repeat
	move.l	sv_chtop(a6),a4 get sysvar extension
	move.l	sx_kbenc(a4),a3 get encoder address
	jsr	(a3)	  call encoder via vector table
	bra.s	special   special processing return
	bra.s	kb_inch   put it into the queue
	rts		  ignore char return

* Routine to finish off keyboard read and handle auto-repeat.

* d3 -ip - number of polls missed
* d5 -i  - bit 4 set if last key is held down
* a2 -ip - current keyboard queue
* d0-d2/a3 destroyed

ip_kbend
	asr.b	#4,d5	  see if the key is still held down
	bcc.s	autorld   if not, reload the counter and get out
	sub.w	d3,sv_arcnt(a6) step counter by the number of polls
	bgt.s	rts0	  if it hasn't timed out yet
	jsr	io_qtest(pc)	  is the queue empty?
	beq.s	shortd	  no, so don't allow autorepeat
	move.w	sv_arbuf(a6),d1 retrieve char to be repeated
	beq.s	shortd	  nothing
	bsr.s	kb_inch   put it into the queue
shortd
	move.w	sv_arfrq(a6),sv_arcnt(a6) reload with the shorter delay
	rts

* Special processing routines
spent	macro	t
	dc.b	(\1-spend)&$7fffffff
	endm

sptab
*		    ctrl/space		event 4 = break job 0
*		    ctrl/alt/space		event 5 = break multi
*		    shift/ctrl/space	    event 6 = user event 0
*		    shift/ctrl/alt/space    event 7 = user event 1
	spent	ctlt	ctrl/tab		alternate screen
	spent	frez	ctrl/alt/tab		toggle display freeze
	spent	rts0	shift/ctrl/tab		reserve... easy to key!
	spent	soft	shift/ctrl/alt/tab	soft reset
	spent	comp	ctrl/enter		compose
	spent	ctlc	ctrl/alt/enter		cursor to next queue
	spent	caps	shift/ctrl/enter	caps lock
	spent	user	shift/ctrl/alt/enter	call user subroutine

special
	subq.b	#4,d1
	bpl.s	usetab
	bchg	d1,sx_event(a4)
	rts

usetab
	move.b	sptab(pc,d1.w),d1
	jmp	spend(pc,d1.w)

* Put a char in the queue
kb_inch
	cmp.w	sv_cqch(a6),d1
	beq.s	ctlc
	sf	sv_scrst(a6)	  unfreeze screen
	move.w	d1,sv_arbuf(a6) store char in the autorepeat buffer
	cmp.b	#255,d1   is it a two-byte code?
	bne.s	in1
	jsr	io_qtest(pc)	  how many bytes are left? (nb only d1.b zapped)
	subq.w	#2,d2	  are they enough?
	blt.s	autorld   no, don't put the character in
	st	d1		  reset the alt code
	bsr.s	in1	  put the escape in the queue
	lsr.w	#8,d1	  get the second code
in1
	jsr	io_qin(pc)	  put it in the queue and return
autorld
	move.w	sv_ardel(a6),sv_arcnt(a6) reload the auto-rept counter
	rts

frez
	not.b	sv_scrst(a6)	  toggle freeze flag
rts0
	rts

caps
	not.b	sv_caps(a6)	  toggle caps lock flag byte
	lea	sv_csub(a6),a4	get capslock user routine address
isprog
	tst.l	(a4)	  is there some code there?
	beq.s	rts0	  no - not a good idea to call it...
	jmp	(a4)	  yes, call it and get out
* N.B. changed above to use a4 instead of a5, which wasn't saved!

comp
	addq.b	#1,sv_ichar(a6) set compose start
	rts

user
spend	equ	user-31
;	 assert  0,sx_case
	move.l	(a4),d0   get user code routine address
	bpl.s	rts0	  no flag (bit 31), so no call
	bclr	d1,d0	  don't let msb propagate into pc
	move.l	d0,a4	  if it was set up the way we want (negative) ...
	bra.s	isprog	  ... check there's some code before calling it

ctlt
	bsr.s	ip_dspm   ensure sx_dspm is up to date
	bchg	#7,sx_dspm(a4)	toggle displayed screen
	move.b	sx_dspm(a4),d0	pick up new value
	bmi.s	ctlt1	  top bit set, use screen 1
	add.b	d0,d0	  shift for screen 0
ctlt1
	and.b	#1<<mc..blnk+1<<mc..m256+1<<mc..scrn,d0
	move.b	d0,mc_stat	  set hardware only
	rts

soft
	moveq	#9,d1
	jmp	390	  soft reset, quick

ctlc
	move.l	a0,-(sp)	  save a0
	lea	-sd_end(a2),a0	find start of io definition block
	tst.b	sd_curf(a0)	  should cursor in old wdw be visible?
	bge.s	switch_q
	jsr	sd_cure(pc)	  ensure cursor in old window is visible
	lea	sd_end(a0),a2	restore a2
switch_q
;	 assert  q_nextq,0
	move.l	(a2),a2   switch to next queue
	cmp.l	sv_keyq(a6),a2	is this the original queue?
	beq.s	end_swit
	tst.b	sd_curf-sd_end(a2) is this cursor active?
	beq.s	switch_q	  no...
	move.l	a2,sv_keyq(a6)	set new key queue pointer
	clr.w	sv_fstat(a6)	  reset cursor flash cycle
end_swit
	move.l	sd_scrb-sd_end(a2),d1 have a look at the screen base here
	add.w	d1,d1	  does it end with $0000 or $8000?
	bne.s	offscr	  no - forget it
	roxr.b	#1,d1
	add.b	sv_mcsta(a6),d1 are we already on the indicated screen?
	bpl.s	offscr	  yes - forget it
	swap	d1
	subq.b	#2,d1	  is it $xx020000 or $xx028000?
	bne.s	offscr	  no - forget it
	bsr.s	ctlt	  switch over to that screen
	move.b	d0,sv_mcsta(a6) and say that's what we're on
offscr
	move.l	(sp)+,a0	  restore a0
	rts

* A routine to copy sv_mcsta bits into sx_dspm in case someone poked it!

* a4 -	o- sysvar extension base
* d0-d1 destroyed
ip_dspm
	moveq	#1<<mc..blnk+1<<mc..m256,d1
	move.b	sv_mcsta(a6),d0
	bmi.s	setscr1
	lsr.b	#1,d0
	moveq	#(1<<mc..blnk+1<<mc..m256)>>1,d1
setscr1
	and.b	d1,d0
	not.b	d1
	move.l	sv_chtop(a6),a4 get sysvar extension
	and.b	d1,sx_dspm(a4)
	or.b	d0,sx_dspm(a4)
	rts
  ENDGEN	
	

	end
