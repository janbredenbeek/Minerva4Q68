* **********************************
* Q68 serial drivers for Minerva ROM
* **********************************
* Copyright (C) 2023 Jan Bredenbeek
* 
* This program is free software; you can redistribute it and/or
* modify it under the terms of the GNU General Public License
* as published by the Free Software Foundation; either version 2
* of the License, or (at your option) any later version.
* 
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software Foundation,
* Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

* Change history:

* 20231030 JB  v0.1     first version using original access layer code
* 20231114 JB  v0.2     new access layer and configurable buffers
* 20231129 JB  v0.3     changed default name back from SER4 to SER1
* 20231228 JB  v1.0     first release version

version setstr  1.0

        include 'm_inc_assert'
        include 'm_mincf'
        include 'm_inc_bv'
        include 'm_inc_err'
        include 'm_inc_io'
        include 'm_inc_mt'
        include 'm_inc_q'
;        include 'm_inc_ser'
        include 'm_inc_sv'
        include 'm_inc_sx'
        include 'm_inc_q68'
        include 'm_inc_vect'

; offsets from start linkage block (A3)

        offset  $28

sp_io    ds.l   1               ; JSR instruction to io.serio (short jump)
         ds.l   1               ; address of pending i/o routine
         ds.l   1               ; address of fetch byte routine
         ds.l   1               ; address of send byte routine
         ds.w   1               ; RTS instruction
sp_name  ds.l   1               ; name of SER device (default 'SER4')
sp_srx   ds.l   1               ; name of SRX device (default 'SRX4')
sp_stx   ds.l   1               ; name of STX device (default 'STX4')
sp_rxq   ds.l   1               ; pointer to receive queue
sp_txq   ds.l   1               ; pointer to transmit queue
sp_rxch  ds.l   1               ; pointer to channel for receive
sp_txch  ds.l   1               ; pointer to channel for transmit
sp_rxsiz ds.w   1               ; receive queue size
sp_txsiz ds.w   1               ; transmit queue size
xof_thrs ds.w   1               ; (W) free buffer threshold for sending XOFF
xon_thrs ds.w   1               ; (W) free buffer threshold for sending XON
sp_flow  ds.b   1               ; (B)  0: no flow control
                                ;     -1: xon/xoff raw
                                ;     +1: xon/xoff transparent (escaped)
txhold   ds.b   1               ; (B) remote requested to hold
rxhold   ds.b   1               ; (B) we requested remote to hold
rx_dle   ds.b   1               ; received DLE flag (transparent mode)
         ds.w   0
sp_len   equ    *               ; length of physical definition block

; channel definition block

         offset  $18
ser_dir  ds.b   1               ; open for tx (0), rx (+1) or both (-1)
         ds.b   1               ; filler
ser_rxq  equ    *               ; start of rx queue (or none)
                                ; followed by tx queue (if any)
ser_len  equ    *
                                
xon      equ    'Q'-$40         ; xon character itself
xoff     equ    'S'-$40         ; xoff character itself
dle      equ    'P'-$40         ; DLE character
dle_offs equ    $40             ; offset for DLE escape
def_rxsz equ    16384+$10       ; default rx queue size
def_txsz equ    1024+$10        ; default tx queue size
def_rxhw equ    512             ; default for xof_thrs
def_rxlw equ    def_rxsz-512    ; default for xon_thrs


string$ macro   a
        noexpand
[.lab]  dc.w    .e.[.l]-*-2
        dc.b    [a]
.e.[.l] equ     *
        ds.w    0
        endm

; Initialise the serial port
; ==========================
;

	section q68_ser

        xdef     ser_init

initreg reg     d1-d3/a0-a3

ser_init
        movem.l initreg,-(sp)
        moveq   #sp_len,d1
        moveq   #0,d2
        moveq   #mt.alchp,d0
        trap    #1
        tst.l   d0
        bne     init_end
        lea     ser_xint,a1
        move.l  a1,sv_axint(a0)
        lea     ser_schd,a1
        move.l  a1,sv_aschd(a0)
        lea     dev_def,a3              ; set up pointers
        lea     sv_aopen(a0),a2
        bsr.s   getaddr                 ; open routine
        bsr.s   getaddr                 ; close routine
        move.l  a2,sv_aio(a0)           ; physical i/o routine
        move.w  #$4eb8,(a2)+            ; jsr absolute short
        move.w  io.serio,(a2)+          ; to io_serio
        bsr.s   getaddr                 ; pending i/o routine
        bsr.s   getaddr                 ; fetch character
        bsr.s   getaddr                 ; send character
        move.w  #$4e75,(a2)+            ; rts
        move.l  #'SER1',(a2)+           ; set device name
        move.l  #'SRX1',(a2)+
        move.l  #'STX1',(a2)+
        move.l  #def_rxsz<<16+def_txsz,sp_rxsiz(a0)
        move.l  #def_rxhw<<16+def_rxlw,xof_thrs(a0)
        assert  0,sv_lxint
        moveq   #mt.lxint,d0
        trap    #1
        lea     sv_lschd(a0),a0
        moveq   #mt.lschd,d0
        trap    #1
        addq.l  #sv_lio-sv_lschd,a0
        moveq   #mt.liod,d0
        trap    #1
        moveq   #mt.inf,d0
        trap    #1
        move.l  sv_chtop(a0),a0
        lea     sx_serb(a0),a0
        moveq   #mt.riod,d0
        trap    #1                      ; remove old SER driver link
        lea     procs,a1
        move.w  bp.init,a2
        jsr     (a2)
        suba.l  a0,a0
        lea     signon,a1
        move.w  ut.mtext,a2
        jsr     (a2)
init_end
        movem.l (sp)+,initreg
        rts

* calculate and store the absolute addresses required for io_serio

getaddr move.w  (a3)+,a1        ; get relative pointer
        lea     -2(a3,a1.w),a1  ; make address absolute
        move.l  a1,(a2)+        ; and store it
        rts
                    
dev_def dc.w    open-*          ; open routine
        dc.w    close-*         ; close routine
        dc.w    pend-*          ; pending input
        dc.w    fbyte-*         ; fetch character
        dc.w    sbyte-*         ; send char

signon  string$ {'SER driver for Q68 v[version]  JB 2023',10}

procs   dc.w    6               ; 5 procs but allow for long names
        dc.w    ser_use-*
        dc.b    7,'SER_USE'
        dc.w    ser_flow-*
        dc.b    8,'SER_FLOW'
        dc.w    ser_buff-*
        dc.b    8,'SER_BUFF'
        dc.w    ser_room-*
        dc.b    8,'SER_ROOM'
        dc.w    ser_clear-*
        dc.b    9,'SER_CLEAR'
        dc.w    0,0,0

* subroutine to get address of linkage block from BASIC
* Entry: none
* Exit: D0 error code (0 OK, err.nf block not found)
*       A3 points to linkage block

get_lb:
        movem.l d1-d2/a0-a1,-(sp)  ; save these registers
        moveq   #mt.inf,d0
        trap    #1
        lea     sv_i2lst(a0),a3 ; pointer to external interrupt task list
        lea     ser_xint,a1     ; address of our external interrupt routine
glb_loop:
        tst.l   (a3)            ; end of list reached?
        beq.s   glb_notf        ; yes
        move.l  (a3),a3         ; else, get next entry
        cmpa.l  4(a3),a1        ; is this the entry we're looking for?
        bne.s   glb_loop        ; no, loop back
        moveq   #0,d0           ; success
        bra.s   glb_ret
glb_notf:
        moveq   #err.nf,d0      ; oops... block not found!
glb_ret movem.l (sp)+,d1-d2/a0-a1
        rts

* Get a string from S*BASIC (real string or SB name)
* Entry: A3, A5 pointer to parameters
* Exit: A1 ptr to string on RI stack, D0 error code

get_name        
        cmpa.l  a3,a5           ; parameter must be given
        bne.s   gtfnam_1
err_bp
        moveq   #err.bp,d0
        rts
gtfnam_1  
        moveq   #$0f,d0
        and.b   1(a6,a3.l),d0
        subq.b  #1,d0
        bne.s   gtfnam_2
        move.l  a5,-(a7)
        lea     8(a3),a5
        move.w  ca.gtstr,a2
        jsr     (a2)
        move.l  (a7)+,a5
        tst.l   d0
        bne.s   gtfnam_r
        bra.s   gtfnam_e
gtfnam_2  
        moveq   #0,d0
        move.w  2(a6,a3.l),d0
        blt.s   err_bn
        lsl.l   #3,d0
        move.l  bv_ntbas(a6),a0
        adda.l  d0,a0
        move.w  2(a6,a0.l),a0
        adda.l  bv_nlbas(a6),a0
        moveq   #3,d1
        add.b   0(a6,a0.l),d1
        bclr    #0,d1
        move.w  d1,-(a7)
        move.w  bv.chrix,a2
        jsr     (a2)
        move.l  bv_rip(a6),a1
        suba.w  (a7)+,a1
        move.l  a1,a2
        moveq   #0,d1
        move.b  0(a6,a0.l),d1
        move.w  d1,0(a6,a2.l)
gtfnamlp  
        move.b  1(a6,a0.l),2(a6,a2.l)
        addq.w  #1,a0
        addq.w  #1,a2
        subq.b  #1,d1
        bne.s   gtfnamlp
gtfnam_e  
        move.l  a1,bv_rip(a6)
        moveq   #0,d0
gtfnam_r  
        rts
err_bn    
        moveq   #err.bn,d0
        rts

* Set name of SER device (also STX/SRX, but only last character)

ser_use
        bsr     get_name
        bne.s   use_r
        cmpi.w  #4,(a6,a1.l)
        bne     err_bp
        bsr     get_lb
        bne.s   use_r
        move.l  #$dfdfdfff,d1
        and.l   2(a6,a1.l),d1
        move.l  d1,sp_name(a3)  ; set device name
        move.b  d1,sp_srx+3(a3) ; set SRXn/STXn as per last character of name
        move.b  d1,sp_stx+3(a3) ; NB: This assumes supplied name is sensible!
use_r   rts

* Set default flow control type: I none, X XON/XOFF, H XON/XOFF with escape

ser_flow
        bsr     get_name
        bne.s   flow_r
        cmpi.w  #1,(a6,a1.l)
        bne     err_bp
        bsr     get_lb
        bne.s   flow_r
        moveq   #$df-256,d1
        and.b   2(a6,a1.l),d1
        subi.b  #'H',d1
        beq.s   flow_h
        subq.b  #'I'-'H',d1
        beq.s   flow_i
        subi.b  #'X'-'I',d1
        bne     err_bp
        st      sp_flow(a3)
        rts
flow_h  
        move.b  #1,sp_flow(a3)
        rts
flow_i  
        clr.b   sp_flow(a3)
flow_r
        rts

ser_buff
        moveq   #-1,d4          ; pre-set default rx queue size
        cmpa.l  a3,a5
        beq.s   sb_default
        move.w  ca.gtint,a2
        jsr     (a2)
        bne.s   sb_rts
        moveq   #err.bp,d0      ; prepare for sanity check
        subq.w  #2,d3
        bgt.s   sb_rts
        moveq   #$10,d7         ; constant
        move.w  (a6,a1.l),d5    ; new tx queue size
        cmp.w   d7,d5
        blt.s   sb_rts          ; must be 16 or more
        add.w   d7,d5           ; allow for queue header
        bvs.s   sb_rts          ; total must be < 32768
        tst.w   d3              ; rx queue size specified?
        blt.s   sb_default
        move.w  2(a6,a1.l),d4   ; yes, get new rx queue size
        cmp.w   d7,d4
        blt.s   sb_rts          ; must be 16 or more
        add.w   d7,d4
        bvs.s   sb_rts          ; total must be < 32768
sb_default
        bsr     get_lb
        bne.s   sb_rts
        trap    #0
        moveq   #err.iu,d0
        move.l  sp_rxch(a3),d1
        or.l    sp_txch(a3),d1
        bne.s   sb_iu           ; no channels may be open!
        move.w  d5,sp_txsiz(a3) ; set tx queue size
        tst.w   d4              ; rx queue size specified?
        ble.s   sb_ok           ; no, end.
        move.w  d4,sp_rxsiz(a3) ; set it
        move.w  d4,d1
        lsr.w   #2,d1
        move.w  d1,xof_thrs(a3) ; and SER_ROOM to 1/4th
        sub.w   d1,d4
        move.w  d4,xon_thrs(a3) ; XON threshold to 3/4th
sb_ok
        moveq   #0,d0
sb_iu
        andi.w  #$dfff,sr
sb_rts
        rts

* Set amount of free room in rx queue to send XOFF below

ser_room
        move.w  ca.gtint,a2
        jsr     (a2)
        bne.s   sr_rts
        subq.w  #1,d3
        bne     err_bp
        move.w  (a6,a1.l),d1
        bsr     get_lb
        bne.s   sr_rts
        move.w  sp_rxsiz(a3),d2 ; rx queue size
        cmp.w   d2,d1
        bgt     err_bp          ; ser_room must be < queue size
        move.w  d1,xof_thrs(a3) ; set XOFF threshold
        sub.w   d1,d2
        move.w  d2,xon_thrs(a3) ; XON threshold = queue size - ser_room
sr_rts
        rts

* Clear input and output queues
        
ser_clear
        bsr     get_lb
        bne.s   sc_rts
        assert  sp_rxq,sp_txq-4
        lea     sp_rxq(a3),a1
sc_lp
        move.l  (a1)+,d1        ; get queue address or 0
        beq.s   sc_nxt
        move.l  d1,a2           ; clear queue
        move.l  q_nextin(a2),q_nxtout(a2)
sc_nxt
        bchg    #0,d0           ; d0 initially 0
        beq     sc_lp           ; loop 2 times for rx and tx queue
sc_rts
        rts

* Open routine

open        
        cmpi.w  #4,(a0)
        blt.s   notf
        cmpi.w  #6,(a0)         ; check name length, must be 4 to 6
        bgt.s   notf            ; NB: 5th char is handshake, 6th is ignored
        moveq   #-1,d7          ; signal 'open for rx/tx'
        move.l  #$dfdfdfff,d0
        and.l   2(a0),d0        ; convert to uppercase
        cmp.l   sp_name(a3),d0  ; compare against name set by ser_use
        beq.s   ser_ok          ; ok, continue
        moveq   #1,d7
        cmp.l   sp_srx(a3),d0   ; compare against 'SRXn'
        beq.s   ser_ok          ; ok, continue
        moveq   #0,d7           ; signal 'open for tx only'
        cmp.l   sp_stx(a3),d0   ; compare against 'STXn'
        beq.s   ser_ok          ; ok, continue
notf    
        moveq   #err.nf,d0      ; report NF, system will try other drivers
        rts
err_iu
        moveq   #err.iu,d0      ; 'in use'
        rts

; seems one want to open ser1/srx1/stx1

ser_ok
        cmpi.w  #4,(a0)         ; flow specified?
        beq.s   alloc_ch        ; no, use default
        moveq   #0,d6           ; try 'I'
        moveq   #$df-256,d0
        and.b   2+4(a0),d0
        cmpi.b  #'I',d0
        beq.s   set_hs
        moveq   #1,d6           ; try 'H'
        cmpi.b  #'H',d0
        beq.s   set_hs
        moveq   #-1,d6          ; try 'X'
        cmpi.b  #'X',d0
        bne.s   alloc_ch
set_hs
        move.b  d6,sp_flow(a3)  ; set flow control (global!)

* sorry - we do not reuse old channel blocks

alloc_ch
        moveq   #0,d1           ; initialise channel block size
        tst.b   d7              ; opening for tx only?
        beq.s   tx_only         ; yes
        tst.l   sp_rxch(a3)     ; is there already a rx channel?
        bne     err_iu          ; oops... 
        add.w   sp_rxsiz(a3),d1 ; add rx queue size
        tst.b   d7              ; opening rx only
        bgt.s   rx_only         ; yes, skip
tx_only 
        tst.l   sp_txch(a3)     ; is there already a tx channel?
        bne     err_iu          ; oops...
        add.w   sp_txsiz(a3),d1 ; add tx queue size
rx_only
        addi.l  #ser_len,d1     ; add long! just in case for insane queue sizes
        move.l  a3,-(sp)
        move.w  mm.alchp,a3
        jsr     (a3)            ; now allocate the channel block
        move.l  (sp)+,a3
        bne.s   err_rts
        lea     ser_rxq(a0),a2  ; start of queue(s)
        move.b  d7,ser_dir(a0)  ; set direction
        beq.s   alloc_tx        ; skip if tx-only
        move.l  a0,sp_rxch(a3)  ; set ptr to rx chan
        move.w  sp_rxsiz(a3),d1
        bsr.s   alloc_q
        move.l  a2,sp_rxq(a3)   ; set rx queue address
        adda.w  d1,a2           ; tx queue (if any) comes after rx queue
        sf      rx_dle(a3)      ; clear flag
        bset    #q68..rxstat,uart_status ; enable rx interrupt
alloc_tx
        tst.b   d7              ; open for rx only?
        bgt.s   open_ok         ; yes, skip this
        move.l  a0,sp_txch(a3)  ; set ptr to tx chan
        move.w  sp_txsiz(a3),d1
        bsr.s   alloc_q         ; no, allocate
        move.l  a2,sp_txq(a3)   ; set tx queue address
open_ok
        moveq   #0,d0
err_rts
        rts

alloc_q
        movem.l d1/a3,-(sp)
        subi.w  #$10,d1         ; subtract header length
        move.w  io.qset,a3      ; initialise queue
        jsr     (a3)
        movem.l (sp)+,d1/a3
        rts
        
close
        tst.b   ser_dir(a0)     ; opened tx only?
        beq.s   cl_tx           ; yes
        bclr    #q68..rxstat,uart_status ; clear rx interrupt
        clr.l   sp_rxch(a3)     ; clear input channel
        clr.l   sp_rxq(a3)      ; clear rx queue pointer
        tst.b   ser_dir(a0)     ; rx only?
        ble.s   cl_tx           ; no, set tx queue EOF
        move.w  mm.rechp,a2
        jmp     (a2)            ; exit and clean up channel block
cl_tx
        move.l  sp_txq(a3),a2
        move.w  io.qeof,a1      ; set tx queue to EOF
        jsr     (a1)            ; (scheduler task will clean it up later)
        moveq   #0,d0
        rts

; check for pending input

pend
        tst.b   ser_dir(a0)     ; transmit only?
        beq.s   err_ef          ; yes, return EOF
        move.l  sp_rxq(a3),a2
        move.l  a3,-(sp)
        move.w  io.qtest,a3
        jsr     (a3)
        move.l  (sp)+,a3
        rts
err_ef
        moveq   #err.ef,d0
        rts

; fetch a byte

fbyte
        tst.b   ser_dir(a0)     ; transmit only?
        beq.s   err_ef          ; yes, return EOF
        move.l  sp_rxq(a3),a2
        move.l  a3,-(sp)
        move.w  io.qout,a3
        jsr     (a3)
        move.l  (sp)+,a3
        bne.s   fbyt_rts
        tst.b   sp_flow(a3)     ; any flow control?
        ble.s   fbyt_rts        ; no, unless transparent
        cmpi.b  #DLE,d1         ; received DLE?
        beq.s   got_dle         ; yes
        tst.b   rx_dle(a3)      ; previous was DLE?
        beq.s   fbyt_rts        ; no
        sf      rx_dle(a3)      ; reset flag
        subi.b  #dle_offs,d1    ; subtract DLE offset
fbyt_rts
        rts

; use DLE to escape XON/XOFF in data (and DLE itself!)
got_dle
        st      rx_dle(a3)      ; set flag to signal next byte is escaped
        moveq   #err.nc,d0      ; we've got no byte so far!
        rts

; send a byte

sbyte   tst.b   ser_dir(a0)     ; is channel opened for transmit?
        bgt.s   err_ro          ; no, return 'read only'
        move.l  sp_txq(a3),a2   ; get tx queue
        move.l  a3,-(sp)        ; save A3 (smashed by queue routines!)
        tst.b   sp_flow(a3)     ; flow control 'transparent'?
        ble.s   sb_put          ; no, go ahead
        move.b  d1,d0           ; copy byte
        subi.b  #DLE,d0         ; is it DLE?
        beq.s   sb_dle
        subq.b  #XON-DLE,d0     ; is it XON?
        beq.s   sb_dle
        subq.b  #XOFF-XON,d0    ; is it XOFF?
        bne.s   sb_put          ; none of the above
sb_dle
        move.b  d1,d3           ; save byte
        move.w  io.qtest,a3
        jsr     (a3)            ; check room in queue
        subq.w  #2,d2
        blt.s   sb_full         ; must be room for 2 bytes
        moveq   #DLE,d1
        move.w  io.qin,a3
        jsr     (a3)            ; send DLE
        moveq   #dle_offs,d1
        add.b   d3,d1           ; followed by byte + offset
sb_put        
        move.w  io.qin,a3
        jsr     (a3)            ; send byte to queue
        move.l  (sp)+,a3        ; restore A3
sb_txen
        bset    #q68..txstat,uart_status ; enable tx interrupt
        rts                     ; exit with status from io.qin
err_ro  
        moveq   #err.ro,d0      ; 'read only'
        rts
sb_full                         ; no room for 2 bytes, return NC
        move.l  (sp)+,a3        ; restore A3
        moveq   #err.nc,d0      ; return 'not complete'
        bra     sb_txen

* External interrupt routine

ser_xint
        move.l  a3,-(sp)                ; save A3 (the queue routines smash it!)
        lea     uart_status,a4
        moveq   #q68.rxand,d2
        and.b   (a4),d2                 ; receive buffer empty?
        bne.s   rx_chk_q                ; yes, nothing to fetch
	move.b	uart_rxdata-uart_status(a4),d1 ; always get data
rx_chk_q
        move.l  sp_rxq(a3),a2           ; get address of rx queue (or 0)
        tst.b   sp_flow(a3)             ; flow control enabled?
        bne.s   chk_flow                ; yes, check flow control
        move.l  a2,d0                   ; do we have a rx queue?
        beq     txser                   ; no queue, skip this
        tst.b   d2                      ; got any data?
        bne     txser                   ; no, try sending something

; receive loop without flow control
ser_x_l1
        move.w  io.qin,a3
        jsr     (a3)                    ; store in queue
        btst	#q68..rxmpty,(a4)       ; rx fifo has still bytes waiting?
        bne.s   txser                   ; no
        move.b  uart_rxdata-uart_status(a4),d1 ; get next byte
        bra     ser_x_l1
        
; receive loop with flow control
ser_x_l2
        moveq   #q68.rxand,d2
        and.b   (a4),d2                 ; receive buffer empty?
        bne.s   txser                   ; yes, get out
	move.b	uart_rxdata-uart_status(a4),d1 ; get next byte
; loop enters here
chk_flow
        tst.b   d2                      ; got any data?
        bne.s   chk_room                ; no
        move.l  (sp),a3
        cmpi.b  #xoff,d1                ; remote asked to hold tx?
        beq.s   hold                    ; yes, stop sending
        cmpi.b  #xon,d1                 ; got XON?
        bne.s   put_q                   ; no, put it in queue
        sf      txhold(a3)              ; resume sending
        bset    #q68..txstat,(a4)
        bra.s   ser_x_l2
hold
        st      txhold(a3)              ; stop sending
        bclr    #q68..txstat,(a4)
        bra     ser_x_l2
put_q
        move.l  a2,d0                   ; do we have a rx queue?
        beq.s   txser                   ; no, skip this
        move.w  io.qin,a3
        jsr     (a3)                    ; put byte into queue
chk_room
        move.l  a2,d0
        beq.s   txser                   ; skip if no queue
        move.w  io.qtest,a3
        jsr     (a3)                    ; test for room
        move.l  (sp),a3
        cmp.w   xof_thrs(a3),d2         ; NB: Q68 has 16-byte fifo
        bls.s   rx_full                 ; if room < threshold, check for XOFF
        cmp.w   xon_thrs(a3),d2         ; free space still less than 60?
        bls     ser_x_l2                ; yes, loop back
        tst.b   rxhold(a3)              ; XON already sent?
        beq     ser_x_l2                ; yes, loop back
        moveq   #xon,d1                 ; ok, may send XON now
        bra.s   send_x
rx_full
        tst.b   rxhold(a3)              ; remote already told to hold?
        bne     ser_x_l2                ; yes, don't hammer with XOFFs
        moveq   #xoff,d1                ; must send XOFF now
send_x  
        bsr.s   tx_flow                 ; force send of XON/XOFF
        cmpi.b  #xoff,d1                ; was it XOFF?
        seq     rxhold(a3)              ; yes, set flag
        bra     ser_x_l2                ; loop back

* Serial transmit

txser_s                                 ; entry point from scheduler task
        move.l  a3,-(sp)                ; save A3 first
txser
        lea     uart_status,a4
        move.l  (sp),a3
        tst.b   txhold(a3)              ; are we allowed to send?
        bne.s   nomore                  ; no
        move.l  sp_txq(a3),d0           ; get queue
        beq.s   nomore                  ; no queue, stop sending
        move.l  d0,a2                   
txser_l 
        btst    #q68..txmpty,(a4)       ; UART ready to send?
        beq.s   tx_end                  ; no, return
        move.w  io.qout,a3
        jsr     (a3)                    ; get byte from queue
        bne.s   nomore                  ; nothing got
        move.b  d1,uart_txdata-uart_status(a4) ; send it
        bra     txser_l                 ; loop for more
nomore
        bclr    #q68..txstat,(a4)       ; clear transmit interrupt
tx_end
        move.l  (sp)+,a3                ; restore A3
        rts

* send XON/XOFF with priority

tx_flow
        btst    #q68..txmpty,(a4)       ; UART ready to send?
        beq     tx_flow                 ; no, loop
        move.b  d1,uart_txdata-uart_status(a4)
        rts

* Scheduler loop routine

ser_schd
        tst.l   sp_txq(a3)              ; do we have a queue?
        beq.s   schd_rts                ; no, finished
        moveq   #0,d0                   ; preset 'no error'
        move.w  sr,-(sp)
        ori.w   #$0700,sr               ; don't allow interrupts
        bsr     txser_s                 ; send all pending data
        cmpi.l  #err.ef,d0              ; transmit queue at EOF?
        bne.s   schd_end                ; no, return
        tst.b   rxhold(a3)              ; still holding off other end?
        beq.s   schd_clr
        moveq   #xon,d1
        bsr     tx_flow                 ; don't let other end wait forever!
schd_clr
        move.l  sp_txch(a3),a0          ; clean up channel block and pointers
        clr.l   sp_txch(a3)
        clr.l   sp_txq(a3)
        sf      rxhold(a3)
        sf      txhold(a3)        
        move.w  mm.rechp,a2
        jsr     (a2)
schd_end
        move.w  (sp)+,sr
schd_rts
        rts
                
        end