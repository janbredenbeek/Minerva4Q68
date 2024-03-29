* Screen block operations
        xdef   sd_fill

        xref   cs_color,cs_fill,cs_over

        include 'm_inc_assert'
        include 'm_inc_err'
        include 'm_inc_mc'
        include 'm_inc_sd'
        include 'm_inc_sv'
        include 'm_inc_sx'
        include 'm_mincf'
        include 'm_inc_q68'

        section sd_fill

* To get round some hassles, and slightly improve the functionality of this
* call, we will no longer treat anything as out of range! What we will do is to
* fill in that part of the block specification which overlaps the window.
* All the sizes and offsets will be treated as signed numbers.

* d1 c   colour to fill / overwrite
* a1 c   pointer to block definitions

sd_fill
        GENIF   Q68_M33 <> 0
        move.l  sv_chtop(a6),a2
        btst    #sx..m33,sx_dmod(a2)    ; MODE 33?
        beq.s   sd_fill2                ; no, use QL mode
        move.l  cs.fill16,a3            ; get vector to 16-bit fill
        btst    #sd..xor,sd_cattr(a0)
        beq.s   csset                   ; .. unless OVER -1
        move.l  cs.over16,a3
        bra.s   csset
        ENDGEN
sd_fill2
        btst    #sd..xor,sd_cattr(a0) is xor bit set?
        lea     cs_fill(pc),a3  no - assume fill
        beq.s   csset
        lea     cs_over(pc),a3  yes - it's over required
csset        
        move.l  a1,a2
        subq.l  #4,sp
        move.l  sp,a1           ; buffer for colour masks
        jsr     cs_color(pc)
        movem.l (a2),d2-d3      fetch block size and position
        btst    #mc..m256,sv_mcsta(a6) is it low res
        beq.s   resdone
        moveq   #16,d0          bit number of lsb of x
        bclr    d0,d3           clear xbase lsb
        bclr    d0,d2           clear xsize lsb
resdone
        assert  sd_xmin,sd_ymin-2,sd_xsize-4,sd_ysize-6
        movem.l sd_xmin(a0),d0-d1 get min and size of window
        bsr.s   check
        ble.s   finis           w at x does not overlap window
        bsr.s   check
        ble.s   finis           h at y does not overlap window
        add.l   d3,d0           add x/y to min
        move.w  d0,d1           put y start
        swap    d0              get down x start
        move.w  d2,d3           put h
        swap    d2              get down w
        jsr     (a3)            do the operation
finis
        moveq   #0,d0
        addq.l  #4,sp           restore stack
        rts

check
        swap    d1
        swap    d2
        swap    d3
        tst.w   d2
        ble.s   rts0            size already not positive
        tst.w   d3
        bpl.s   baseok
        add.w   d3,d2
        ble.s   rts0            base + size doesn't get into window
        clr.w   d3
baseok
        sub.w   d3,d1           see how much space after base
        ble.s   rts0            base beyond window, so forget it
        sub.w   d2,d1           can we fit whole width in?
        bgt.s   rts0            yes, so do so
        add.w   d1,d2           chop down size to what we can fit in
rts0
        rts

        end
