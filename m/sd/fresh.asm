* Refresh a screen by clearing it, then redrawing all con channels
        xdef    sd_fresh,sd_modes

        xref    sd_bordn,sd_bordr,sd_clear
        xref    cs_color

        include 'm_mincf'
        include 'm_inc_assert'
        include 'm_inc_ch'
        include 'm_inc_mc'
        include 'm_inc_ra'
        include 'm_inc_sd'
        include 'm_inc_sv'
        include 'm_inc_sx'
        include 'm_inc_q68'

        section sd_fresh

* a5 -i p- base address of screen to be refreshed
* d0-d3/a0-a1/a3-a4 destroyed

sd_fresh
        move.l  a5,a1
        move.l  #ra_ssize/4,d0 ... long words
        
        GENIF   Q68_HIRES = 1
        
        move.l  sv_chtop(a6),a4
        btst    #sx.q68m4,sx_dspm(a4)
        beq.s   cls
        move.l  #1024*768*2/4,d0 ; at most 1024x768px at 2 byte/px = 393216 long word
        
        ENDGEN
cls
        clr.l   (a1)+
        subq.l  #1,d0
        bhi     cls

* Now go through all windows open, resetting and clearing them

        move.l  sv_chbas(a6),a4 start address of channel tables
chn_loop
        move.l  (a4)+,d0        next channel
        blt     next_chn        ... does it exist?
        move.l  d0,a0           set address of channel definition block
        move.l  sv_chtop(a6),a3 sysvar extension
        lea     sx_con(a3),a3   linkage area for con driver
        cmp.l   ch_drivr(a0),a3 is it a window?
        bne     next_chn        ... no

        GENIF   Q68_HIRES = 1

* code below needs to be moved elsewhere with proper sd_wdef calls!
; check if window will fit in new screen

;        move.w  sd_xmin(a0),d0
;        add.w   sd_xsize(a0),d0
;        cmp.w   sx_xlim-sx_con(a3),d0   ; does new width fit?
;        bls.s   chn_chky                ; yes
;        move.w  sx_xlim-sx_con(a3),sd_xsize(a0) ; else reset to screen width
;        clr.w   sd_xmin(a0)             ; and set X origin to 0
chn_chky
;        move.w  sd_ymin(a0),d0
;        add.w   sd_ysize(a0),d0
;        cmp.w   sx_ylim-sx_con(a3),d0   ; same with window height
;        bls.s   chn_rset
;        move.w  sx_ylim-sx_con(a3),sd_ysize(a0)
;        clr.w   sd_ymin(a0)
chn_rset
        move.w  sx_llen-sx_con(a3),sd_linel(a0) ; set new line length
        btst    #sx.q68m4,sx_dspm-sx_con(a3) ; Q68 extended mode?
        beq.s   chn_tst         ; no
        tst.l   sd_scrb(a0)     ; avoid qptr dummy window
        beq.s   chn_set
        move.l  a5,sd_scrb(a0)  ; set new screen buffer addr
        bra.s   chn_set
        
        ENDGEN

chn_tst
        moveq   #ra_bot>>16,d0  treat zero screen base in qptr dummy as scr0
        swap    d0
        
        GENIF   Q68_HIRES = 1
        
        tst.w   sd_scrb(a0)     ; msw is <0 if switching hi-res to lo-res
        bpl.s   chn_cmp
        move.l  d0,sd_scrb(a0)  ; reset base addr to $20000 (no dual screen)
        bra.s   chn_set

        ENDGEN

;       cmp.l   sd_scrb(a0),a5  is this the screen we are doing?
; above is proper, but pander to qptr on it's dummy window... next 4 instrns

chn_cmp
        or.l    sd_scrb(a0),d0  get this channel's screen base
        cmp.l   d0,a5           is this the screen we are doing?
        bne.s   next_chn        ... no
chn_set
        bsr.s   sd_modes        set up info for this mode

        tst.b   (a1)            if cursor is not suppressed ...
        sne     (a1)            ... make it invisible

        move.w  sd_borwd(a0),-(sp) save border as we have to re-make it
        clr.w   d2              in case it is transparent
        jsr     sd_bordn(pc)    and remove old border

        jsr     sd_clear(pc)    clear all of window

        move.b  (a3),d1         set border colour
        move.w  (sp)+,d2        ... and width
        jsr     sd_bordr(pc)

next_chn
        cmp.l   sv_chtop(a6),a4 end of channel list?
        blt     chn_loop
        rts

* Routine for setting mode dependent info

* d0 -  o- ms3b -1, lsb = 0 for mode 4, 1<<sd..dbwd ($40) for mode 8, ccr set
* a0 -ip - channel definition
* a1 -  o- sd_curf(a0)
* a3 -  o- sd_bcolr(a0)
* d1 destroyed

sd_modes
        lea     sd_pmask(a0),a1 set up addresses of colour masks and bytes
        assert  sd_pmask,sd_smask-4,sd_imask-8,sd_cattr-12,sd_curf-13
        lea     sd_pcolr(a0),a3
        assert  sd_pcolr,sd_scolr-1,sd_icolr-2,sd_bcolr-3
        moveq   #3-1,d0         recreate three of them
col_loop
        move.b  (a3)+,d1        get colour byte
        jsr     cs_color(pc)    set colour mask
        addq.l  #4,a1           next mask
        dbra    d0,col_loop

        assert  sd_xinc,sd_yinc-2
        move.l  #6<<16!10,sd_xinc(a0) default x/y increments
        moveq   #1<<mc..m256,d0
        and.b   sv_mcsta(a6),d0
        lsl.b   #sd..dbwd-mc..m256,d0
        move.b  d0,(a1)+        set sd_cattr, point to sd_curf
        beq.s   rts0            512 - all done
        lsl     sd_xinc(a0)     256 - double the x increment
rts0
        rts

        end
