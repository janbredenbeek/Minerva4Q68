; FREE_FMFM AND ALFM keywords 1.00 (c) W. Lenerz 2017
; 2018-12-10  1.01  make sure mem request is even (wl) - thanks to Martyn Hill.
; 2023-07-17  backport to Minerva (Jan Bredenbeek)

	xdef	alfm
	xdef	free_fmem
        xdef    stk_long

        include m_mincf
        include m_inc_sv
        include m_inc_bv
        include m_inc_err
        include m_inc_mt
        include m_inc_vect
        include m_inc_q68

* Return amount of free space in fast memory
* NOTE: The maximum amount of RAM that can be available is 12K - 4 bytes
*       Hence, the current code assumes that the result can fit into 16 bits!

        section fastmem

free_fmem
	moveq   #2,d1
        move.w  bv.chrix,a2
        jsr     (a2)                    ; ensure enough space on ri stack
        bsr.s   ffmem2                  ; free mem to d1
        move.l  bv_rip(a6),a1
        subq.l  #2,a1
        move.w  d1,(a6,a1.l)
	move.l  a1,bv_rip(a6)
        moveq   #3,d4                   ; integer result
        moveq   #0,d0
        rts

; subroutine to calculate free space in fast memory
;   d1    o     amount of free space or 0 if nothing left
;   d3    o     pointer to current free space
;   a2    o     start of fast memory

ffmem2
	move.l	#q68_sramb,a2		; pointer to first free mem in fast ram
	move.l	#q68_sramt,d1		; top of ram
	move.l	(a2),d3                 ; ptr to current free space
	sub.l	d3,d1
	subq.l	#4,d1			; remaining free mem
	bge.s	ok
	clr.l	d1			; nothing remains
ok	rts

errbp   moveq   #err.bp,d0
        rts

alfm    move.w  ca.gtint,a2             ; get integer parameter
        jsr     (a2)
	bne.s	ok
        subq.w  #1,d3                   ; check for exactly 1 argument
        bne.s   errbp
	move.w	(a6,a1.l),d0            ; get required space
	addq.l	#1,d0
	bclr	#0,d0			; make sure it's even
	addq.l	#2,bv_rip(a6)           ; balance stack
; NEW: should enter supervisor mode before proceeding, else another job may
;      have grabbed this space in the meantime! (jb)
        trap    #0                      ; enter supervisor mode
	bsr     ffmem2			; check how much mem is free
	ble.s	oom
	sub.l	d0,d1			; we need this much mem
	blt.s	oom			; but there's not enough remaining
        move.l  d3,d1                   ; pointer to current free space
	add.l	d0,d3			; new Sram free space
	move.l	d3,(a2)
        andi.w  #$dfff,sr               ; back to user mode
stk_long:                               ; this entry stacks a long word in D1
        move.l  d1,d4
        moveq   #6,d1                   ; check if there's room for float
        move.w  bv.chrix,a2
        jsr     (a2)
        move.l  bv_rip(a6),a1
        subq.l  #4,a1
        move.l  d4,(a6,a1.l)            ; stack long integer
        moveq   #9,d0                   ; RI.FLONG (minerva only!)
        moveq   #0,d7                   ; REALLY NEEDED?
        move.w  ri.exec,a2
        jsr     (a2)                    ; convert long to float
        move.l  a1,bv_rip(a6)
        moveq   #2,d4                   ; float result
        rts

oom	andi.w  #$dfff,sr               ; go back to user mode first!
        moveq	#err.om,d0
	rts

	end
