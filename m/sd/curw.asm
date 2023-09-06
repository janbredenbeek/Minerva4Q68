* Cursor writing routines
        xdef    sd_cure,sd_curs,sd_curt,sd_sched
 
        xref    cs_over,cs_plain
        xref    sd_chchk,sd_donl

        include 'm_inc_sd'
        include 'm_inc_sv'
        include 'm_inc_sx'
        include 'm_mincf'
        include 'm_inc_q68'

        section sd_curw

* d0   o error code
* a0 i p address of window control block

sd_curs
        sf      -(sp)           flag -> 0
        tst.b   sd_curf(a0)     check if visible
        bgt.s   toggle
        bra     exit_set

sd_cure
        st      -(sp)           flag -> -1
        tst.b   sd_curf(a0)     check if already visible
        bgt     exit_ok
        jsr     sd_donl(pc)     issue newline if pending
        bra.s   toggle

* d3 c   number of poll interrupts since last call
* d4   s scratch
* a0   s address of current window definition block
* a1   s address of system variables extension
* a6 c   base address of system variables

sd_sched
        lea     sv_fstat(a6),a0
        sub.w   d3,(a0)         decrement count
        bcc     rts0            if it's not gone nasty, leave it
        move.l  sv_chtop(a6),a1 this is where the sysvars extension lives
        moveq   #0,d4
        move.b  sx_fstat(a1),d4
        lsr.b   #3,d4           use top 5 bits as flash rate (0,2,..,28,30)
        move.w  d4,(a0)         (flash rate odd iff underline cursor... so?)
        move.l  sv_keyq(a6),d4  check if there are any queues
        beq.s   rts0
        move.l  d4,a0           put address in a0
        lea     -sd_end(a0),a0  point to start of block
        tst.b   sd_curf(a0)     is cursor suppressed?
        beq.s   rts0            yes - nothing to do

sd_curt
        move.b  sd_curf(a0),-(sp)
        beq.s   exit_ok

toggle
        jsr     sd_chchk(pc)    check if there is room for a character
        bne.s   exit

regon   reg     d1-d3/a1-a2
regoff  reg     d0-d3/a1-a2     (d0 just to discard space for colour masks)
        movem.l regon,-(sp)
        move.l  sv_chtop(a6),a1 get sysvars extension
        move.b  sx_fstat(a1),d1
        jsr     cs_plain(pc)
        move.w  d2,-(sp)
        move.w  d2,-(sp)
        move.l  sp,a1
        move.l  sd_xmin(a0),d0  get top lhs of window
        add.l   sd_xpos(a0),d0  add cursor position
        movem.w sd_xinc(a0),d2/d3 set cursor size
        lsl.b   #4,d1
        bpl.s   curset
        add.w   d3,d0
        lsr.w   #2,d3           make cursor occupy about 1/4 of charsize
        sub.w   d3,d0
curset
        move.w  d0,d1           split into two registers
        swap    d0
        btst    #sx..m33,sx_dmod(a2) ; a2 has been set by cs_plain
        lea     cs_over(pc),a2
        beq.s   curprint
        move.l  cs.over16,a2
curprint
        jsr     (a2)
        movem.l (sp)+,regoff
        neg.b   (sp)

exit_set
        move.b  (sp),sd_curf(a0)  set flag to new status
exit_ok
        moveq   #0,d0
exit
        addq.l  #2,sp
rts0
        rts

        end
