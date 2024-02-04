* Boot loader for Minerva4Q68 (from SMSQ/E, etc.)
* This should be followed by the Minerva ROM code

; 20240128 (JB) now do full Q68 hardware reset

        include 'm_mincf'
        include 'm_inc_q68'

        section code

        trap     #0
        ori.w    #$700,sr       ; supervisor and interrupts off
        lea      q68rom(pc),a0  ; start of Minerva
        suba.l   a1,a1          ; copy to start of Q68 RAM
        move.w   #96*1024/4-1,d0 ; at most 96K
copy    move.l   (a0)+,(a1)+    ; do the copy
        dbra     d0,copy
;        movem.l  0,a0-a1      3w
;        move.l   a0,a7        1w
;        jmp      (a1)         1w
;                              --
;                              5w

        move.w  #q68.reset,q68_reset ; reset Q68 hardware, 4w
        bra.s   *       ; dummy instruction to keep size at 32 bytes

q68rom   equ      *

         end

