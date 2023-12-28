***********************************
* Q68 screen extensions for Minerva
***********************************

* Use and distribution governed by GNU Public License v3
* 
* Changelog:
*
* 20230705 JB:
*   Start of work

        xdef    disp_mode,scr_base,scr_llen,scr_xlim,scr_ylim
        
        xref    stk_long
        
        include m_mincf
        include extrarom_userdefs
        include m_inc_sv
        include m_inc_sx
        include m_inc_bv
        include m_inc_err
        include m_inc_mt
        include m_inc_vect
        include m_inc_q68
        include m_inc_assert

        GENIF   Q68_M33 <> 0
        xdef    q68scr_init
        xref    stk_long,cs_fill16,cs_over16,cs_pan16,cs_recol16,cs_scrol16
        xref    cs_char16
        ENDGEN

        section q68screen

* Table of relative pointers to 16-bit cs_* routines
* These are used to build the absolute pointers at the start of the
* fast memory area. The routines themselves are still in (extra)ROM for now,
* but will be copied to fast RAM at a later stage (and the pointers adjusted).

        GENIF   Q68_M33 <> 0

cs_table
        dc.w    cs_char16-*     ; write character
        dc.w    cs_scrol16-*    ; scroll
        dc.w    cs_pan16-*      ; pan
        dc.w    cs_fill16-*     ; fill an area
        dc.w    cs_over16-*     ; fill with OVER
        dc.w    cs_recol16-*    ; recolour an area
        dc.w    0               ; end marker

q68scr_init
        trap    #0              ; better go supervisor when altering fastmem!
        lea     cs_table(pc),a1
        lea     q68_sramb,a2
        cmpi.l  #$19004,(a2)+
        bne.s   init_oops     ; fastmem area should be empty!
init_lp
        move.w  (a1)+,d0        ; get relative pointer
        beq.s   init_ok         ; end reached
        lea     -2(a1,d0.w),a0  ; get effective address
        move.l  a0,(a2)+        ; and store it
        bra     init_lp         ; loop for next
init_ok
        move.l  a2,q68_sramb    ; set new free memory pointer
        lea     okmsg,a1
        bra.s   init_end
init_oops
        lea     failmsg,a1
init_end
        andi.w  #$dfff,sr       ; back to user mode
        suba.l  a0,a0
        move.w  ut.mtext,a2
        jmp     (a2)            ; print message and exit

okmsg   dc.w    okend-*-2
        dc.b    'Q68 16-bit screen driver v1.0  JB 2023',10
okend   equ     *

failmsg dc.w    failend-*-2
        dc.b    '*Cannot initialise Q68 screen driver*',10
failend equ     *

        ENDGEN

* SCR_BASE, SCR_LLEN, SCR_XLIM, SCR_YLIM functions

* These functions are now commonly found in environments which support
* screen resolutions higher than the original QL's 512x256x4 or 256x256x8.
* SCR_BASE: Returns base of screen buffer
*              $20000: QL screen 0, $28000 QL screen 1 (Minerva dual-screen)
*           $FE800000: Q68 extended modes (2-7)
*
* SCR_LLEN: Returns length of one screen line of pixels in bytes
*           QL mode 4 or 8: 128 bytes
*           Q68 mode 4 (1024x768x4): 256 bytes
*           (Other Q68 modes presently not implemented)
*
* SCR_XLIM, SCR_YLIM: Return display width and height in pixels
*
* On SMSQ/E, these functions have some issues:
*   - SCR_BASE returns a negative value for Q68 extended modes!
*   - The functions accept a channel as parameter, however this is ignored
*     (a pity, since channels may have a different screen base (dual-screen)
*      and often different window sizes)
* In order to remain compatible, we'll ignore the parameter for now...

* subroutine to get status of Q68 extended screen mode

get_dm  moveq   #mt.inf,d0
        trap    #1
        move.l  sv_chtop(a0),a4         ; get base of sx variables
        moveq   #1<<sx.q68m4,d0         ; test status of Q68 extended mode
        and.b   sx_dspm(a4),d0
        beq.s   get_rts                 ; return 0 with QL mode 0 or 8
        move.b  sx_dmod(a4),d0          ; else, return real DISP_MODE
get_rts rts

* table containing values to be returned for scr_base, scr_llen, scr_x/ylim
* each entry two values for QL and Q68 extended mode respectively

scrtab: dc.w    2,$fe80         ; scr_base (msw only)
        dc.w    128,256         ; scr_llen
        dc.w    512,1024        ; scr_xlim
        dc.w    256,768         ; scr_ylim


scr_base:
        bsr     get_dm
        moveq   #2,d1           ; $20000 >> 16
        tst.b   d0              ; old QL mode?
        beq.s   sb_swap         ; yes
        move.w  #q68_screen>>16,d1 ; else, return Q68 extended screen base
sb_swap:
        swap    d1              ; real screen base now in D1
        jmp     stk_long        ; this returns a long word converted to float

scr_llen:        
        moveq   #sx_llen,d4
        bra.s   scr_ent
scr_xlim:
        moveq   #sx_xlim,d4
        bra.s   scr_ent
scr_ylim:
        moveq   #sx_ylim,d4
scr_ent:
        bsr     get_dm          ; get mode
        moveq   #2,d1           ; ensure enough space on ri stack
        move.w  bv.chrix,a2
        jsr     (a2)
        move.l  bv_rip(a6),a1   ; ri stack pointer
        subq.l  #2,a1           ; two bytes needed
        move.w  (a4,d4.w),(a6,a1.l) ; stack the result
        move.l  a1,bv_rip(a6)   ; set stack
        moveq   #3,d4           ; integer result
        moveq   #0,d0           ; finish with no error
        rts

* High resolution 1024x768x4 for the Q68!
* Implemented the DISP_MODE command with a subset of the SMSQ/E version
* Currently, modes 0 (256x256x8), 1 (512x256x4) and 4 (1024x768x4) supported
* (sorry guys, implementing 65536-colour mode would require a total rewrite of
* the screen drivers, including implementation of the GD2 colour schemes, which
* is far beyond the scope of this project and already available within SMSQ/E)
*
* That said, it would be nice if the original QL's 8-colour mode could be made
* available at the higher resolutions offered by the Q68. Also, 1024x768 might
* be difficult to read on modern LCD-type monitors where the native resolution 
* is not an exact multiple of 1024x768. I personally like the Q68's 512x384
* mode which offers good readability (with better aspect ratio than 512x256!)
* at reasonable speed, with the possibility for more than the boring old
* 4 colours. So this remains an item on my to-do list...
*
* The 1024x768 mode is flagged by setting bit 4 of sx.dspm (which is reserved
* in the original Minerva). The parameters of each CON/SCR channel are modified
* accordingly, which is transparant to most TRAPs. The MT.DMODE trap will
* handle most of the hard work when changing the modes.
* Note that Minerva's dual-screen option is not available in 1024x768 mode, and
* attempting to switch to 1024x768 with dual screen enabled will generate an
* error.

disp_mode:
        move.w  ca.gtint,a2
        jsr     (a2)
        bne.s   dm_end
        subq.w  #1,d3
        bne.s   dm_bp           ; one integer parameter allowed
        move.w  (a6,a1.l),d4
        addq.l  #2,bv_rip(a6)
        bsr     get_dm
        assert  sx_dmod,sx_llen-1,sx_xlim-3,sx_ylim-5
        lea     sx_dmod(a4),a1
        cmpi.w  #1,d4
        bhi.s   dm_hires
; QL modes 0 or 1 requested
        bclr    #sx.q68m4,sx_dspm(a4)
        lsl.w   #3,d4           ; now 0 or 8 for DISP_MODE 0/1
        moveq   #8,d1
        eor.b   d4,d1           ; flip bit 3 so DISP_MODE 0 -> MODE 8
        bra.s   dm_setmd
dm_hires:
        cmpi.w  #q68.dmax,d4    ; maximum allowed under Q68
        bhi.s   dm_bp
        move.w  d4,d1           ; D1 = mode requested (2-7)
        tst.b   d0
        bne.s   dm_sethi        ; we're already in hi-res mode!
        move.w  a0,d0
        bpl.s   dm_nc           ; sorry, dual screen not supported in hi-res
dm_sethi:
; reject 16-bit modes if not configured!
        GENIF   Q68_M33 = 0
        cmpi.w  #4,d1
        bne.s   dm_bp
        ENDGEN
        
        bset    #sx.q68m4,sx_dspm(a4) ; set hi-res bit
;        move.b  d4,(a1)+        ; sx_dmod
;        lsl.w   #1,d4
;        move.w  llen_tab(pc,d4.w),(a1)+ ; sx_llen
;        move.w  xlim_tab(pc,d4.w),(a1)+ ; sx_xlim
;        move.w  ylim_tab(pc,d4.w),(a1)+ ; sx_ylim

; MT.DMODE will handle all the nitty-gritty work of clearing the screen,
; resetting windows and finally informing the hardware of the mode change
; This trap has been modified to handle the extended modes when bit 4 of sx_dspm
; has been set.

dm_setmd:
        moveq   #-1,d2          ; leave TV mode alone
        moveq   #mt.dmode,d0
        trap    #1              ; set mode
dm_end  rts

dm_bp:  moveq   #err.bp,d0      ; 'bad parameter' return
        rts
dm_nc   moveq   #err.nc,d0      ; 'not complete'
        rts
        
; DISP_MODE       0,  1,  2,    3,  4,   5,  6,   7
; bits-per-pixel:4*,  2, 16,   16,  2,  8,  16,  16
llen_tab:
        dc.w    128,128,1024,2048,256,1024,1024,2048
xlim_tab:
        dc.w    512,512,512,1024,1024,1024,512,1024
ylim_tab:
        dc.w    256,256,256,512,768,768,384,768


; * DISP_MODE 0 is old QL MODE 8 with 256 pixels across, but still a 512x256
;   coordinate system. Therefore SCR_XLIM returns 512, as is the convention.

        end
        