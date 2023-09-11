* Write a character to screen
        xdef    cs_char16
        xref    getmask16

        include 'm_inc_sd'

        section cs_char

* Parameters:
* d0 x position
* d1 y position
* d2 character to display
* d3 attributes byte
* a0 channel control block
* a1 pointer to colour masks
* a2 primary fount pointer
* a3 secondary fount pointer
* all registers are preserved

* Internal usage:
* d0 bit 31 set if dh and even row count / lsw row loop index
;* d1 bits 5-0 character shift
* d1 mask for chacter bits (lsw)
* d2 character field mask
* d3 bits 31-25 attributes, 31=dw / lsw character row
* d4 bit 31 dh / bit 30 dh row / bit 29 dh blank / lsw char row masked with ink
;* d5 flash mask
* d5 pixel column counter
* d6 paper colour masks
* d7 ink colour masks
* a1 pointer to screen
* a2 pointer to character in fount
* a3 saved char row for dh
* a4 internal jump address
* a5 underscore line number

reglist reg     d0-d7/a1-a5

cs_char16
        movem.l reglist,-(sp)

* Set up the colour patterns

;        movem.l (a1),d6-d7      fetch patterns
        suba.w  #16,sp          ; make room for 4 long words
        moveq   #0,d4           ; flag + offset
masks_lp
        jsr     getmask16(pc)   ; get colour (even lines)
        move.l  d7,d6
        jsr     getmask16(pc)   ; get colour (odd lines)
;        btst    #0,d1           odd row?
;        bne.s   masks_ok        ... yes - carry on
;        exg     d6,d7           ... no - swap the colour masks
masks_ok
        movem.l d6-d7,(sp,d4.w) ; store masks
        bset    #3,d4           ; 0 goes to 8
        beq     masks_lp        ; loop a second time for ink

; the stack now contains:
; 0(sp)  strip masks (even lines)
; 4(sp)  strip masks (odd lines)
; 8(sp)  ink masks (even lines)
; 12(sp) ink masks (odd lines)
        
* Set the fount address

        and.w   #$ff,d2         character is eight bit
        sub.b   (a2)+,d2        take away minimum character in first fount
        cmp.b   (a2)+,d2        is it in range?
        bls.s   calc_font       ... yes - use it
        add.b   -2(a2),d2       ... no - restore character value
        move.l  a3,a2           try alternative fount
        sub.b   (a2)+,d2        take away minimum character
        cmp.b   (a2)+,d2        is it in range?
        bhi.s   font_set        ... no - we're already at invalid char pattern
calc_font
        add.w   d2,a2           address is now 9*character value on from base
        lsl.w   #3,d2
        add.w   d2,a2
font_set

* Now find the screen position

        move.l  sd_scrb(a0),a1  base of screen
        mulu    sd_linel(a0),d1 and y * bytes per row
        add.l   d1,a1           gives the row address
;        moveq   #7,d1
;        and.w   d0,d1           the three ls bits of x form the shift
;        lsr.w   #3,d0           and 8 pixels per byte
;        moveq   #7,d1           ; bit counter for character column
        add.w   d0,d0           ... or rather per pair of bytes
        add.w   d0,a1           ... finishes off the screen address

* Set up the attributes

        move.w  #-1,a5          underline at line -1!!
        btst    #sd..ulin,d3    is underline required
        beq.s   ul_ok           no - leave that
        addq.l  #2,a5           yes - underline at row 1
ul_ok
        moveq   #16,d4          max field width 16 and clear dh flags
        move.w  sd_xinc(a0),d5  ; (NEW) counter for horizontal pixels
        beq     getout          just in case x-inc was zero!
        sub.w   d5,d4
        bcc.s   inc_ok
        moveq   #0,d4           if x-inc is greater than 16, we stop at 16
        ; This needs a bit more fiddling to stick paper into any further
        ; columns when normal writing is occuring....
inc_ok
        subq.w  #1,d5           ready for dbra
        moveq   #0,d2           start field mask
        bset    d4,d2           ; d2 = $8000 .. $0001
        neg.w   d2              ; d2 = $8000 .. $ffff
;        ror.l   #8,d2           ; d2 = $00000080 .. $ff0000ff
;        ror.l   d1,d2           rotate character mask to position

;        moveq   #0,d5           clear flash mask
        ror.l   #sd..dbwd+1,d3  are characters double width
;        bpl.s   width_ok        ... no - that's done
;        addq.w  #8,d1           set double width shift
;        btst    #31-sd..dbwd+sd..flsh,d3 is flash required?
;        beq.s   width_ok        ... no - check for extend
;        move.w  #$4000,d5       set flash on bit
;        bchg    d4,d5           set flash off bit
;        ror.l   d1,d5           rotate flash mask to position
width_ok

        moveq   #0,d0
        move.w  sd_yinc(a0),d0
        beq.l   getout          let's not mess with zero height chars!
        subq.w  #1,d0           ready for dbra
        btst    #31-sd..dbwd+sd..dbht,d3 are tall characters required?
        beq.s   hght_ok
        moveq   #-1,d4          set up tall flags
        ror.l   #1,d0           halve row count but save lsb in bit 31
hght_ok

* Now set up the jump address for character writing mode

        lea     normal,a4       assume normal writing
        btst    #31-sd..dbwd+sd..trns,d3 check for transparent strip
        beq.s   col_loop
        subq.l  #normal-transp,a4
        btst    #31-sd..dbwd+sd..xor,d3 check for xor ink
        beq.s   col_loop
        subq.l  #transp-xor,a4

; * Main loop for each word column occupied by character
; Since we now have 1 word per pixel column, this loop is redundant

collist reg     d0/d6-d7/a1-a2
col_loop
;        movem.l collist,-(sp)
;        move.b  d2,d3           replicate character mask byte
;        lsl.w   #8,d2
;        move.b  d3,d2           ; d2 = $00008080 .. $ff00ffff
;        lsl.w   #8,d5           move flash mask up to ms byte
        moveq   #1,d1
        and.l   16+4(sp),d1     ; pick up original start row
        lsl.w   #2,d1           ; d1 flips between 0 and 4 for even/odd rows

* Inner loop for each row (now outer!)

pixlist reg     d1/d5/a1

row_loop
        bsr     get_mask        ; get strip and ink masks
        movem.l pixlist,-(sp)   ; save registers
        cmp.w   #9,d0           row number 9 or greater?
        bge.s   blank_row       if so, jump, to get blank line
        cmp.w   a5,d0           check for underline row
        beq.s   set_under
        clr.w   d3              clear upper byte
        move.b  (a2)+,d3        and fetch next row
        beq.s   blank_row       if zero - ignore all the next rubbish
        tst.l   d3              for double width, spread characters out
        bpl.s   shft_chr
;        tst.l   d5              are we setting flash at all?
;        beq.s   noflash         no - that's ok
;        and.b   #$7f,d3         yes - can't allow first bit to set ink!
noflash
        move.w  d3,d4
        and.b   #15,d4          ; lower 4 bits
        lsr.b   #4,d3           ; upper 4 bits
        move.b  spread(pc,d3.w),d3
        lsl.w   #8,d3
        move.b  spread(pc,d4.w),d3 ; form complete row in d3.w
        bra.s   mask_chr

spread
        dc.l    $00030c0f,$30333c3f,$c0c3cccf,$f0f3fcff
        
shft_chr
;        ror.w   d1,d3           shift to position and replicate
        lsl.w   #8,d3           ; no dw, use only upper byte
mask_chr
        and.w   d2,d3           mask character with field
        beq.s   blank_row       ; oops.. nothing left!
; loop for each 
pix_loop
;        move.b  d3,d4           replicate a byte
;        lsl.w   #8,d3
;        move.b  d4,d3
        lsl.w   #1,d3           ; get next bit from character column
        subx.w  d1,d1           ; set D1 to $0000 or $ffff 

mask_ink
        move.w  d1,a3           save masked character row in case dh
        move.w  d1,d4
        and.w   d7,d4           mask character with ink
;        or.w    d5,d4           put flash in
        jsr     (a4)            call code for writing mode
        dbra    d5,pix_loop
        bra.s   end_row         ; next row

set_under
        move.w  #-1,d3           full row underline
        addq.l  #1,a2           move fount pointer past ignored byte
        move.w  d3,a3           save masked character row in case dh
        move.w  d3,d4
        and.w   d7,d4           mask character with ink (can't flash)
setul_lp
        move.w  d3,d1           ; set mask to all 1s
        jsr     (a4)            call code for writing mode
        dbra    d5,setul_lp
        bra.s   end_row         ; next row
        
blank_row
        bclr    #29,d4          clear flag in case dh
blank_2nd
        btst    #31-sd..dbwd+sd..trns,d3 is it anything but normal writing?
        bne.s   end_row         yes - nothing to do
;        move.w  (a1),d4         pick up existing stuff
;        eor.w   d6,d4           flip with paper pattern bits
;        and.w   d2,d4           keep just the field area we want to affect
blank_lp
        move.w  d6,(a1)+        ; just set pixel to paper colour!
        swap    d6              ; swap stipple pattern
        dbra    d5,blank_lp
        bra.s   end_row

; write mode routines
; d1.w -i   mask: 0 for paper, $ffff for ink
; d4.w -ip  d1 ANDed with ink colour mask
; d5.w -i o pixel counter (updated)
; d6.w -ip  paper colour mask

xor
        eor.w   d4,(a1)+        xor into screen
        bra.s   end_pix

transp
        not.w   d1              mask in background not part of character
        and.w   (a1),d1
        bra.s   combine

normal
;        eor.w   d2,d3           invert character in field
;        and.w   d6,d3           mask inverse character with paper
;        or.w    d4,d3           put in ink/flash
;        move.w  d2,d4           get inverted field mask in d4
        not.w   d1              ; invert mask
        and.w   d6,d1           ; mask inverse with paper
        and.w   (a1),d1         blank out character area of screen
combine
        or.w    d1,d4           combine the two
        move.w  d4,(a1)+        and put in screen
end_pix swap    d6              ; swap colours for next column
        swap    d7
        rts
        
end_row
;        swap    d6              swap colours
;        swap    d7
        movem.l (sp)+,pixlist
        bchg    #2,d1           ; swap even and odd colours
        add.w   sd_linel(a0),a1 take next line
        tst.l   d4              check for double height
        bpl.s   next_row

dh_row
        bchg    #30,d4          toggle row bit
        beq.s   next_row        if it was already second row, carry on normal
        tst.l   d0              is it the last row of an odd height?
        beq.s   col_end         yes - then it's finished
        bsr.s   get_mask        ; get masks for next row
        movem.l pixlist,-(sp)   ; save registers again
        bset    #29,d4          is it 2nd row of a blank character?
        beq     blank_2nd       yes - go do that
        move.w  a3,d1           restore the character
        bra     mask_ink        go do normal stuff 

next_row
        dbra    d0,row_loop

col_end
;        movem.l (sp)+,collist   recover various registers
;        clr.w   d2              check for any remaining bits
;        rol.l   #1,d2           ... by rotating mask to next bit
;        bne.s   nxt_col         if there's more, go do it
getout
        adda.w  #16,a7          ; drop storage for colour masks
        movem.l (sp)+,reglist
        rts

; get stored 16-bit colour masks for each row (strip + ink)
; assumes stored masks are at 4(sp)!
; d1 -ip   0 for even rows, 4 for odd rows
; d6 -  o- strip colour mask (lsw current pixel, msw next pixel)
; d7 -  o- ink colour mask (lsw current pixel, msw next pixel)
; a1 -ip   address of current pixel
; d4.w smashed

get_mask
        move.l  4(sp,d1.w),d6   ; get strip mask (alternate each row)
        move.l  4+8(sp,d1.w),d7 ; id. for ink mask
        move.w  a1,d4
        and.w   #2,d4           ; d4 = 2 for odd pixel column
        bne.s   noswap          ; yes
        swap    d6              ; swap left and right stipple colours
        swap    d7
noswap
        rts
;nxt_col
;        clr.w   d5              roll in next byte's worth of flash mask
;        rol.l   #8,d5
;        asr.l   #4,d4           reset dh flags

;        eor.b   #8,d1           flip bytes on the rotate
;        subq.w  #1,d1           ; count down character column bit to shift out
;        addq.l  #2,a1           move to next column
;        bra.l   col_loop

        end
