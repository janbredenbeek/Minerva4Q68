* **********************************
* q68 history driver for Minerva ROM
* **********************************
* Based on SMSQ/E History device by Marcel Kilgus
**
* Copyright (C) 2025 Jan Bredenbeek
* 
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program.  If not, see <https://www.gnu.org/licenses/>.
; 
; Changelog:
;
; 2025-07-28 First version
;
; 2025-08-11 Now accepts foreign language versions (1A98 - 1Z99) too.

version setstr  1.01

        include m_mincf
        include extrarom_userdefs
        include m_inc_bv
        include m_inc_sv
        include m_inc_io
        include m_inc_mt
        include m_inc_vect

string$	macro	a
	noexpand
[.lab]	dc.w	.e.[.l]-*-2
	dc.b	[a]
.e.[.l]	equ	*
	ds.w	0
	endm

        xdef    his_base

        section history
        
his_base
        moveq   #mt.inf,d0
        trap    #1
        tst.l   d1
        bne.s   his_err         ; must be launched from Job 0!
        andi.b  #$fe,d2
        cmpi.w  #'98',d2        ; minor version must be 98 or 99
        bne.s   his_err
        swap    d2
        cmpi.w  #'1.',d2        ; accept 1.98 or 1.99
        beq.s   ver_ok
        andi.w  #$ffdf,d2
        cmpi.w  #'1A',d2
        blt.s   his_err         ; Also accept 1A98...
        cmpi.w  #'1Z',d2
        bgt.s   his_err         ; ... to 1Z99 for foreign languages
ver_ok
        tst.l   bv_hichn(a6)    ; is there a HISTORY channel already?
        bgt.s   his_exit        ; Yes, leave it alone!
        jsr     his_init        ; call the actual initialisation routine
        tst.l   d0
        bne.s   his_err         ; bail out on any error
        moveq   #-1,d1          ; for this job
        moveq   #0,d3           ; exclusive access
        lea     his_name,a0
        moveq   #io.open,d0
        trap    #2              ; open HISTORY channel
        tst.l   d0
        bne.s   his_err         ; oops!
        move.l  a0,bv_hichn(a6) ; store ID
        suba.l  a0,a0           ; print message
        lea     his_msg,a1
        move.w  ut.mtext,a2
        jmp     (a2)            ; exit
his_err
        move.l  d0,d7           ; save error code
        suba.l  a0,a0
        lea     his_erms,a1     ; print error message
        move.w  ut.mtext,a2
        jsr     (a2)
        move.l  d7,d0
his_exit
        rts
        
his_name string$ 'HISTORY_2048'
his_msg  string$ {'HISTORY device for Minerva v[version] JB 2025',10}
his_erms string$ {'*** Cannot initialise HISTORY device!',10,'(maybe incorrect Minerva version?',10}

his_init equ    *

* Now include the HISTORY_rext built from SMSQ/E source
 
        incbin 'hist_rext'

        end