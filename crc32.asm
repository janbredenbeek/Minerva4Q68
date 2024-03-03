*******************************************
* Calculate CRC32 over a given memory range
*******************************************

* Usage:   a$=CRC32$(start, length)
* Returns: an 8-digit hexadecimal string containing the CRC-32 over the bytes
*          in the given memory range

* Copyright (C) 1989-2024 Jan Bredenbeek
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

        include m_inc_bv
        include m_inc_err
        include m_inc_ri
        include m_inc_vect

        section crc32
        
        lea     proctab,a1
        move.w  bp.init,a2
        jmp     (a2)

proctab
        dc.w    0,0,1           ; no procs, one function
        dc.w    crc32-*
        dc.b    6,'CRC32$'
        dc.w    0

crc_bp  moveq   #err.bp,d0
crc_rt  rts

crc32
        move.w  ca.gtlin,a2     ; get long integers
        jsr     (a2)
        bne.s   crc_rt
        subq.w  #2,d3           ; usage: a$=CRC32$(start,length)
        bne.s   crc_bp
        moveq   #14-8,d1        ; need 14 bytes minus 2 longwords
        move.w  bv.chrix,a2
        jsr     (a2)            ; check math stack
        move.l  bv_rip(a6),a1
        move.l  (a6,a1.l),a2    ; start address
        move.l  4(a6,a1.l),d1   ; length
        beq.s   crc_bp          ; .. which must be nonzero
        moveq   #-1,d2          ; initialise CRC
crc32_lp
        moveq   #0,d0
        move.b  (a2)+,d0        ; get next byte
        eor.b   d2,d0           ; xor with lsb of running crc
        lsl.w   #2,d0           ; index into crc table
        move.l  crc32tab(pc,d0.w),d0
        lsr.l   #8,d2
        eor.l   d0,d2           ; new crc = (old crc >> 8) xor crc(index)
        subq.l  #1,d1
        bne     crc32_lp        ; loop for all bytes
        not.l   d2              ; invert crc
        subq.l  #14-8,a1        ; make room for string result + long word
        move.l  d2,(a6,a1.l)    ; put result on math stack
        lea     6(a1),a0        ; buffer for string result (above long word)
        move.w  #8,-2(a6,a0.l)  ; string length
        move.w  cn.itohl,a2
        jsr     (a2)            ; convert result to hex string
        move.l  a1,bv_rip(a6)   ; set stack (a1 is past long word now)
        moveq   #1,d4           ; string result
        moveq   #0,d0           ; no error
        rts

* CRC32 value corresponding to each byte value

crc32tab
        dc.l    $00000000,$77073096,$ee0e612c,$990951ba
        dc.l    $076dc419,$706af48f,$e963a535,$9e6495a3
        dc.l    $0edb8832,$79dcb8a4,$e0d5e91e,$97d2d988
        dc.l    $09b64c2b,$7eb17cbd,$e7b82d07,$90bf1d91
        dc.l    $1db71064,$6ab020f2,$f3b97148,$84be41de
        dc.l    $1adad47d,$6ddde4eb,$f4d4b551,$83d385c7
        dc.l    $136c9856,$646ba8c0,$fd62f97a,$8a65c9ec
        dc.l    $14015c4f,$63066cd9,$fa0f3d63,$8d080df5
        dc.l    $3b6e20c8,$4c69105e,$d56041e4,$a2677172
        dc.l    $3c03e4d1,$4b04d447,$d20d85fd,$a50ab56b
        dc.l    $35b5a8fa,$42b2986c,$dbbbc9d6,$acbcf940
        dc.l    $32d86ce3,$45df5c75,$dcd60dcf,$abd13d59
        dc.l    $26d930ac,$51de003a,$c8d75180,$bfd06116
        dc.l    $21b4f4b5,$56b3c423,$cfba9599,$b8bda50f
        dc.l    $2802b89e,$5f058808,$c60cd9b2,$b10be924
        dc.l    $2f6f7c87,$58684c11,$c1611dab,$b6662d3d
        dc.l    $76dc4190,$01db7106,$98d220bc,$efd5102a
        dc.l    $71b18589,$06b6b51f,$9fbfe4a5,$e8b8d433
        dc.l    $7807c9a2,$0f00f934,$9609a88e,$e10e9818
        dc.l    $7f6a0dbb,$086d3d2d,$91646c97,$e6635c01
        dc.l    $6b6b51f4,$1c6c6162,$856530d8,$f262004e
        dc.l    $6c0695ed,$1b01a57b,$8208f4c1,$f50fc457
        dc.l    $65b0d9c6,$12b7e950,$8bbeb8ea,$fcb9887c
        dc.l    $62dd1ddf,$15da2d49,$8cd37cf3,$fbd44c65
        dc.l    $4db26158,$3ab551ce,$a3bc0074,$d4bb30e2
        dc.l    $4adfa541,$3dd895d7,$a4d1c46d,$d3d6f4fb
        dc.l    $4369e96a,$346ed9fc,$ad678846,$da60b8d0
        dc.l    $44042d73,$33031de5,$aa0a4c5f,$dd0d7cc9
        dc.l    $5005713c,$270241aa,$be0b1010,$c90c2086
        dc.l    $5768b525,$206f85b3,$b966d409,$ce61e49f
        dc.l    $5edef90e,$29d9c998,$b0d09822,$c7d7a8b4
        dc.l    $59b33d17,$2eb40d81,$b7bd5c3b,$c0ba6cad
        dc.l    $edb88320,$9abfb3b6,$03b6e20c,$74b1d29a
        dc.l    $ead54739,$9dd277af,$04db2615,$73dc1683
        dc.l    $e3630b12,$94643b84,$0d6d6a3e,$7a6a5aa8
        dc.l    $e40ecf0b,$9309ff9d,$0a00ae27,$7d079eb1
        dc.l    $f00f9344,$8708a3d2,$1e01f268,$6906c2fe
        dc.l    $f762575d,$806567cb,$196c3671,$6e6b06e7
        dc.l    $fed41b76,$89d32be0,$10da7a5a,$67dd4acc
        dc.l    $f9b9df6f,$8ebeeff9,$17b7be43,$60b08ed5
        dc.l    $d6d6a3e8,$a1d1937e,$38d8c2c4,$4fdff252
        dc.l    $d1bb67f1,$a6bc5767,$3fb506dd,$48b2364b
        dc.l    $d80d2bda,$af0a1b4c,$36034af6,$41047a60
        dc.l    $df60efc3,$a867df55,$316e8eef,$4669be79
        dc.l    $cb61b38c,$bc66831a,$256fd2a0,$5268e236
        dc.l    $cc0c7795,$bb0b4703,$220216b9,$5505262f
        dc.l    $c5ba3bbe,$b2bd0b28,$2bb45a92,$5cb36a04
        dc.l    $c2d7ffa7,$b5d0cf31,$2cd99e8b,$5bdeae1d
        dc.l    $9b64c2b0,$ec63f226,$756aa39c,$026d930a
        dc.l    $9c0906a9,$eb0e363f,$72076785,$05005713
        dc.l    $95bf4a82,$e2b87a14,$7bb12bae,$0cb61b38
        dc.l    $92d28e9b,$e5d5be0d,$7cdcefb7,$0bdbdf21
        dc.l    $86d3d2d4,$f1d4e242,$68ddb3f8,$1fda836e
        dc.l    $81be16cd,$f6b9265b,$6fb077e1,$18b74777
        dc.l    $88085ae6,$ff0f6a70,$66063bca,$11010b5c
        dc.l    $8f659eff,$f862ae69,$616bffd3,$166ccf45
        dc.l    $a00ae278,$d70dd2ee,$4e048354,$3903b3c2
        dc.l    $a7672661,$d06016f7,$4969474d,$3e6e77db
        dc.l    $aed16a4a,$d9d65adc,$40df0b66,$37d83bf0
        dc.l    $a9bcae53,$debb9ec5,$47b2cf7f,$30b5ffe9
        dc.l    $bdbdf21c,$cabac28a,$53b39330,$24b4a3a6
        dc.l    $bad03605,$cdd70693,$54de5729,$23d967bf
        dc.l    $b3667a2e,$c4614ab8,$5d681b02,$2a6f2b94
        dc.l    $b40bbe37,$c30c8ea1,$5a05df1b,$2d02ef8d

        end
