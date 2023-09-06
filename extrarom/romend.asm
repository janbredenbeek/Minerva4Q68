* End of extrarom - should be the very last module linked!

        section extrarom_end

        xdef    rom_end

        dc.w    0       ; make sure it ends at an even address
rom_end equ     *

        end
