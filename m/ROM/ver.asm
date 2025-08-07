	section rom

        include m_mincf

	xdef	vers_major
	xdef	vers_minor
	xdef	vers_sub

vers_major equ	'1'
vers_minor equ	'98'

        GENIF   Q68 = 0
vers_sub   equ	'j2'
        ENDGEN
        
        GENIF   Q68 <> 0
vers_sub   equ  'q2'
        ENDGEN

	end
