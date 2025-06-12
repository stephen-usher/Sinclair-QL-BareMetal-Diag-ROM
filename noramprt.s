;
; noram_print_chr
;
; Print a character to the serial port without RAM
;
;
; d0 - Character to send to the serial port
; a6 - return address
;
; Corrupted registers: d4
;

noram_print_chr:
	lea	zx83base,a4

	move.b	d0,zx83off_w_transdata(a4)	; Transmit the character.

nr_p_chr_wait:
;	btst.b	#2,zx83off_r_cstatus(a4)	; See if the transmit buffer is full
	move.b	zx83off_r_cstatus(a4),d4
	andi.b	#$02,d4
	cmpi.b	#$02,d4
	beq	nr_p_chr_wait	; If it is go around again until it's empty

nr_p_chr_end:

	jmp	(a6)

;
; noram_print_string
;
; Print a NULL terminated string to the serial port without RAM
;
;
; a5 - address of the start of the string
; a6 - return address
;
; Corrupted registers: d4, d6, a4
;

noram_print_string:
	lea	zx83base,a4

nr_p_string_loop1:
	move.l	#0,d6		; clear out d6
	move.b	(a5)+,d6	; Copy character into d6 and increment a5

	tst.b	d6		; Is it a NULL byte?
	beq	nr_p_string_end		; If so finish

	move.b	d6,zx83off_w_transdata(a4)	; Transmit the character.

nr_p_string_wait:
;	btst.b	#2,zx83off_r_cstatus(a4)	; See if the transmit buffer is full
	move.b	zx83off_r_cstatus(a4),d4
	andi.b	#$02,d4
	cmpi.b	#$02,d4
	beq	nr_p_string_wait	; If it is go around again until it's empty

	bra	nr_p_string_loop1	; Print the next character

nr_p_string_end:

	jmp	(a6)


;
; noram_print_string_scr
;
; Print a NULL terminated string to the serial port without RAM
;
; a0 - IO base address
; a5 - address of the start of the string
; a6 - return address
; d2 - character line offset from top
;
; d0, d2, , d3, d4, d5, d6, a1, a4 and a5 corrupted
;

noram_print_string_scr:
	lea	ramstart,a4
	mulu	#1280,d2	; d2 holds address offset of the character row
	add.l	d2,a4		; Add this to the start address

	move.l	#0,d0		; Set up the starting position to print onto screen
	move.l	d0,d5


nr_p_string_scr_loop1:
	move.l	#0,d6		; clear out d6
	move.b	(a5)+,d6	; Copy character into d6 and increment a5

	tst.b	d6		; Is it a NULL byte?
	beq	nr_p_string_scr_end		; If so finish

; Try to write to the screen

	mulu	#2,d0		; The X offset is two bytes so double d0

	sub.l	#31,d6		; Subtract 31 as the font table doesn't have non-printable characters.
	mulu	#10,d6		; Set the offset of the character in the font table (each entry is 10 bytes long)

	lea	font_char0,a1	; Load the start address of the font table into a1.
	add.l	d6,a1		; Point to the start of the character.

	move.l	#8,d3		; Set up the loop counter

nr_prt_chr_loop:
	move.b	#8,d4			; Set the d4 to be the offset to
	sub.b	d3,d4			; the current scanline of the character.
	move.b	(0,a1,d4),(0,a4,d0)	; Write the font byte to the screen.
	move.b	(0,a1,d4),(1,a4,d0)	; Write the font byte to the screen.
	add.l	#128,d0			; Increment the pointer to the next scanline.
	dbf	d3,nr_prt_chr_loop     ; Go around again if we haven't done all the lines.

	add.b	#1,d5			; Move the cursor to the right
	move.l	d5,d0

	bra	nr_p_string_scr_loop1	; Print the next character

nr_p_string_scr_end:
	mulu	#2,d0
	move.l	#8,d3
nrpdendlp:
	move.b	#$FF,(0,a4,d0)
	add.l	#128,d0
	dbf	d3,nrpdendlp

	jmp	(a6)
