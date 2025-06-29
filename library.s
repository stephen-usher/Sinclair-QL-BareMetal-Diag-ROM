;*****************************************************************************
; Start of utility library functions.
;*****************************************************************************


;***
;
; read_clock_value - Read the ZX8302 clock.
;
; Returns low word in d0 and high word in d1
;
;***

read_clock_value:
	movem.l	d2-d7/a0-a6,-(SP)

	lea	zx83_r_clock,a0		; Load the address of the RTC base
	move.w	(a0),d1			; Copy the high word into d1
	move.w	2(a0),d0		; Copy the low word into d0

	movem.l	(SP)+,d2-d7/a0-a6
	rts

;***
;
; sleep - wait for a number of seconds.
;
; d0.w - Number of seconds to wait.
;
;***

sleep:
	movem.l	d0-d7/a0-a6,-(SP)

	move.w	d0,d3			; Counter for the number of seconds to wait.

	jsr	read_clock_value	; Read the clock values into d0 and d1

	move.w	d0,d2			; Copy the low word of the time to d2

sleep_loop1:
	jsr	read_clock_value	; Read the clock
	cmp.w	d0,d2			; Has it changed?
	beq	sleep_loop1		; No? Then read it again!
	subi.b	#1,d3			; Decrement our counter for the number times around the loop
	tst.b	d3			; Have we reached the end?
	beq	sleep_end		; If yes then finish
	move.w	d0,d2			; Update the "previous value" of the time
	bra	sleep_loop1		; Go around again
	
sleep_end:

	movem.l	(SP)+,d0-d7/a0-a6
	rts

;***
;
; prt_str - Send a string to both the serial port and the screen at the current
;	cursor position
;
;***

prt_str:
	jsr	ser_prt_str
	jsr	scr_prt_str
	rts

;***
;
; Print a string to the serial port.
;
; a0 - pointer to the NULL terminated string.
;
;***

ser_prt_str:
	movem.l	d0/a0-a1,-(SP)
ser_prt_str_loop1:
	cmpi.b	#0,(a0)			; Is it a NULL byte?
	beq	ser_prt_str_end		; If so finish
	move.b	(a0)+,d0		; Copy character into d0 and increment a1
	jsr	ser_prt_chr
	bra	ser_prt_str_loop1	; Print the next character

ser_prt_str_end:
	movem.l (SP)+,d0/a0-a1
	rts

;***
;
; prt_chr - print a character to both serial port and screen
;
; d0.b	-	character to be sent.
;
;***

prt_chr:
	jsr	ser_prt_chr
	jsr	scr_prt_chr
	rts

;***
;
; ser_prt_chr - send a character to the serial port
;
; d0 - character to be sent.
;
;*

ser_prt_chr:
	movem.l	d0-d6/a0-a6,-(SP)

	move.l	#0,d2

	lea	zx83base,a1	; Load the transmit register address into A1
	move.b	d0,zx83off_w_transdata(a1)			; Transmit the character.
ser_prt_chr_wait:
;	btst.b	#2,zx83off_r_cstatus(a1)		; See if the transmit buffer is full
	move.b	zx83off_r_cstatus(a1),d2
	andi.b	#2,d2
	cmpi.b	#2,d2
	beq	ser_prt_chr_wait	; If it is go around again until it's empty

	movem.l	(SP)+,d0-d6/a0-a6
	rts

;***
;
; Print a string to the screen from the current cursor position.
;
; A0 - pointer to the NULL terminated string.
;
;***

scr_prt_str:
	movem.l	d0-d6/a0-a6,-(SP)

	move.l	#0,d1			; Set the offset pointer to zero
	move.l	d1,d0			; Clear out d0 as well
scr_prt_str_loop1:
	move.b	(0,a0,d1),d0		; Copy the character from memory into d0
	tst.b	d0
	beq	scr_prt_str_end		; If the character is a NULL skip to the end
	add.l	#1,d1			; Increment the offset pointer

	jsr	scr_prt_chr		; Print the character

	bra	scr_prt_str_loop1	; Go around again for the next byte

scr_prt_str_end:
	movem.l	(SP)+,d0-d6/a0-a6
	rts

;***
;
; Print a character on the screen at the current cursor position and move the cursor on one position
;
; d0 - ASCII character to be printed
;
;***

scr_prt_chr:
	movem.l d0-d6/a0-a6,-(SP)

	lea	ramstart,a0	; Set the screen start pointer

	lea	sysvarbase,a1	; Obtain the current cursor position
	move.l	a1,a2		; d1 and d2 are working registers

	move.l	#0,d1		; Clear out d1, d2, d4, d5 and d6
	move.l	#0,d2
	move.l	#0,d4
	move.l	#0,d5
	move.l	#0,d6

	and.l	#$000000ff,d0	; Make sure that we only have the character in d0

	move.b	sysv_cur_x(a1),d1	; Load d1 with X character postion
	move.b	sysv_cur_y(a1),d2		; Load d2 with Y character position

	move.b	d1,d5		; Copy the X and Y positions into d5 and d6
	move.b	d2,d6

	andi.l	#$0000007F,d0	; Make sure that the data we're given holds just the character
	cmpi.b	#31,d0		; Is the character non-printable?
	blt	scr_prt_chr_special ; Jump to the special character decode routine if so.

	sub.l	#31,d0		; Subtract 31 as the font table doesn't have non-printable characters.
	mulu	#10,d0		; Set the offset of the character in the font table

	mulu	#2,d1		; The X offset is two bytes
	mulu	#1280,d2		; Each scan line is 128 bytes, so each character line is 1280 bytes
	add.l	d1,d2		; d2 should hold the offset address of the top byte of the character.

	lea	font_char0,a1	; Load the start address of the font table into a1.
	add.l	d0,a1		; Point to the start of the character.

	move.l	#8,d3		; Set up the loop counter

scr_prt_chr_loop:
	move.b	#8,d4			; Set the d4 to be the offset to
	sub.b	d3,d4			; the current scanline of the character.
	move.b	(0,a1,d4),(0,a0,d2)	; Write the font byte to the screen.
	move.b	(0,a1,d4),(1,a0,d2)	; Write the font byte to the screen.
	add.l	#128,d2			; Increment the pointer to the next scanline.
	dbf	d3,scr_prt_chr_loop     ; Go around again if we haven't done all the lines.

	add.b	#1,d5			; Move the cursor to the right
	cmp.b	#64,d5			; Test if we've gone off the right size of the screen.
	bne	scr_prt_chr_inc_jmp
	add.b	#1,d6			; If we have move the cursor down
	move.b	#0,d5			; and back to the left. (Wrap)

scr_prt_chr_inc_jmp:
	cmpi.l	#24,d6			; Are we below the bottom of the screen?
	blt	scr_prt_chr_nowrap
	move.l	#0,d6			; If so wrap to the top.
scr_prt_chr_nowrap:

scr_prt_chr_end:
	move.b	d5,d0			; Set the real cursor position variables
	move.b	d6,d1			; to match our shadow copies.
	jsr	scr_cur_pos

	movem.l	(SP)+,d0-d6/a0-a6
	rts

;
; 	Deal with control characters.
; 	We only worry about carrage return and line feed and backspace.
;

scr_prt_chr_special:
	cmpi.b	#$0d,d0			; Is it carrage return?
	bne	scr_prt_chr_ncr
	move.b	#0,d5			; If yes move the cursor to the left side
scr_prt_chr_ncr:
	cmpi.b	#$0a,d0			; Is it line feed?
	bne	scr_prt_chr_nlf
	add.b	#1,d6			; Move the cursor down.
scr_prt_chr_nlf:
	cmpi.b	#8,d0			; Is it backspace?
	bne	scr_prt_ret_special
	jsr	scr_cur_bsp_int		; Run the backspace routine

scr_prt_ret_special:
	bra	scr_prt_chr_inc_jmp	; Jump back to the main routine

	align 2

;***
;
; get_cur_pos - get the current cursor position.
;
; Returns
;
; d0.b	-	X position (0 - 63)
; d1.b	-	Y position (0 - 23)
;
;***

get_cur_pos:
	movem.l	d2-d7/a0-a6,-(SP)

	lea	sysvarbase,a0

	move.b	sysv_cur_x(a0),d0
	move.b	sysv_cur_y(a0),d1

	movem.l	(SP)+,d2-d7/a0-a6
	rts

;***
;
; cur_pos - Set the cursor position for both serial and screen
;
;***

cur_pos:
	jsr	ser_cur_pos
	jsr	scr_cur_pos
	rts

;***
;
; Send the ANSI codes for cursor position to the serial port.
;
; d0 - X position in character cells.
; d1 - Y position in character cells.
;
;***

ser_cur_pos:
	movem.l	d0-d1/a0-a6,-(SP)

	move.b	d0,d2
	move.b	d1,d3

	lea	escbrack,a0
	jsr	ser_prt_str

	move.b	d2,d0
	andi.w	#$00FF,d0

	jsr	itoa

	lea	workspace,a0

	jsr	ser_prt_str

	moveq	#$3b,d0
	jsr	ser_prt_chr
	
	move.w	d3,d0
	andi.w	#$00FF,d0

	jsr	itoa

	jsr	ser_prt_str

	moveq	#$48,d0
	jsr	ser_prt_chr
	
	movem.l	(SP)+,d0-d1/a0-a6
	rts

;***
;
; Set the screen cursor position
;
; d0 - X position in character cells.
; d1 - Y position in character cells.
;
;***

scr_cur_pos:
	movem.l	a0,-(SP)		; Save the registers we're going to modify

	lea	sysvarbase,a0		; Load the base system variable address into a0
	move.b	d0,sysv_cur_x(a0)	; Write the values to the system variables.
	move.b	d1,sysv_cur_y(a0)

	movem.l	(SP)+,a0		; Restore the registers.
	rts

;***
;
; cls - clear the screen on serial and screen
;
;***

cls:
	jsr	ser_cls
	jsr	scr_cls
	rts

;***
;
; ser_cls - Clear the terminal screen.
;
;***

ser_cls:
	movem.l	a0,-(SP)

	lea	clrandhome,a0
	jsr	ser_prt_str

	movem.l	(SP)+,a0
	rts

;***
;
; scr_cls - Clear the screen
;
;***

scr_cls:
	movem.l	d0-d7/a0-a6,-(SP)

	moveq	#0,d0
	moveq	#0,d1
	jsr	scr_cur_pos

	lea	ramstart,a0
	move.l	#$7fff,d0

scr_cls_loop:
	move.b	#0,(0,a0,d0)
	dbf	d0,scr_cls_loop

	movem.l	(SP)+,d0-d7/a0-a6
	rts

;***
;
; cur_bol - Set the cursor to the beginning of the line.
;
; Calls the routines to set the cursor at the beginning of the line for
; both the screen and the serial terminal.
;
;***

cur_bol:
	jsr	ser_cur_bol
	jsr	scr_cur_bol
	rts

;***
;
; ser_cur_bol - sets the cursor to the beginning of the line
;
; Just sends a carriage return to the serial port.
;
;***

ser_cur_bol:
	movem.l	d0,-(SP)

	moveq	#$0d,d0
	jsr	ser_prt_chr

	movem.l	(SP)+,d0
	rts

;***
;
; scr_cur_bol - set the screen cursor to the beginning of line.
;
;***

scr_cur_bol:
	movem.l	d0-d1,-(SP)

	jsr	get_cur_pos
	moveq	#0,d0
	jsr	scr_cur_pos

	movem.l	(SP)+,d0-d1
	rts

;***
;
; scr_cur_bsp - Backspace the cursor.
;
;***

scr_cur_bsp:
	movem.l	a0-a6/d0-d6,-(SP)

	jsr	get_cur_pos		; X position is in d0 and Y in d1

	move.b	d0,d5
	move.b	d1,d6

	jsr	scr_cur_bsp_int

	move.b	d5,d0
	move.b	d6,d1

	jsr	scr_cur_pos		; Set the cursor position in the system variables

	movem.l	(SP)+,a0-a6/d0-d6
	rts

;***
;
; scr_cur_bsp_int - Backspace the cursor.
;
;***

scr_cur_bsp_int:
	movem.l	a0-a6/d0-d1,-(SP)

	cmp.b	#0,d5			; Is the cursor at the beginning of a line?
	bne	scr_cur_bsp_skip1	; No? Then we don't have to do special stuff.

	moveq	#64,d5			; Move cursor one character beyond the end of the line (corrected later)

	cmp.b	#0,d6			; Is the cursor at the top of the screen?

	bne	scr_cur_bsp_skip2	; No, so we don't have to worry about wrapping.
	moveq	#24,d6

scr_cur_bsp_skip2:
	sub.b	#1,d6			; Move the cursor up one line

scr_cur_bsp_skip1:
	sub.b	#1,d5			; Move the cursor back one position


	movem.l	(SP)+,a0-a6/d0-d1
	rts

;***
;
; itoa - Convert a word length unsigned integer to a decimal ASCII string.
;
; a0 - Pointer to start of destination memory. It's up to the caller to
;	make it large enough.
; d0 - The word unsigned value.
;
;***

itoa:
	movem.l	d0-d5/a0-a6,-(SP)

	move.w	d0,d1		; Copy the value into d1 and d2
	move.w	d0,d2

	move.l	#-1,d3		; Add a counter for the number of charaters pushed onto stack

itoa_loop1:
	divu.w	#10,d1		; Find the modulus of 10 of the value and
	mulu	#10,d1		; store it in d2.
	sub.w	d1,d2
	and.l	#$FF,d2		; Mask off any unwanted bits, just in case


	add.b	#$30,d2		; Add the ASCII offset for the character '0'
	move.b	d2,-(SP)	; Push the character onto the stack
	addi.l	#1,d3		; Increment the number of character on the stack.

	divu.w	#10,d1		; Get d1 ready for the next deciman value.
	move.w	d1,d2		; Copy the value into d2 ready to go around again.

	tst.b	d1		; Is it zero? If so break out of the loop.
	bne	itoa_loop1

	move.l	#0,d4		; Set the offset in the string.

itoa_loop2:
	move.b	(SP)+,d5
	move.b	d5,(0,a0,d4)	; Pop the character off the stack and put it into the string

	addi.l	#1,d4		; Increment the offset in the string
	dbf	d3,itoa_loop2	; Loop around if there are more characters

	move.b	#0,(0,a0,d4)	; Write the NULL terminator to the end of the string

	movem.l	(SP)+,d0-d5/a0-a6
	rts

;*****************************************************************************
; ltoh	- translate a long to a hexadecimal string.
;
; a0 - Pointer to start of destination memory. It's up to the caller to
;	make it large enough.
; d0 - The long unsigned value.
;
;*****************************************************************************

ltohtable:	dc.b	"0123456789abcdef"

ltoh:
	movem.l	d0-d5/a0-a6,-(SP)

	lea	ltohtable,a1

; Byte 3

	move.l	d0,d1

	lsr.l	#8,d1		; Shift the next byte to the bottom.
	lsr.l	#8,d1
	lsr.l	#8,d1

	and.l	#$000000ff,d1	; Mask any higher bits.

	move.b	d1,d2		; Copy it to d2 so we can get the top nibble.
	lsr.b	#4,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3
	move.b	d1,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3

; Byte 2

	move.l	d0,d1

	lsr.l	#8,d1		; Shift the next byte to the bottom.
	lsr.l	#8,d1

	and.l	#$000000ff,d1	; Mask any higher bits.

	move.b	d1,d2		; Copy it to d2 so we can get the top nibble.
	lsr.b	#4,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3
	move.b	d1,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3

; Byte 1

	move.l	d0,d1

	lsr.l	#8,d1		; Shift the next byte to the bottom.

	and.l	#$000000ff,d1	; Mask any higher bits.

	move.b	d1,d2		; Copy it to d2 so we can get the top nibble.
	lsr.b	#4,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3
	move.b	d1,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3

; Byte 0

	move.l	d0,d1

	and.l	#$000000ff,d1	; Mask any higher bits.

	move.b	d1,d2		; Copy it to d2 so we can get the top nibble.
	lsr.b	#4,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3
	lsl.l	#8,d3		; shift it up one
	move.b	d1,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3


	move.b	#0,(a0)		; Add a NULL byte

	movem.l	(SP)+,d0-d5/a0-a6
	rts

;*****************************************************************************
; wtoh	- translate a word to a hexadecimal string.
;
; a0 - Pointer to start of destination memory. It's up to the caller to
;	make it large enough.
; d0 - The long unsigned value.
;
;*****************************************************************************

wtoh:
	movem.l	d0-d5/a0-a6,-(SP)

	lea	ltohtable,a1

; Byte 1

	move.l	d0,d1

	lsr.l	#8,d1		; Shift the next byte to the bottom.

	and.l	#$000000ff,d1	; Mask any higher bits.

	move.b	d1,d2		; Copy it to d2 so we can get the top nibble.
	lsr.b	#4,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3
	move.b	d1,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3

; Byte 0

	move.l	d0,d1

	and.l	#$000000ff,d1	; Mask any higher bits.

	move.b	d1,d2		; Copy it to d2 so we can get the top nibble.
	lsr.b	#4,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3
	lsl.l	#8,d3		; shift it up one
	move.b	d1,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3


	move.b	#0,(a0)		; Add a NULL byte

	movem.l	(SP)+,d0-d5/a0-a6
	rts

;*****************************************************************************
; btoh	- translate a byte to a hexadecimal string.
;
; a0 - Pointer to start of destination memory. It's up to the caller to
;	make it large enough.
; d0 - The long unsigned value.
;
;*****************************************************************************

btoh:
	movem.l	d0-d5/a0-a6,-(SP)

	lea	ltohtable,a1

	move.l	d0,d1

	and.l	#$000000ff,d1	; Mask any higher bits.

	move.l	d1,d2		; Copy it to d2 so we can get the top nibble.
	lsr.b	#4,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3
	lsl.l	#8,d3		; shift it up one
	move.b	d1,d2
	and.b	#$0f,d2		; Mask off the top nibble.
	move.b	(0,a1,d2),(a0)+	; Copy the character into the bottom of d3


	move.b	#0,(a0)		; Add a NULL byte

	movem.l	(SP)+,d0-d5/a0-a6
	rts


;*****************************************************************************
;
; twiddle_reset	- Reset the twiddler state.
;
;*****************************************************************************

twiddle_reset:
	movem.l	d0-d5/a0-a6,-(SP)

	lea	sysvarbase,a0		; Get the base of the system varibles into a0
	move.b	#0,sysv_cur_twd(a0)	; Set the twiddler state to zero

	movem.l	(SP)+,d0-d5/a0-a6
	rts

;*****************************************************************************
;
; prt_twiddle	- Reset the twiddler state.
;
;*****************************************************************************

twiddletxt:	dc.b	"|/-\"

prt_twiddle:
	movem.l	d0-d7/a0-a6,-(SP)


	move.l	#0,d0
	move.l	#0,d1

	lea	sysvarbase,a1		; Get the base of the system varibles into a0
	move.b	sysv_cur_twd(a1),d1	; Get the twiddler state.

	lea	twiddletxt,a2		; Get the beginning of the twiddler text into a2

	move.b	(0,a2,d1),d0		; Put the current twiddler character into d0
	jsr	prt_chr			; Print it.

	add.b	#1,d1			; Increment the pointer to the twiddler character
	cmp.b	#4,d1			; Is it beyond the end?
	blt	prt_twiddle_skip1	; No? We don't need to do anything.

	moveq	#0,d1			; It was too big, so reset it.

prt_twiddle_skip1:
	move.b	d1,sysv_cur_twd(a1)	; Update the system variable.

	move.l	#0,d0
	moveq	#8,d0			; Print a backspace.
	jsr	prt_chr
;	jsr	scr_cur_bsp

	movem.l	(SP)+,d0-d7/a0-a6
	rts

;*****************************************************************************
;
;	keycode2str	- Convert a keycode byte to a pointer to the keycode
;				name string.
;
;	Inputs:
;
;	d0.b	- keycode byte (valid values 0 - 63)
;
;	Returns:
;
;	a0	- Address of the string name for the keycode
;
;*****************************************************************************

keycode2str:
	movem.l	d0-d7/a1-a6,-(SP)

	lea	keycode_table,a1

	andi.l	#$3f,d0			; Make sure that we are in the range 0-63
	mulu.w	#4,d0

	move.l	(0,a1,d0),a0		; Copy the value with the byte offset in the table to a0

	movem.l	(SP)+,d0-d7/a1-a6
	rts

;*****************************************************************************
;
;	keymodifierstr	- Convert a keycode byte to a pointer to the keycode
;				name string.
;
;	NOTE: Uses workspace memory!
;
;	Inputs:
;
;	d0.b	- keycode modifier byte as returned by the IPC
;
;	Returns:
;
;	a0	- Address of the string name for the key modifier list
;
;*****************************************************************************

keymodifierstr:
	movem.l	d0-d7/a1-a6,-(SP)

	lea	workspace,a0

	move.b	#0,(a0)

	andi.l	#$07,d0			; We only care about the bottom three bits


;
; ALT
;

	btst	#0,d0
	beq	keymodifierstr_skip1

	lea	key_alt_txt,a1

keymodifierstr_loop1:
	cmpi.b	#0,(a1)
	beq	keymodifierstr_loop1_end
	move.b	(a1)+,(a0)+
	bra	keymodifierstr_loop1
keymodifierstr_loop1_end:

keymodifierstr_skip1:


;
; CTRL
;

	btst	#1,d0
	beq	keymodifierstr_skip2

	lea	key_ctl_txt,a1

keymodifierstr_loop2:
	cmpi.b	#0,(a1)
	beq	keymodifierstr_loop2_end
	move.b	(a1)+,(a0)+
	bra	keymodifierstr_loop2
keymodifierstr_loop2_end:

keymodifierstr_skip2:

;
; SHIFT
;

	btst	#2,d0
	beq	keymodifierstr_skip3

	lea	key_shift_txt,a1

keymodifierstr_loop3:
	cmpi.b	#0,(a1)
	beq	keymodifierstr_loop3_end
	move.b	(a1)+,(a0)+
	bra	keymodifierstr_loop3
keymodifierstr_loop3_end:

keymodifierstr_skip3:

	move.b	#0,(a0)

	lea	workspace,a0

	movem.l	(SP)+,d0-d7/a1-a6
	rts

;*****************************************************************************
;
;	read_keyboard	-	Read the keyboard queue
;
;	Inputs:
;
;	a0	-	Address of memory into which keydefs are copied.
;				Must be at least 64 bytes.
;
;	Returns:
;
;	d0.b	-	Number of keydefs returned
;
;*****************************************************************************

read_keyboard:
	movem.l	d1-d7/a0-a6,-(SP)

	move.l	d0,d1

	move.l	#0,d0			; Clear out d0 completely.

	lea	sysvarbase,a3
	move.b	#1,sysv_idisable(a3)	; Disable IPC interrupt handling

	lea	kbd_buf_offset,a1
	move.w	(a1),d0
	move.l	d0,d1
	lsl.w	#1,d1

	cmpi.b	#0,d0			; Are there any keydefs in the buffer?
	beq	read_keyboard_end

	lea	kbd_buffer,a2

	subi.b	#1,d1

read_keyboard_loop1:
	move.w	(a2)+,(a0)+
	dbf	d1,read_keyboard_loop1

	move.w	#0,(a1)			; Reset the buffer count.

read_keyboard_end:
	move.b	#0,sysv_idisable(a3)	; Re-enable IPC interrupt handling

	movem.l	(SP)+,d1-d7/a0-a6
	rts

;*****************************************************************************
;
;	read_ser1	-	Read the keyboard queue
;
;	Inputs:
;
;	a0	-	Address of memory into which data bytes are copied.
;				Must be at least 1K bytes.
;
;	Returns:
;
;	d0.b	-	Number of bytes returned
;
;*****************************************************************************

read_ser1:
	movem.l	d1-d7/a0-a6,-(SP)

	move.l	#0,d0			; Clear out d0 completely.

	lea	sysvarbase,a3
	move.b	#1,sysv_idisable(a3)	; Disable IPC interrupt handling

	lea	ser1_buf_count,a1
	move.w	(a1),d0

	cmpi.b	#0,d0			; Are there any bytes in the buffer?
	beq	read_ser1_end

	lea	ser1_buffer,a2

	subi.b	#1,d0
	andi.w	#$3ff,d0		; Make sure it's within the maximum size
	move.l	d0,d1			; Copy to the loop variable
	addi.w	#1,d0			; increase it back to the correct value

read_ser1_loop1:
	move.b	(a2)+,(a0)+
	dbf	d1,read_ser1_loop1

	move.w	#0,(a1)			; Reset the buffer count.

read_ser1_end:
	move.b	#0,sysv_idisable(a3)	; Re-enable IPC interrupt handling

	movem.l	(SP)+,d1-d7/a0-a6
	rts

;*****************************************************************************
;
;	read_ser2	-	Read the keyboard queue
;
;	Inputs:
;
;	a0	-	Address of memory into which data bytes are copied.
;				Must be at least 1K bytes.
;
;	Returns:
;
;	d0.b	-	Number of bytes returned
;
;*****************************************************************************

read_ser2:
	movem.l	d1-d7/a0-a6,-(SP)

	move.l	#0,d0			; Clear out d0 completely.

	lea	sysvarbase,a3
	move.b	#1,sysv_idisable(a3)	; Disable IPC interrupt handling

	lea	ser2_buf_count,a1
	move.w	(a1),d0

	cmpi.b	#0,d0			; Are there any bytes in the buffer?
	beq	read_ser2_end

	lea	ser2_buffer,a2

	subi.b	#1,d0
	andi.w	#$3ff,d0		; Make sure it's within the maximum size
	move.l	d0,d1			; Copy to the loop variable
	addi.w	#1,d0			; increase it back to the correct value

read_ser2_loop1:
	move.b	(a2)+,(a0)+
	dbf	d1,read_ser2_loop1

	move.w	#0,(a1)			; Reset the buffer count.

read_ser2_end:
	move.b	#0,sysv_idisable(a3)	; Re-enable IPC interrupt handling

	movem.l	(SP)+,d1-d7/a0-a6
	rts

;*****************************************************************************
; End of utility library functions.
;*****************************************************************************
