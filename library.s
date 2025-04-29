;*****************************************************************************
; Start of utility library functions.
;*****************************************************************************

;***
;
; ipc_write_nibble	- Write a nibble to the IPC.
;
; Parameters:
;
; d0.b	-	Nibble (big endian, lower four bits)
; d1.w	-	Timeout
;
; Returned values:
;
; d1.w	-	Error if zero.
;
; d0.b corrupted
;***

ipc_write_nibble:
	movem.l	d2-d7/a0-a6,-(SP)

	move.b	d0,d2			; Copy the data into our register
	moveq	#3,d3			; Set the bit counter

ipc_write_nibble_loop1:
	move.b	d2,d0			; Copy the data byte into d0
	rol.b	#1,d2			; Shift d2 down by one bit
	andi.b	#%00001000,d0			; Mask off the other bits
	ror.b	#3,d0
	jsr	ipc_write_bit		; Write the bit to the IPC
	tst.w	d1			; Test if write failed
	beq	ipc_write_nibble_loop1_end
	dbf	d3,ipc_write_nibble_loop1	; If no timeout and we still have bits to process, go around the loop

ipc_write_nibble_loop1_end:

	movem.l	(SP)+,d2-d7/a0-a6
	rts

;***
;
; ipc_write_byte	- Write a byte to the IPC.
;
; Parameters:
;
; d0.b	-	Byte (big endian)
; d1.w	-	Timeout
;
; Returned values:
;
; d1.w	-	Error if zero.
;
; d0.b corrupted
;***

ipc_write_byte:
	movem.l	d2-d7/a0-a6,-(SP)

	move.b	d0,d2			; Copy the data into our register

	jsr	ipc_write_nibble

	move.b	d2,d0
	ror.b	#4,d0

	jsr	ipc_write_nibble
	
	movem.l	(SP)+,d2-d7/a0-a6
	rts


;***
;
; ipc_write_word	- Write a word to the IPC.
;
; Parameters:
;
; d0.w	-	Word (big endian)
; d1.w	-	Timeout
;
; Returned values:
;
; d1.w	-	Error if zero.
;
; d0.b corrupted
;***

ipc_write_word:
	movem.l	d2-d7/a0-a6,-(SP)

	move.b	d0,d2			; Copy the data into our register
	moveq	#15,d3			; Set the bit counter

ipc_write_word_loop1:
	move.w	d2,d0			; Copy the data byte into d0
	rol.w	#1,d2			; Shift d2 down by one bit
	andi.w	#$4000,d0		; Mask off the other bits
	ror.w	#7,d0			; Shift the top bit down to bit 0
	ror.w	#6,d0			; Shift the top bit down to bit 0
	jsr	ipc_write_bit		; Write the bit to the IPC
	tst.w	d1			; Check for a timeout
	beq	ipc_write_word_loop1_end	; Error - abort
	dbf	d3,ipc_write_word_loop1	; If no timeout and we still have bits to process, go around the loop

ipc_write_word_loop1_end

	movem.l	(SP)+,d2-d7/a0-a6
	rts

;***
;
; ipc_write_bit - Write a bit to the IPC with timeout.
;
; d0.b	-	Bit to be written in bit 0.
; d1.w	-	Timeout in number of retries reading the status bit.
;		(Must be given)
;
; Returned values.
;
; d1.w	-	Zero if timeout error otherwise undefined.
;
; Notes:
;
; Each bit of data has to be enclosed in a "packet" starting with 0 and ending in 1
;
; i.e. We have to send a zero bit, followed by the bit value, followed by a 1
;
;***

ipc_write_bit:
	movem.l	d2-d7/a0-a6,-(SP)

	andi.b	#%00000001,d0		; Mask off any erroneous bits.

	lea	zx83_w_ipcwreg,a0	; Load the ZX8302 registers into a0 and a1
	lea	zx83_r_cstatus,a1

	rol.b	#1,d0			; Move the data bit to be bit 1
	or.b	#%00001100,d0		; Or it with the rest of the required register content.

	move.w	d1,d4

	move.b	d0,(a0)		; Write a zero data bit to the IPC Write register.

ipc_write_bit_loop1:			; Loop around checking the status bit (6)
	move.b	(a1),d3
	btst	#6,d3
	beq	ipc_write_bit_loop1_end ; if it's zero then exit loop
	dbf	d4,ipc_write_bit_loop1	; Go around d1.w + 1 times.

ipc_write_bit_loop1_end:

	addi.w	#1,d4			; Add 1 to d4. If the loop has completed then this will be -1
					; We need it to be 0 as an error condition.
	move.w	d4,d1			; update the return value.

	movem.l	(SP)+,d2-d7/a0-a6
	rts

;***
;
; ipc_read_byte	-	Read a byte from the IPC with timeout.
;
; Parameters
;
; d1.w	-	Timeout value. Must be set.
;
; Returned values
;
; d0.b	-	Returned byte
; d1.b	-	Error condition, zero if error. Returned byte invalid.
;
;***

ipc_read_byte:
	movem.l	d2-d7/a0-a6,-(SP)

	move.l	#0,d4			; Clear out the register holding the returned value.

	move.w	#7,d3			; Set up the loop counter

ipc_read_byte_loop1:
	jsr	ipc_read_bit		; Read a bit from the IPC
	rol.b	d4			; Shift the target byte up by one bit
	add.b	d0,d4			; Add the returned bit value
	tst.b	d1			; Test if there was an error.
	dbne	d3,ipc_read_byte_loop1	; If no error and counter > -1 then got around

	move.b	d4,d0			; Copy the returned byte value into d0 for return.

	movem.l	(SP)+,d2-d7/a0-a6
	rts
;***
;
; ipc_read_nibble	-	Read a nibble from the IPC with timeout.
;
; Parameters
;
; d1.w	-	Timeout value. Must be set.
;
; Returned values
;
; d0.b	-	Returned byte
; d1.b	-	Error condition, zero if error. Returned byte invalid.
;
;***

ipc_read_nibble:
	movem.l	d2-d7/a0-a6,-(SP)

	move.l	#0,d4			; Clear out the register holding the returned value.

	move.w	#3,d3			; Set up the loop counter

ipc_read_nibble_loop1:
	jsr	ipc_read_bit		; Read a bit from the IPC
	rol.b	d4			; Shift the target byte up by one bit
	add.b	d0,d4			; Add the returned bit value
	tst.b	d1			; Test if there was an error.
	beq	ipc_read_nibble_loop1_end
	dbf	d3,ipc_read_nibble_loop1	; If no error and counter > -1 then got around

ipc_read_nibble_loop1_end

	move.b	d4,d0			; Copy the returned byte value into d0 for return.

	movem.l	(SP)+,d2-d7/a0-a6
	rts

;***
;
; ipc_read_bit	- Read a bit from the IPC with timeout.
;
; Parameters
;
; d1.w	-	Timeout in number of retries reading the status register.
;		(Must be given)
; Returns
;
; d0.b	-	Bit returned in bit 0 of the byte.
; d1.w	-	Timed out if 0 otherwise undefined.
;
;***

ipc_read_bit:
	movem.l	d2-d7/a0-a6,-(SP)

	lea	zx83_r_cstatus,a0	; Load the address of the communication status register into a0
	lea	zx83_w_ipcwreg,a1	; Load the address of the write data register into a1

	move.b	#%00001110,(a1)		; Ask for data.

ipc_read_bit_loop1:
	move.b	(a0),d2			; Get the contents of the communications status register into d2
	btst	#6,d2			; Is the IPC acknowledge bit set?
	beq	ipc_read_bit_loop1_end	; If the bit is cleared it's acknowledged, exit loop
	dbf	d1,ipc_read_bit_loop1	; If d1 is > -1 go around again.

ipc_read_bit_loop1_end:

	addi.w	#1,d1			; If the timeout failed then d1 will be -1, so add 1.

	andi.b	#%1000000,d2		; Mask off all the bits we're not interested in.
	ror.b	#7,d2			; Move it to bit 0
	move.b	d2,d0			; Copy it to d0

	movem.l	(SP)+,d2-d7/a0-a6
	rts

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
	movem.l	a1,-(SP)

	lea	zx83_w_transdata,a1	; Load the transmit register address into A1
	move.b	d0,(a1)			; Transmit the character.
ser_prt_chr_wait:
	btst.b	#2,$20(a1)		; See if the transmit buffer is full
	bne	ser_prt_chr_wait	; If it is go around again until it's empty

	movem.l	(SP)+,a1
	rts

;***
;
; Print a string to the screen from the current cursor position.
;
; A0 - pointer to the NULL terminated string.
;
;***

scr_prt_str:
	movem.l	d0-d1,-(SP)

	move.l	#0,d1			; Set the offset pointer to zero
scr_prt_str_loop1:
	move.b	(0,a0,d1),d0		; Copy the character from memory into d0
	tst.b	d0
	beq	scr_prt_str_end		; If the character is a NULL skip to the end
	add.l	#1,d1			; Increment the offset pointer

	jsr	scr_prt_chr		; Print the character

	bra	scr_prt_str_loop1	; Go around again for the next byte

scr_prt_str_end:
	movem.l	(SP)+,d0-d1
	rts

;***
;
; Print a character on the screen at the current cursor position and move the cursor on one position
;
; d0 - ASCII character to be printed
;
;***

scr_prt_chr:
	movem.l d0-d6/a0-a2,-(SP)

	lea	ramstart,a0	; Set the screen start pointer

	lea	sysvarbase,a1	; Obtain the current cursor position
	move.l	a1,a2		; d1 and d2 are working registers

	move.l	#0,d1		; Clear out d1, d2, d5 and d6
	move.l	#0,d2
	move.l	#0,d5
	move.l	#0,d6

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
	move.l	d1,d3			; Check that we've not gone off the right of the screen (64 characters wide).
	sub.l	#63,d3
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

	movem.l	(SP)+,d0-d6/a0-a2
	rts

;
; 	Deal with control characters.
; 	We only worry about carrage return and line feed.
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
	move.l	#$3fff,d0

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
; End of utility library functions.
;*****************************************************************************
