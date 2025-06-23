;*******************************************************************
;
; IPC communcation routines.
;
;*******************************************************************

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
;
; A bibble with bits 1234 need to be sent in the order 2142
;
;***

ipc_write_nibble:
	movem.l	d0/d2-d6/a0-a5,-(SP)

	move.b	d0,d2		; Bit 4
	move.b	d0,d3		; Bit 3
	move.b	d0,d4		; Bit 2
	move.b	d0,d5		; Bit 1

	andi.b	#%00000001,d2
	andi.b	#%00000010,d3
	andi.b	#%00000100,d4
	andi.b	#%00001000,d5

	lsr.b	#1,d3
	lsr.b	#2,d4
	lsr.b	#3,d5

	move.b	d5,d0
	jsr	ipc_write_bit
	move.b	d4,d0
	jsr	ipc_write_bit
	move.b	d3,d0
	jsr	ipc_write_bit
	move.b	d2,d0
	jsr	ipc_write_bit

	movem.l	(SP)+,d0/d2-d6/a0-a5
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
	movem.l	d0/d2-d6/a0-a5,-(SP)

	move.b	d0,d2			; Copy the data into our register

	move.b	d2,d0
	lsr.b	#4,d0
	andi.b	#$0f,d0

	jsr	ipc_write_nibble

	move.b	d2,d0
	andi.b	#$0f,d0

	jsr	ipc_write_nibble
	

	movem.l	(SP)+,d0/d2-d6/a0-a5
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
	movem.l	d0/d2-d6/a0-a5,-(SP)

	move.w	d0,d2			; Copy the data into our register

	move.w	d2,d0
	lsr.w	#8,d0
	jsr	ipc_write_byte

	move.w	d2,d0
	jsr	ipc_write_byte

	movem.l	(SP)+,d0/d2-d6/a0-a5
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
	movem.l	d0/d2-d6/a0-a5,-(SP)

	lea	zx83_w_ipcwreg,a0	; Load the ZX8302 registers into a0 and a1
	lea	zx83_r_cstatus,a1

	lsl.b	#1,d0			; Move the data bit to be bit 1
	andi.b	#%00000010,d0		; Mask off any erroneous bits.

	or.b	#%00001100,d0		; Or it with the rest of the required register content.

	move.w	d1,d4

	move.b	d0,(a0)		; Write a zero data bit to the IPC Write register.

ipc_write_bit_loop1:			; Loop around checking the status bit (6)
	move.b	(a1),d3
	andi.b	#$40,d3
	cmpi.b	#$40,d3
	bne	ipc_write_bit_loop1_end ; if it's zero then exit loop
	dbf	d4,ipc_write_bit_loop1	; Go around d1.w + 1 times.

ipc_write_bit_loop1_end:

	addi.w	#1,d4			; Add 1 to d4. If the loop has completed then this will be -1
					; We need it to be 0 as an error condition.
	move.w	d4,d1			; update the return value.

	movem.l	(SP)+,d0/d2-d6/a0-a5
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
	movem.l	d2-d6/a0-a5,-(SP)

	move.l	#0,d4			; Clear out the register holding the returned value.
	move.l	d4,d5

	move.w	#3,d3			; Set up the loop counter

ipc_read_byte_loop1:
	jsr	ipc_read_bit		; Read a bit from the IPC
	lsl.b	#1,d5			; Shift the target byte up by one bit
	add.b	d0,d5			; Add the returned bit value
	dbf	d3,ipc_read_byte_loop1	; If no error and counter > -1 then got around

	move.w	#3,d3			; Set up the loop counter

ipc_read_byte_loop2:
	jsr	ipc_read_bit		; Read a bit from the IPC
	lsl.b	#1,d4			; Shift the target byte up by one bit
	add.b	d0,d4			; Add the returned bit value
	dbf	d3,ipc_read_byte_loop2	; If no error and counter > -1 then got around

	lsl.b	#4,d5			; Shift it to the higher byte
	add.b	d5,d4			; Put the second byte on the top

	move.b	d4,d0			; Copy the returned byte value into d0 for return.

	movem.l	(SP)+,d2-d6/a0-a5
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
	lsl.b	d4			; Shift the target byte up by one bit
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
	movem.l	d2-d6/a0-a5,-(SP)

	move.l	#0,d4
	move.l	d4,d2

	move.w	d1,d3

	lea	workspace,a0
	lea	zx83base,a2	; Load the address of the communication status register into a0
	lea	zx83_w_ipcwreg,a1	; Load the address of the write data register into a1

	move.b	#%00001110,(a1)		; Ask for data.

ipc_read_bit_loop1:
	move.b	zx83off_r_cstatus(a2),d2			; Get the contents of the communications status register into d2
	move.b	zx83off_r_cstatus(a2),d2			; Twice... for Reasons(tm)
	move.l	d2,d4
	andi.b	#$40,d2
	cmpi.b	#$40,d2			; Is the IPC acknowledge bit set?
	bne	ipc_read_bit_loop1_end	; If the bit is cleared it's acknowledged, exit loop
	dbf	d1,ipc_read_bit_loop1	; If d1 is > -1 go around again.

ipc_read_bit_loop1_end:

	addi.w	#1,d1			; If the timeout failed then d1 will be -1, so add 1.

	andi.b	#%10000000,d4		; Mask off all the bits we're not interested in.
	lsr.b	#7,d4
	move.b	d4,d0			; Copy it to d0

	movem.l	(SP)+,d2-d6/a0-a5
	rts

;***
;
; ipc_read_keyboard	- Read the IPC keyboard buffer and put into our buffer.
;
; Protocol is:
; Send command nibble 8
; Read return nibble containing:
;	1 bit	-	Last keydef still held
;	3 bits	-	Count of waiting keydefs.
;
; Then each keydef contains:
;
;	nibble	-	Status:		(Lost Keys)(Shift)(Ctrl)(Alt)
;	byte	-	Keyrow/column:	2 bits ignore, 3 bits Column, 3 bits Row
;***

ipc_read_keyboard:
	movem.l	d0-d7/a0-a6,-(SP)

; d0 holds the status byte... so we can obtain information about if a key is held down

	move.l	#0,d6
	btst	#3,d0
	beq	ipc_read_keyboard_notheld
	move.l	#$8,d6
ipc_read_keyboard_notheld:

	lea	kbd_buffer,a0	; Put the base address of the keyboard buffer into a0
	lea	kbd_buf_offset,a1
	move.w	(a1),d4			; Load the current keyboard buffer counter into d4

	move.w	#$ffff,d1

	move.l	#8,d0
	jsr	ipc_write_nibble

	jsr	ipc_read_nibble

	andi.b	#$07,d0

	cmpi.b	#0,d0
	beq	ipc_read_keyboard_nokeydefs

	subi.b	#1,d0			; Reduce the count by one as that's what's required for the loop counter

	move.l	d0,d5			; d5 holds the number of keydefs to read.

	move.l	#$0,d2				; Clear out our work registers
	move.l	d2,d3
	
ipc_read_keyboard_loop1:
	jsr	ipc_read_nibble
	move.b	d0,d2			; Copy the status nibble into d2
	or.b	d6,d2			; Merge the held bit into d2
	jsr	ipc_read_byte
	move.b	d0,d3			; Copy the key column/row into d3

	cmpi.b	#64,d4			; Have we reached the end of our buffer?
	bge	ipc_read_keyboard_skip_save	; Yes? Don't save the information.

	move.b	d2,(0,a0,d4)		; copy the status
	addi.l	#1,d4
	move.b	d3,(0,a0,d4)		; copy the key data
	addi.l	#1,d4

ipc_read_keyboard_skip_save:

	dbf	d5,ipc_read_keyboard_loop1

	move.w	d4,(a1)

ipc_read_keyboard_nokeydefs:
	movem.l	(SP)+,d0-d7/a0-a6
	rts

;***
;	ipc_read_ser1	- Read the IPC serial buffer into our buffer
;***
ipc_read_ser1:
	movem.l	d0-d7/a0-a6,-(SP)

	lea	ser1_buffer,a0	; Put the base address of the ser1 buffer into a0
	lea	ser1_buf_count,a1
	lea	sysvarbase,a2
	addi.l	#1,sysv_intcount1(a2)

	move.w	(a1),d4			; Load the current ser1 buffer counter into d4

	move.w	#$0fff,d1

	move.l	#6,d0
	jsr	ipc_write_nibble

	jsr	ipc_read_byte

	move.l	d0,d5			; Copy the status/count into d5
	andi.b	#%00111111,d5		; Mask out the status bits
	cmpi.b	#0,d5			; Are there any data bytes to read? 
	beq	ipc_read_ser1_nodata

	subi.b	#1,d5			; Reduce the number by one for our loop counter.

ipc_read_ser1_loop1:
	jsr	ipc_read_byte
	cmpi.w	#1023,d4		; Are we at the end of the buffer.
	bge	ipc_read_ser1_skipbyte	; Yes? just drop the byte.

	move.b	d0,(0,a0,d4)		; Copy the byte into the buffer
	addi.w	#1,d4			; Increment the count.

ipc_read_ser1_skipbyte:
	dbf	d5,ipc_read_ser1_loop1

	move.w	d4,(a1)			; Copy back the byte counter

ipc_read_ser1_nodata:
	movem.l	(SP)+,d0-d7/a0-a6
	rts

;***
;	ipc_read_ser2	- Read the IPC serial buffer into our buffer
;***
ipc_read_ser2:
	movem.l	d0-d7/a0-a6,-(SP)

	lea	ser2_buffer,a0	; Put the base address of the ser2 buffer into a0
	lea	ser2_buf_count,a1
	lea	sysvarbase,a2
	addi.l	#1,sysv_intcount2(a2)

	move.w	(a1),d4			; Load the current ser2 buffer counter into d4

	move.w	#$0fff,d1

	move.l	#7,d0
	jsr	ipc_write_nibble

	jsr	ipc_read_byte

	move.l	d0,d5			; Copy the status/count into d5
	andi.b	#%00111111,d5			; Mask out the status bits
	cmpi.b	#0,d5			; Are there any data bytes to read? 
	beq	ipc_read_ser2_nodata

	subi.b	#1,d5			; Reduce the number by one for our loop counter.

ipc_read_ser2_loop1:
	jsr	ipc_read_byte
	cmpi.w	#1023,d4		; Are we at the end of the buffer.
	bge	ipc_read_ser2_skipbyte	; Yes? just drop the byte.

	move.b	d0,(0,a0,d4)		; Copy the byte into the buffer
	addi.w	#1,d4			; Increment the count.

ipc_read_ser2_skipbyte:
	dbf	d5,ipc_read_ser2_loop1

	move.w	d4,(a1)			; Copy back the byte counter

ipc_read_ser2_nodata:
	movem.l	(SP)+,d0-d7/a0-a6
	rts

;*****************************************************************************
;
;	open_ser1	-	Open ser1
;
;	Inputs:
;
;	None
;
;	Returns:
;
;	None
;
;*****************************************************************************

open_ser1:
	movem.l	d0-d7/a0-a6,-(SP)
	move.w	#$0FFF,d1
	lea	workspace,a0

	lea	sysvarbase,a2
	lea	ser1_buf_count,a3

	move.w	#0,(a3)			; 	Clear the buffer counter.

	move.l	#0,d0

	move.b	#1,sysv_idisable(a2)	;	Disable IPC interrupt handling

; Open ser1

	move.b	#$2,d0
	jsr	ipc_write_nibble

	move.b	#0,sysv_idisable(a2)	;	Re-enable IPC interrupt handling


	movem.l	(sp)+,d0-d7/a0-a6
	rts

;*****************************************************************************
;
;	open_ser2	-	Open ser2
;
;	Inputs:
;
;	None
;
;	Returns:
;
;	None
;
;*****************************************************************************

open_ser2:
	movem.l	d0-d7/a0-a6,-(SP)
	move.w	#$0FFF,d1
	lea	workspace,a0

	lea	sysvarbase,a2
	lea	ser2_buf_count,a3

	move.w	#0,(a3)			; 	Clear the buffer counter.

	move.l	#0,d0

	move.b	#1,sysv_idisable(a2)	;	Disable IPC interrupt handling

; Open ser2

	move.b	#$3,d0
	jsr	ipc_write_nibble

	move.b	#0,sysv_idisable(a2)	;	Re-enable IPC interrupt handling


	movem.l	(sp)+,d0-d7/a0-a6
	rts

;*****************************************************************************
;
;	close_ser1	-	Close ser1
;
;	Inputs:
;
;	None
;
;	Returns:
;
;	None
;
;*****************************************************************************

close_ser1:
	movem.l	d0-d7/a0-a6,-(SP)
	move.w	#$0FFF,d1
	lea	workspace,a0

	lea	sysvarbase,a2
	lea	ser1_buf_count,a3

	move.w	#0,(a3)			; 	Clear the buffer counter.

	move.l	#0,d0

	move.b	#1,sysv_idisable(a2)	;	Disable IPC interrupt handling

; Close ser1

	move.b	#$4,d0
	jsr	ipc_write_nibble

	move.b	#0,sysv_idisable(a2)	;	Re-enable IPC interrupt handling


	movem.l	(sp)+,d0-d7/a0-a6
	rts

;*****************************************************************************
;
;	close_ser2	-	Close ser2
;
;	Inputs:
;
;	None
;
;	Returns:
;
;	None
;
;*****************************************************************************

close_ser2:
	movem.l	d0-d7/a0-a6,-(SP)
	move.w	#$0FFF,d1
	lea	workspace,a0

	lea	sysvarbase,a2
	lea	ser2_buf_count,a3

	move.w	#0,(a3)			; 	Clear the buffer counter.

	move.l	#0,d0

	move.b	#1,sysv_idisable(a2)	;	Disable IPC interrupt handling

; Close ser2

	move.b	#$5,d0
	jsr	ipc_write_nibble

	move.b	#0,sysv_idisable(a2)	;	Re-enable IPC interrupt handling


	movem.l	(sp)+,d0-d7/a0-a6
	rts
;*****************************************************************************
;
;	set_imput_baud
;
;	Inputs:
;
;	d0.b		- Baud rate encoded as per IPC and ZX8302
;
;	Returns:
;
;	None
;
;*****************************************************************************

set_input_baud:
	movem.l	d0-d7/a0-a6,-(SP)

	move.w	#$0FFF,d1

	lea	sysvarbase,a2
	move.b	#1,sysv_idisable(a2)	;	Disable IPC interrupt handling

	move.l	#0,d2
	move.b	d0,d2

; Set the baud rate

	move.b	#$d,d0
	jsr	ipc_write_nibble

	move.b	d2,d0
	jsr	ipc_write_nibble

	move.b	#0,sysv_idisable(a2)	;	Re-enable IPC interrupt handling
	movem.l	(SP)+,d0-d7/a0-a6
	rts
