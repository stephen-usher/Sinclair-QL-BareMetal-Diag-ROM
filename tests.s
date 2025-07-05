
;*****************************************************************************
;
; Start of diagnostic routines.
;
;*****************************************************************************

;***
;
; init_ipc_test - Run the initial tests on the IPC.
;
;***

	align 2

init_ipc_test:
	movem.l	d1-d7/a0-a6,-(SP)

	lea	sysvarbase,a5

	move.b	#1,sysv_idisable(a5)	; And flag this in the system variables.

	lea	init_ipc_start_txt,a0
	jsr	prt_str

	lea	testingtxt,a0
	jsr	prt_str

	lea	init_ipc_reset_txt,a0
	jsr	prt_str

	jsr	ipc_reset

	tst.b	d1
	beq	init_ipc_test_fail

	lea	completedtxt,a0
	jsr	prt_str

	lea	init_ipc_selftest_txt,a0
	jsr	prt_str

	jsr	ipc_selftest

	tst.b	d1
	beq	init_ipc_test_fail

	lea	completedtxt,a0
	jsr	prt_str

	lea	init_ipc_beeptest_txt,a0
	jsr	prt_str

	jsr	init_ipc_beep

	lea	completedtxt,a0
	jsr	prt_str

	move.b	#0,sysv_idisable(a5)		; Remove the no interrupt flag in the system variables

	lea	init_ipc_keyreadtest_txt,a0
	jsr	prt_str

	jsr	init_ipc_readkey

	jsr	init_ipc_serial

	movem.l	(SP)+,d1-d7/a0-a6
	rts

init_ipc_test_fail:
	lea	failedtxt,a0
	jsr	prt_str

	move.l	-1,d0

	movem.l	(SP)+,d1-d7/a0-a6
	rts

;***
;
; ipc_reset - Attempt to reset the IPC
;
;***

ipc_reset:
	move.w	#$0FFF,d1
	move.b	#0,d0
	jsr	ipc_write_nibble
	rts

;***
;
; ipc_selfttest - Runt eh IPC selftest and report result
;
;***

ipc_selftest:
	movem.l	d2-d7/a0-a6,-(SP)

	move.l	#$1a,d2

	move.l	#$0FFF,d1

	move.b	#$0F,d0
	jsr	ipc_write_nibble

	move.b	d2,d0
	jsr	ipc_write_byte

	jsr	ipc_read_byte

	andi.w	#$00FF,d0
	move.b	d0,d3

	lea	wrotehextxt,a0
	jsr	prt_str

	move.b	d2,d0
	lea	workspace,a0
	jsr	btoh
	jsr	prt_str

	lea	readhextxt,a0
	jsr	prt_str

	move.b	d3,d0
	lea	workspace,a0
	jsr	btoh
	jsr	prt_str

	lea	spacecolonspacetxt,a0
	jsr	prt_str

	moveq	#0,d1
	cmp.b	d2,d3
	bne	ipc_selftest_end
	addi.b	#1,d1

ipc_selftest_end:

	movem.l	(SP)+,d2-d7/a0-a6
	rts

;***
;
; init_ipc_beep - Run try to tell the IPC to beep
;
;***

init_ipc_beep:
	move.w	#$0FFF,d1

	move.l	#$0a,d0			; Command A
	jsr	ipc_write_nibble

	move.b	#$12,d0			; Pitch 1
	jsr	ipc_write_byte

	move.b	#$34,d0			; Pitch 2
	jsr	ipc_write_byte

	move.w	#$5678,d0		; Step interval
	jsr	ipc_write_word

	move.w	#$abcd,d0		; duration
	jsr	ipc_write_word

	move.b	#2,d0			; pitch step
	jsr	ipc_write_nibble

	move.b	#4,d0			; pitch wrap
	jsr	ipc_write_nibble

	move.b	#8,d0			; pitch randomness
	jsr	ipc_write_nibble

	move.b	#1,d0			; pitch fuzziness
	jsr	ipc_write_nibble

	rts

;***
;
; init_ipc_readkey - read the keyboard
;
; The first returned byte has a top status nibble.
;
;	0000	- No key pressed, no byte to follow.
;	0001	- One key in buffer, byte to follow.
;	0010	- Two(?) keys in buffer, two(?) bytes to follow.
;	1001	- Key currently being held down, byte to follow.	
;
; I don't caurrently know if a value of two does mean two bytes to follow.
;
;***

init_ipc_readkey:
	movem.l	d0-d7/a0-a6,-(SP)

	lea	workspace,a0

	jsr	read_keyboard

	move.w	#5,d0
	jsr	sleep

	jsr	read_keyboard

	cmpi.w	#0,d0
	beq	init_ipc_readkey_test_nopress

	move.b	(a0),d2
	move.b	1(a0),d3

	lea	init_ipc_keyreadyes_txt,a0
	jsr	prt_str

	move.b	d2,d0
	jsr	keymodifierstr
	jsr	prt_str

	move.b	d3,d0
	jsr	keycode2str
	jsr	prt_str

	btst	#3,d2
	beq	init_ipc_readkey_skip2
	lea	init_ipc_keyreadyesheld_txt,a0
	jsr	prt_str

init_ipc_readkey_skip2:

	bra	init_ipc_keyread_end

init_ipc_readkey_test_nopress:

	lea	init_ipc_keyreadno_txt,a0
	jsr	prt_str

init_ipc_keyread_end:

	lea	crlftxt,a0
	jsr	prt_str
	
	movem.l	(SP)+,d0-d7/a0-a6
	rts

;***
;
; init_ipc_serial - Test the serial input
;
;***

init_ipc_serial:
	movem.l	d0-d7/a0-a6,-(SP)

	move.w	#$0FFF,d1

	move.l	#0,d0
	move.l	#10,d5

	move.b	#$2,d0
	jsr	set_input_baud

	jsr	open_ser2

	lea	workspace,a1		; Storage for the data bytes, 1K
	lea	workspace+1024,a2	; Our workspace storage.

	lea	ipc_serial_txt1,a0
	jsr	prt_str

	move.l	#5,d0
	jsr	sleep

	move.l	a1,a0

	jsr	read_ser2	; Loop around until we recieve some bytes

	cmpi.w	#0,d0
	beq	init_serial_nobytes

	lea	ipc_serial_txt2,a0
	jsr	prt_str

	move.l	a2,a0		; Put our workspace pointer into a0
	move.l	d0,d2		; d0 holds the number of bytes in the returned buffer
	subi.w	#1,d2		; Create a loop variable
	move.l	a1,a3		; Working copy of the data pointer

	move.l	a1,a0
	move.b	#0,(0,a0,d0)	; Add a NULL to the end of the workspace
	jsr	prt_str

	lea	crlftxt,a0
	jsr	prt_str	

	bra	init_serial_end

init_serial_nobytes:
	lea	ipc_serial_txt3,a0
	jsr	prt_str

init_serial_end:

	movem.l (SP)+,d0-d7/a0-a6
	rts

ipc_serial_txt1:	dc.b	"Serial port test: Send data to ser2 within 5 seconds",$d,$a,0
ipc_serial_txt2:	dc.b	"Bytes ready: ",0
ipc_serial_txt3:	dc.b	"No bytes ready.",$d,$a,0

	align	2


;***
;
; quick_test_clock - Quickly test the clock is running.
;
;***

quick_test_clock:
	movem.l	d0-d7/a0-a6,-(SP)

	lea	qtclocktxt,a0		; Print the testing clock message
	jsr	prt_str

	jsr	read_clock_value	; Read the clock values into d0 and d1

	move.w	d0,d2			; Copy the low word of the time to d2
	moveq	#5,d3			; Counter for the number of seconds to wait.

	lea	workspace,a0		; Generate the ASCII representation
	jsr	itoa

	jsr	prt_str			; Print it

	jsr	cur_bol			; Send the cursor to the beginning of the line.

quick_test_clock_loop1:
	jsr	read_clock_value	; Read the clock
	cmp.w	d0,d2			; Has it changed?
	beq	quick_test_clock_loop1	; No? Then read it again!
	subi.b	#1,d3			; Decrement our counter for the number times around the loop
	tst.b	d3			; Have we reached the end?
	beq	quick_test_clock_loop1_end		; If yes then finish
	move.w	d0,d2			; Update the "previous value" of the time
	lea	workspace,a0		; Generate the ASCII reprisentation
	jsr	itoa
	jsr	prt_str			; Print it
	jsr	cur_bol			; send the cursor to the beginning of the line.
	bra	quick_test_clock_loop1	; Go around again

	moveq	#$0d,d0		; Print a carriage return linefeed
	jsr	prt_chr

quick_test_clock_loop1_end:

	movem.l (SP)+,d0-d7/a0-a6
	rts

;***
;
; size_memory - Find how much RAM we have
;
;***

size_memory:
	movem.l d0-d7/a0-a6,-(SP)

	lea	sizememtxt,a0	; Print the start message
	jsr	prt_str

	lea	workspace,a0

	move.w	#$5800,(a0)	; Write 'X' and NULL into the workspace.
	jsr	twiddle_reset	; Reset the twiddler

	lea	extramstart,a1

sizememloop1:
	move.l	a1,a2
	add.l	#1,a2		; Put the address above the one we're testing into a2
	move.b	#$aa,d3		; Put the test value 10101010 into d3
	move.b	d3,(a1)		; Write test byte to memory
	move.b	#0,(a2)		; Ground the data lines in case we get a phantom result due to flowing lines.
	moveq	#0,d3
	move.b	(a1),d3		; Copy the data into d3
	cmpi.b	#$aa,d3		; Compare what's in this location with the test byte
	bne	sizememloop1end	; If it's not the same then we've run out of RAM
	move.b	#$55,d3		; Put the test value 01010101 into d3
	move.b	d3,(a1)		; Write test byte to memory
	move.b	#0,(a2)		; Ground the data lines in case we get a phantom result due to flowing lines.
	moveq	#0,d3
	move.b	(a1),d3		; Copy the memory data into d3
	cmpi.b	#$55,d3		; Compare what's in this location with the test byte
	bne	sizememloop1end	; If it's not the same then we've run out of RAM
	add.l	#$1000,a1	; Add 4K to the address
	jsr	prt_twiddle		; Print an X
	bra	sizememloop1	; Go around again.

sizememloop1end:

	cmp.l	#extramstart,a1	; Have we gone anywhere? If not then there's no RAM expansion.
	beq	sizememskip1

sizememloop2:
	move.b	#$0a,-(a1)	; Because we are skipping 4K at a time we may be up to 4K-1
	cmp.b	#$0a,(a1)	; over the end of memory, so step back one byte at a time
	bne	sizememloop2	; until we hit RAM again.

sizememskip1:

	move.l	a1,d0		; Copy the last address with memory into d0

	lea	sysvarbase,a2		; Update the ramtop system variable
	move.l	d0,sysv_ramtop(a2)

	lea	sizememtoptxt,a0	; Print the ramtop address message
	jsr	prt_str

	lea	workspace,a0

	jsr	ltoh			; Print the ramtop value in hex
	jsr	prt_str

	lea	sizememsizetxt,a0	; Print the ram size text
	jsr	prt_str

	lea	workspace,a0

	sub.l	#ramstart,d0		; Calulate the RAM size in bytes
	move.l	d0,sysv_ramsiz(a2)	; Update the system variable
	add.l	#1,d0

	lsr.l	#8,d0			; Divide the address by 1024
	lsr.l	#2,d0

	jsr	itoa			; Print out the decimal value
	jsr	prt_str

	move.b	#$4b,d0			; Print a 'K'
	jsr	prt_chr

	lea	crlftxt,a0		; Print carriage return and linefeed
	jsr	prt_str

	movem.l (SP)+,d0-d7/a0-a6
	rts


;*****************************************************************************
;
;	ext_mem_test	- Expanded memory tests.
;
;*****************************************************************************

ext_mem_test:
	movem.l	d0-d7/a0-a6,-(SP)

	lea	sysvarbase,a1

	move.l	sysv_ramsiz(a1),d5	; Put the RAM size into d5

	cmp.l	#$20000,d5		; Do we have any RAM expansions?
	ble	ext_mem_tests_skip	; Skip if we don't

	lea	extmemtstbantxt,a0
	jsr	prt_str

	jsr	ext_mem_test_march	; Test using own march test.
	jsr	ext_mem_test_own	; Test using own address.

ext_mem_tests_skip:

	movem.l	(SP)+,d0-d7/a0-a6
	rts

;*****************************************************************************
;
;	ext_mem_test_own	- Expanded memory tests, own address.
;
;*****************************************************************************

ext_mem_test_own:
	movem.l	d0-d7/a0-a6,-(SP)

	lea	extmemtstowntxt1,a0
	jsr	prt_str

	jsr	twiddle_reset

	lea	sysvarbase,a1

	move.l	sysv_ramsiz(a1),d5	; Put the RAM size into d5

	sub.l	#$20001,d5		; Remove the base 128K

	move.l	d5,d4			; Copy the RAM size into d4 and mask off the top word.
	and.l	#$0000ffff,d4

	lsr.l	#8,d5			; Shift d5 down by a word so it's the top half of the size.
	lsr.l	#8,d5
	and.l	#$0000ffff,d5		; And mask any high bits.

	lea	extramstart,a0
	
	move.w	d4,d1			; Copy the values into working registers
	move.w	d5,d2

	move.l	#0,d3

ext_mem_test_own_wr:
	move.b	d3,(a0)+
	add.l	#1,d3
	dbf	d1,ext_mem_test_own_wr
	jsr	prt_twiddle
	dbf	d2,ext_mem_test_own_wr

	lea	donetxt,a0
	jsr	prt_str

	lea	extmemtstowntxt2,a0
	jsr	prt_str

	jsr	twiddle_reset

	lea	extramstart,a0

	move.w	d4,d1			; Copy the values into working registers
	move.w	d5,d2

	move.l	#0,d3

ext_mem_test_own_re:
	cmp.b	(a0)+,d3
	beq	ext_mem_test_own_re_ok

	jsr	ext_mem_test_error

ext_mem_test_own_re_ok:
	add.l	#1,d3

	dbf	d1,ext_mem_test_own_re
	jsr	prt_twiddle
	dbf	d2,ext_mem_test_own_re

	lea	donetxt,a0
	jsr	prt_str

	movem.l	(SP)+,d0-d7/a0-a6
	rts

;*****************************************************************************
;
;	ext_mem_test_march	- Expanded memory tests, march.
;
;*****************************************************************************

ext_mem_test_march:
	movem.l	d0-d7/a0-a6,-(SP)

	lea	sysvarbase,a1

	move.l	sysv_ramsiz(a1),d5		; Put the RAM size into d5

	sub.l	#$20001,d5		; Remove the base 128K

	move.l	d5,d4			; Copy the RAM size into d4 and mask off the top word.
	and.l	#$0000ffff,d4

	lsr.l	#8,d5			; Shift d5 down by a word so it's the top half of the size.
	lsr.l	#8,d5

	lea	extmemtstmarchtxt1,a0
	jsr	prt_str

	jsr	twiddle_reset

	lea	extramstart,a0

	move.w	d4,d1			; Copy the values into working registers
	move.w	d5,d2

	move.l	#$ff,d3

ext_mem_test_march_wr:
	move.b	d3,(a0)+
	dbf	d1,ext_mem_test_march_wr
	jsr	prt_twiddle
	dbf	d2,ext_mem_test_march_wr

	lea	donetxt,a0
	jsr	prt_str


	lea	extmemtstmarchtxt2,a0
	jsr	prt_str

	jsr	twiddle_reset

	lea	extramstart,a0

	move.w	d4,d1			; Copy the values into working registers
	move.w	d5,d2

	move.l	#$ff,d3

ext_mem_test_march_flip1:
	cmp.b	(a0),d3
	beq	ext_mem_test_march_flip1_ok

	jsr	ext_mem_test_error

ext_mem_test_march_flip1_ok:
	eori.b	#$ff,(a0)+

	dbf	d1,ext_mem_test_march_flip1
	jsr	prt_twiddle
	dbf	d2,ext_mem_test_march_flip1

	lea	donetxt,a0
	jsr	prt_str


	lea	extmemtstmarchtxt3,a0
	jsr	prt_str

	jsr	twiddle_reset

	lea	extramstart,a0

	move.w	d4,d1			; Copy the values into working registers
	move.w	d5,d2

	move.l	#$00,d3

ext_mem_test_march_flip2:
	cmp.b	(a0),d3
	beq	ext_mem_test_march_flip2_ok

	jsr	ext_mem_test_error

ext_mem_test_march_flip2_ok:
	eori.b	#$ff,(a0)+

	dbf	d1,ext_mem_test_march_flip2
	jsr	prt_twiddle
	dbf	d2,ext_mem_test_march_flip2

	lea	donetxt,a0
	jsr	prt_str

	lea	extmemtstmarchtxt4,a0
	jsr	prt_str

	jsr	twiddle_reset

	lea	extramstart,a0

	move.w	d4,d1			; Copy the values into working registers
	move.w	d5,d2

	move.l	#$ff,d3

ext_mem_test_march_flip3:
	cmp.b	(a0),d3
	beq	ext_mem_test_march_flip3_ok

	jsr	ext_mem_test_error

ext_mem_test_march_flip3_ok:
	eori.b	#$ff,(a0)+

	dbf	d1,ext_mem_test_march_flip3
	jsr	prt_twiddle
	dbf	d2,ext_mem_test_march_flip3

	lea	donetxt,a0
	jsr	prt_str


	movem.l	(SP)+,d0-d7/a0-a6
	rts

;*****************************************************************************
;
;	ext_mem_test_error	- Print memory error.
;
;	d3 holds the test data and a0 is the address.
;
;*****************************************************************************

ext_mem_test_error:
	movem.l	d0-d7/a0-a6,-(SP)

	move.l	a0,a1

	lea	extmemtsterr1,a0
	jsr	prt_str

	lea	workspace,a0

	move.l	a1,d0

	jsr	ltoh
	jsr	prt_str

	lea	extmemtsterr2,a0
	jsr	prt_str

	lea	workspace,a0
	move.b	d3,d0

	jsr	btoh
	jsr	prt_str

	lea	extmemtsterr3,a0
	jsr	prt_str

	lea	workspace,a0
	move.b	(a1),d0

	jsr	btoh
	jsr	prt_str

	lea	crlftxt,a0
	jsr	prt_str

	movem.l	(SP)+,d0-d7/a0-a6
	rts

;*****************************************************************************
;
; End of diagnostic routines.
;
;*****************************************************************************
