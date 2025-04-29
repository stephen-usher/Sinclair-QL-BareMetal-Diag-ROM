
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
;	jsr	ipc_read_nibble
	rts

;***
;
; ipc_selfttest - Runt eh IPC selftest and report result
;
;***

ipc_selftest:
	move.w	#$0FFF,d1
	move.b	#$0F,d0
	jsr	ipc_write_nibble

	move.b	#$01,d0
	jsr	ipc_write_byte

	move.b	#$aa,d0
	jsr	ipc_write_byte

	jsr	ipc_read_byte

	andi.w	#$00FF,d0

	lea	workspace,a0
	jsr	itoa

	jsr	prt_str

	rts

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

;*****************************************************************************
;
; End of diagnostic routines.
;
;*****************************************************************************
