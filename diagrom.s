; Sinclair QL BareMetal Diagnostic ROM
;
; Version 0.00
;
; Copyright 2025 Stephen Usher (and any other contributers listed below).
;
; Contributers:
;
; License: Creative Commons, Attribution.
;
; (Basically, you can copy and modify the code as you wish as long as you
; keep the list of contributers and don't try to pass it off as all your
; own work. In other words, "Don't be a Dick!")
;
; This ROM replaces the main system ROM using the same EPROM adapter
; that you would use with a Minerva ROM.
;
; The design is such that to boot and do something useful only needs
; a minimully working system. It can run the initial memory test and
; report back even if no RAM is in the system.
;
; All it need to work is a working CPU, a ZX8301 which can generate
; the clock for the CPU, address decoding and DTACK generation and a
; ZX8302 which still has enough functionality to write data out of
; serial port 1.
;
; In other words, even if the video side of the ZX8301 is dead you
; can still test the rest of the system using a terminal.
;
; The serial conection on Ser1 runs at 4800 baud. Almost the very
; first thing the code does is write out a banner message, so look
; for that.
;
; The ROM will then run a couple of thorough memory tests on the
; lower 128K of RAM. Firstly an 'own address' test and then a full
; March test. The combination of these should catch any bad bits
; or addressing errors.
;
; Once the ROM has determined that RAM is usable it can start
; running more fully, first checking the IPC for functionality
; and then bringing up a menu of further tests. This can be accessed
; either via the serial port or on the main screen if the display is
; working.
;
; This ROM is just another tool in the diagnostic arsnal. If it
; doesn't work then that probably says something about the state
; or the ULAs or the CPU and an oscilloscope would be your best
; bet at that point.
;

	org	$00000000

startsp		equ	$0003FFF0

zx83base	equ	$00018000

zx83off_r_clock		equ	$00
zx83off_r_cstatus	equ	$20
zx83off_r_istatus	equ	$21
zx83off_r_mdvtrk1	equ	$22
zx83off_r_mdvtrk2	equ	$23

zx83off_w_clock		equ	$0
zx83off_w_transreg	equ	$2
zx83off_w_ipcwreg	equ	$3
zx83off_w_mdvreg		equ	$20
zx83off_w_imaskreg	equ	$21
zx83off_w_transdata	equ	$22

zx83off_vidreg	equ	$63

zx83_r_clock		equ	zx83base
zx83_r_cstatus		equ	zx83base+zx83off_r_cstatus
zx83_r_istatus		equ	zx83base+zx83off_r_istatus
zx83_r_mdvtrk1		equ	zx83base+zx83off_r_mdvtrk1
zx83_r_mdvtrk2		equ	zx83base+zx83off_r_mdvtrk2

zx83_w_clock		equ	zx83base+zx83off_w_clock
zx83_w_transreg		equ	zx83base+zx83off_w_transreg
zx83_w_ipcwreg		equ	zx83base+zx83off_w_ipcwreg
zx83_w_mdvreg		equ	zx83base+zx83off_w_mdvreg
zx83_w_imaskreg		equ	zx83base+zx83off_w_imaskreg
zx83_w_transdata	equ	zx83base+zx83off_w_transdata

ramstart	equ	$00020000
basramsiz	equ	$0001FFFF
extramstart	equ	$00040000

sysvarbase	equ	$00028000

sysv_cur_x	equ	$0
sysv_cur_y	equ	$1
sysv_cur_twd	equ	$2
sysv_ramtop	equ	$4
sysv_ramsiz	equ	$8
sysv_ipcintreg	equ	$12
sysv_intermask	equ	$14
sysv_idisable	equ	$18

;
; Keyboard buffer: 32 words
;
kbd_buf_offset	equ	$000028200
kbd_buffer	equ	$000028202
;
; Ser1 receive buffer: 1K
;
ser1_buf_count	equ	$000028242	; One word.
ser1_buffer	equ	$000028244	; 1024 bytes
;
; Ser2 receive buffer: 1K
;
ser2_buf_count	equ	$000028644	; One word.
ser2_buffer	equ	$000028646	; 1024 bytes

workspace	equ	$00002a000	; Generic workspace.

	dc.l	startsp
	dc.l	start
	dc.l	buserr
	dc.l	addresserr
	dc.l	illegalins
	dc.l	divzero
	dc.l	chkerr
	dc.l	trapv
	dc.l	privviol
	dc.l	trace
	dc.l	linea
	dc.l	linef
	dc.l	$00000000
	dc.l	$00000000
	dc.l	$00000000
	dc.l	unintvec
	dc.l	$00000000
	dc.l	$00000000
	dc.l	$00000000
	dc.l	$00000000
	dc.l	$00000000
	dc.l	$00000000
	dc.l	$00000000
	dc.l	$00000000
	dc.l	spurint
	dc.l	intvec1
	dc.l	intvec2
	dc.l	intvec3
	dc.l	intvec4
	dc.l	intvec5
	dc.l	intvec6
	dc.l	intvec7
	dc.l	trapvec
	
	org	$00000400

start:
	move.l	#0,d0
	move.l	d0,d1
	move.l	d0,d2
	move.l	d0,d3
	move.l	d0,d4
	move.l	d0,d5
	move.l	d0,d6
	move.l	d0,d7

	move.l	d0,a0
	move.l	d0,a1
	move.l	d0,a2
	move.l	d0,a3
	move.l	d0,a4
	move.l	d0,a5
	move.l	d0,a6
	move.l	d0,a7

	lea	zx83base,a0
	move.b	#0,zx83off_vidreg(a0)	; Change to MODE 4
;	move.b	#$0F,zx83off_w_imaskreg(a0)	; Clear and disable interrupts
	move.b	#%00001010,zx83off_w_transreg(a0)	; Set the ouput serial port settings (ser2, 4800 baud)
	move.b	#$01,zx83off_w_ipcwreg(a0)	; Reset the IPC
	move.b	#$00,zx83off_w_clock(a0)	; Reset the clock
	lea	bannertext,a5	; Put the address of the text banner into a1
	lea	banend1,a6	; Put the pointer to the return address into a6
	bra	noram_print_string
banend1:
	lea	banend2,a6	; Put the pointer to the return address into a6
	moveq	#0,d2
	bra	noram_print_string_scr
banend2:
	lea	memtststarttxt,a5
	lea	memtststartend1,a6
	bra	noram_print_string
memtststartend1:
	moveq	#1,d2
	lea	memtststartend2,a6
	bra	noram_print_string_scr
memtststartend2:
	bra	memtest
endmemtst:

	lea	startsp,sp
	lea	zx83base,a0
;	move.b	#%01011111,zx83off_w_imaskreg(a0)	; Clear and disable interrupts
	lea	sysvarbase,a0
	move.b	#%01000000,sysv_intermask(a0)
	move.b	#0,sysv_idisable(a0)
;	andi.w	#%1111100111111111,SR

	bra	main

	include 'noramprt.s'
	include	'norammemtst.s'

buserr:
addresserr:
illegalins:
divzero:
chkerr:
trapv:
privviol:
trace:
linea:
linef:
unintvec:
spurint:
	rte
intvec1:
intvec2:
intvec3:
intvec4:
intvec5:
intvec6:
intvec7:
; Interrupt handler.
	eori.w	#$10,SR
	movem.l	d0-d7/a0-a6,-(SP)

	lea	sysvarbase,a1
	lea	zx83base,a2

	move.b	sysv_intermask(a1),d0
	ori.b	#%00011111,d0
;	move.b	d0,zx83off_w_imaskreg(a2)

	cmpi.b	#1,sysv_idisable(a1)
	beq	int_handler_end

	move.b	#1,sysv_idisable(a1)		; Stop re-entry of the interrupt routine

	move.l	#$0fff,d1

	move.b	#$1,d0
	jsr	ipc_write_nibble

	jsr	ipc_read_byte

	btst	#0,d0
	beq	int_handler_notkbd
	jsr	ipc_read_keyboard
int_handler_notkbd:

	btst	#4,d0
	beq	int_handler_notser1
	jsr	ipc_read_ser1
int_handler_notser1:

	btst	#5,d0
	beq	int_handler_notser2
	jsr	ipc_read_ser2
int_handler_notser2:

	move.b	#0,sysv_idisable(a1)

int_handler_end:

	movem.l	(SP)+,d0-d7/a0-a6
	eori.w	#$10,SR
	rte

trapvec:
	rte

	align 2

	include 'library.s'
	include 'tests.s'

;
; The main program starts here as we have a stack and can program normally.
;

main:
	jsr	cls

	lea	bannertext,a0		; Write out the banner to the screen
	jsr	prt_str

	lea	memtstcomptext,a0	; Write out the memory test completed
	jsr	prt_str

	jsr	size_memory		; Find out how much RAM we have.

	jsr	ext_mem_test		; Test the expanded RAM.

	jsr	quick_test_clock

	jsr	init_ipc_test		; Run the initial IPC tests.

	bra	end

end:	bra end

	include 'ipc.s'

	align 2

	include 'font.inc'

	align 2

	include 'strings.inc'

	align 2

	include 'keycodes.inc'
