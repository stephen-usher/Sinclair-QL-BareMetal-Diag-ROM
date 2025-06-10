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

workspace	equ	$000028200

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
	move.b	#$0F,zx83off_w_imaskreg(a0)	; Clear and disable interrupts
	move.b	#%00001010,zx83off_w_transreg(a0)	; Set the ouput serial port settings (ser2, 4800 baud)
	move.b	#$01,zx83off_w_ipcwreg(a0)	; Reset the IPC
	move.b	#$00,zx83off_w_clock(a0)	; Reset the clock
	lea	bannertext,a5	; Put the address of the text banner into a1
	lea	banend,a6	; Put the pointer to the return address into a6
	moveq	#0,d2
	bra	noram_print_string
banend:
	lea	memtststarttxt,a5
	lea	memtststartend,a6
	moveq	#1,d2
	bra	noram_print_string
memtststartend:
	bra	memtest
endmemtst:

	lea	startsp,sp

	bra	main

;
; noram_print_string
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

noram_print_string:
	lea	ramstart,a4
	mulu	#1280,d2	; d2 holds address offset of the character row
	add.l	d2,a4		; Add this to the start address

	move.l	#0,d0		; Set up the starting position to print onto screen
	move.l	d0,d5


nr_p_string_loop1:
	move.l	#0,d6		; clear out d6
	move.b	(a5)+,d6	; Copy character into d6 and increment a5

	tst.b	d6		; Is it a NULL byte?
	beq	nr_p_string_end		; If so finish

	move.b	d6,zx83off_w_transdata(a0)	; Transmit the character.

nr_p_string_wait:
;	btst.b	#2,zx83off_r_cstatus(a0)	; See if the transmit buffer is full
	move.b	zx83off_r_cstatus(a0),d4
	andi.b	#$02,d4
	cmpi.b	#$02,d4
	beq	nr_p_string_wait	; If it is go around again until it's empty

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

	bra	nr_p_string_loop1	; Print the next character

nr_p_string_end:
	mulu	#2,d0
	move.l	#8,d3
nrpdendlp:
	move.b	#$FF,(0,a4,d0)
	add.l	#128,d0
	dbf	d3,nrpdendlp

	jmp	(a6)

memtstsizel	equ	$7fff
memtstsizeh	equ	$0001
	
memtest:
	move.l	#1,d2		; Memory test 1

	move.l	#memtstsizel,d0	; Set up the RAM test loop counters
	move.l	#memtstsizeh,d1

	lea	ramstart,a1	; Load the base of RAM into a1

	move.l	#0,d3
ownaddrwr:
	move.b	d3,(a1)+	; Write the lower byte to RAM
	add.l	#1,d3
	dbf	d0,ownaddrwr	; Go around until the end of memory
	dbf	d1,ownaddrwr

	move.l	#memtstsizel,d0	; Set up the RAM test loop counters
	move.l	#memtstsizeh,d1

	lea	ramstart,a1	; Load the base of RAM into a1

	move.l	#0,d3
ownaddrrd:
	cmp.b	(a1),d3	; Test if the memory matches the lowest byte of the address.
	bne	memerror	; Error if not the same.
	add.l	#1,a1
	add.l	#1,d3
	dbf	d0,ownaddrrd	; Go around until end of memory
	dbf	d1,ownaddrrd

	move.l	#memtstsizel,d0	; Set up the RAM test loop counters
	move.l	#memtstsizeh,d1

	lea	ramstart,a1	; Load the base of RAM into a1

marchtestp1:
	move.b	#$FF,(a1)+	; Write a byte of 1s to RAM.
	dbf	d0,marchtestp1	; Go around until all RAM has been filled.
	dbf	d1,marchtestp1	; Go around until all RAM has been filled.

	move.l	#2,d2		; Memory test 2

	move.l	#memtstsizel,d0	; Set up the RAM test loop counters
	move.l	#memtstsizeh,d1

	lea	ramstart,a1	; Load the base of RAM into a1

	move.l	#$000000FF,d3
marchtestp2:
	cmpi.b	#$FF,(a1)	; Test if what we read is what we wrote.
	bne	memerror	; If it's not error and halt.
	eori.b	#-1,(a1)+	; XOR the memory value with all ones.
	dbf	d0,marchtestp2	; Go around again until all memory has been done.
	dbf	d1,marchtestp2	; Go around again until all memory has been done.

	move.l	#3,d2		; Memory test 3

	move.l	#memtstsizel,d0	; Set up the RAM test loop counters
	move.l	#memtstsizeh,d1

	lea	ramstart,a1	; Load the base of RAM into a1

	move.l	#$00000000,d3
marchtestp3:
	cmpi.b	#$00,(a1)	; Test if what we read is the opposite of what we wrote.
	bne	memerror	; If it's not error and halt.
	eori.b	#-1,(a1)+	; XOR the memory value with all ones.
	dbf	d0,marchtestp3	; Go around again until all memory has been done.
	dbf	d1,marchtestp3	; Go around again until all memory has been done.

	move.l	#4,d2		; Memory test 4

	move.l	#memtstsizel,d0	; Set up the RAM test loop counters
	move.l	#memtstsizeh,d1

	lea	ramstart,a1	; Load the base of RAM into a1

	move.l	#$000000FF,d3
marchtestp4:
	cmpi.b	#$FF,(a1)	; Test if what we read is what we wrote originally.
	bne	memerror	; If it's not error and halt.
	eori.b	#-1,(a1)+	; XOR the memory value with all ones.
	dbra	d0,marchtestp4	; Go around again until all memory has been done.
	dbra	d1,marchtestp4	; Go around again until all memory has been done.

memtstend:

	bra	endmemtst	; The memory test has completed successfully.

memerror:
	lea	memerrtext,a5	; Put the address of the memory error text into a5
	lea	memerrorprtret1,a6
	moveq	#3,d2
	bra	noram_print_string
memerrorprtret1:
	move.l	a1,d4
	move.b	(a1),d0
	eor.b	d3,d0
	moveq	#7,d1
memerrorbiterrorloop:
	move.b	d1,d5
	addi.b	#$30,d5
	btst.l	d1,d0
	bne	memerrorbiterrorskip
	move.b	d5,$22(a0)
memerrorbiterrorskip:
	dbf	d1,memerrorbiterrorloop

	lea	ramstart,a1
	rol.b	#2,d2
	adda.l	d2,a1
eloopy:
	move.l	#$0000FFFF,d0
eloopy2:
	move.w	d0,(a1)
	move.w	d0,128(a1)
	move.w	d0,256(a1)
	move.w	d0,384(a1)
	move.w	d0,512(a1)
	move.w	d0,640(a1)
	move.w	d0,768(a1)
	move.w	d0,896(a1)
	dbeq	d0,eloopy2
	bra	eloopy

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
intvec1:
intvec2:
intvec3:
intvec4:
intvec5:
intvec6:
intvec7:
trapvec:
	bra end

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


	align 2

	include 'font.inc'

	align 2

	include 'strings.inc'

	align 2

	include 'keycodes.inc'
