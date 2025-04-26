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
ramstart	equ	$00020000
basramsiz	equ	$0001FFFF

sysvarbase	equ	$00029000

sysv_cur_x	equ	100
sysv_cur_y	equ	101

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
	lea	zx83base,a0
	move.b	#0,$63(a0)	; Change to MODE 4
	move.b	#$0F,$21(a0)	; Clear and disable interrupts
	move.b	#$05,$2(a0)	; Set the ouput serial port settings (ser1, 4800 baud)
	lea	bannertext,a1	; Put the address of the text banner into a1
printban:
	cmpi.b	#0,(a1)		; Is it a NULL byte?
	beq	banend		; If so finish
	move.b	(a1)+,d0	; Copy character into d0 and increment a1
	move.b	d0,$22(a0)	; Transmit the character.
banprtwait:
	btst.b	#2,$20(a0)	; See if the transmit buffer is full
	bne	banprtwait	; If it is go around again until it's empty
	bra	printban	; Print the next character
banend:
	bra	memtest
endmemtst:

	lea	startsp,sp

	move.b	#0,d0		; Set the cursor to the top left of the display
	move.b	#0,d1

	jsr	scr_cur_pos
	jsr	ser_cur_pos

	lea	bannertext,a0	; Write out the banner to the screen
	jsr	scr_prt_str	; It's already been sent over the serial connection

	lea	memtstcomptext,a0	; Write out the memory test completed
	jsr	scr_prt_str		; message to both serial and screen
	jsr	ser_prt_str

;	lea	ramstart,a1
;loopy:
;	move.l	#255,d0
;loopy2:
;	move.b	d0,(a1)
;	dbf	d0,loopy2

	bra	end

;
; noram_ser_print_string
;
; Print a NULL terminated string to the serial port without RAM
;
; a0 - IO base address
; a5 - address of the start of the string
; a6 - return address
;
; d6 and a5 corrupted
;

noram_ser_print_string:
noram_ser_print_string_loop1:
	cmpi.b	#0,(a5)		; Is it a NULL byte?
	beq	noram_ser_print_string_end		; If so finish
	move.b	(a5)+,d6	; Copy character into d0 and increment a1
	move.b	d6,$22(a0)	; Transmit the character.
noram_ser_print_string_wait:
	btst.b	#2,$20(a0)	; See if the transmit buffer is full
	bne	noram_ser_print_string_wait	; If it is go around again until it's empty
	bra	noram_ser_print_string_loop1	; Print the next character
noram_ser_print_string_end:
	jmp	(a6)
	
memtest:
	move.l	#1,d2		; Memory test 1

	move.l	#basramsiz,d0	; Put the base RAM size into d0
	move.l	d0,d1
	lsr.l	#8,d1
	lsr.l	#8,d1		; Put the high word of the count into d1
	lea	ramstart,a1	; Load the base of RAM into a1

	move.l	#0,d3
ownaddrwr:
	move.b	d3,(a1)+	; Write the lower byte to RAM
	add.l	#1,d3
	dbf	d0,ownaddrwr	; Go around until the end of memory
	dbf	d1,ownaddrwr

	move.l	#basramsiz,d0	; Put the base RAM size into d0
	move.l	d0,d1
	lsr.l	#8,d1
	lsr.l	#8,d1		; Put the high word of the count into d1
	lea	ramstart,a1	; Load the base of RAM into a1

	move.l	#0,d3
ownaddrrd:
	cmp.b	(a1),d3	; Test if the memory matches the lowest byte of the address.
	bne	memerror	; Error if not the same.
	add.l	#1,a1
	add.l	#1,d3
	dbf	d0,ownaddrrd	; Go around until end of memory
	dbf	d1,ownaddrrd

	move.l	#basramsiz,d0	; Put the base RAM size into d0
	move.l	d0,d1
	lsr.l	#8,d1
	lsr.l	#8,d1		; Put the high word of the count into d1
	lea	ramstart,a1	; Load the base of RAM into a1

marchtestp1:
	move.b	#$FF,(a1)+	; Write a byte of 1s to RAM.
	dbf	d0,marchtestp1	; Go around until all RAM has been filled.
	dbf	d1,marchtestp1	; Go around until all RAM has been filled.

	move.l	#2,d2		; Memory test 2

	move.l	#basramsiz,d0	; Put the base RAM size into d0
	move.l	d0,d1
	lsr.l	#8,d1
	lsr.l	#8,d1		; Put the high word of the count into d1
	lea	ramstart,a1	; Load the base of RAM into a1

	move.l	#$000000FF,d3
marchtestp2:
	cmpi.b	#$FF,(a1)	; Test if what we read is what we wrote.
	bne	memerror	; If it's not error and halt.
	eori.b	#-1,(a1)+	; XOR the memory value with all ones.
	dbf	d0,marchtestp2	; Go around again until all memory has been done.
	dbf	d1,marchtestp2	; Go around again until all memory has been done.

	move.l	#3,d2		; Memory test 3

	move.l	#basramsiz,d0	; Put the base RAM size into d0
	move.l	d0,d1
	lsr.l	#8,d1
	lsr.l	#8,d1		; Put the high word of the count into d1
	lea	ramstart,a1	; Load the base of RAM into a1

	move.l	#$00000000,d3
marchtestp3:
	cmpi.b	#$00,(a1)	; Test if what we read is the opposite of what we wrote.
	bne	memerror	; If it's not error and halt.
	eori.b	#-1,(a1)+	; XOR the memory value with all ones.
	dbf	d0,marchtestp3	; Go around again until all memory has been done.
	dbf	d1,marchtestp3	; Go around again until all memory has been done.

	move.l	#4,d2		; Memory test 4

	move.l	#basramsiz,d0	; Put the base RAM size into d0
	move.l	d0,d1
	lsr.l	#8,d1
	lsr.l	#8,d1		; Put the high word of the count into d1
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

memerrorprtret1redir:	dc.l	memerrorprtret1

memerror:
	lea	memerrtext,a5	; Put the address of the memory error text into a5
	lea	memerrorprtret1redir,a6
	bra	noram_ser_print_string
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

end:	bra end

;
; Print a string to the serial port.
;
; a0 - pointer to the NULL terminated string.
;

ser_prt_str:
	movem.l	d0/a1,-(SP)
	lea	zx83base,a1		; Load the I/O base address into A1
ser_prt_str_loop1:
	cmpi.b	#0,(a0)			; Is it a NULL byte?
	beq	ser_prt_str_end		; If so finish
	move.b	(a0)+,d0		; Copy character into d0 and increment a1
	move.b	d0,$22(a1)		; Transmit the character.
ser_prt_str_wait:
	btst.b	#2,$20(a1)		; See if the transmit buffer is full
	bne	ser_prt_str_wait	; If it is go around again until it's empty
	bra	ser_prt_str_loop1	; Print the next character
ser_prt_str_end:
	movem.l (SP)+,d0/a1
	rts

;
; Send the ANSI codes for cursor position to the serial port.
;
; Cursor position is in the system variables.
;

ser_cur_pos:
	rts

;
; Print a string to the screen from the current cursor position.
;
; A0 - pointer to the NULL terminated string.
;

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

;
; Print a character on the screen at the current cursor position and move the cursor on one position
;
; d0 - ASCII character to be printed
;

scr_prt_chr:
	movem.l d0-d6/a0-a2,-(SP)

	lea	ramstart,a0	; Set the screen start pointer

	lea	sysvarbase,a1	; Obtain the current cursor position
	add.l	sysv_cur_x,a1	; and save a copy in d5 and d6
	move.l	a1,a2		; d1 and d2 are working registers

	move.l	#0,d1
	move.l	#0,d2
	move.l	#0,d5
	move.l	#0,d6

	move.b	(a1)+,d1
	move.b	(a1),d2

	move.b	d1,d5
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
	move.l	d1,d3			; Check that we've not gone off the right of the screen.
	sub.l	#81,d3
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
; Deal with control characters.
; We only worry about carrage return and line feed.
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

;
; Set the screen cursor position
;
; d0 - X position in character cells.
; d1 - Y position in character cells.
;

scr_cur_pos:
	movem.l	a0,-(SP)		; Save the registers we're going to modify
	lea	sysvarbase,a0		; Load the base system variable address into a0
	add.l	sysv_cur_x,a0		; Add the offset to the cursor location
	move.b	d0,(a0)+		; Write the values to the system variables.
	move.b	d1,(a0)
	movem.l	(SP)+,a0		; Restore the registers.

	rts

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

bannertext:
	dc.b	"QL Diagnostics version 0.00",$0d,$0a,$0
memerrtext:
	dc.b	"Memory test error, halting.",$0d,$0a,$0
memtstcomptext:
	dc.b	"Initial memory test complete.",$0d,$0a,$0

align

;
; This font has ben extracted from the Eurofont file
; by Dilwyn Jones, downloaded from his archive.
;

font_space:	dc.b	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
font_char0:	dc.b	$54,$28,$54,$28,$54,$28,$54,$28,$54,$0
font_char1:	dc.b	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
font_char2:	dc.b	$10,$10,$10,$10,$10,$0,$10,$0,$0,$0
font_char3:	dc.b	$28,$28,$0,$0,$0,$0,$0,$0,$0,$0
font_char4:	dc.b	$28,$28,$7c,$28,$7c,$28,$28,$0,$0,$0
font_char5:	dc.b	$38,$50,$50,$38,$14,$14,$38,$0,$0,$0
font_char6:	dc.b	$64,$64,$8,$10,$20,$4c,$4c,$0,$0,$0
font_char7:	dc.b	$20,$50,$50,$20,$54,$48,$34,$0,$0,$0
font_char8:	dc.b	$10,$10,$0,$0,$0,$0,$0,$0,$0,$0
font_char9:	dc.b	$4,$8,$10,$10,$10,$8,$4,$0,$0,$0
font_char10:	dc.b	$40,$20,$10,$10,$10,$20,$40,$0,$0,$0
font_char11:	dc.b	$10,$54,$38,$10,$38,$54,$10,$0,$0,$0
font_char12:	dc.b	$0,$10,$10,$7c,$10,$10,$0,$0,$0,$0
font_char13:	dc.b	$0,$0,$0,$0,$0,$18,$18,$8,$10,$0
font_char14:	dc.b	$0,$0,$0,$7c,$0,$0,$0,$0,$0,$0
font_char15:	dc.b	$0,$0,$0,$0,$0,$18,$18,$0,$0,$0
font_char16:	dc.b	$4,$4,$8,$10,$20,$40,$40,$0,$0,$0
font_char17:	dc.b	$38,$44,$4c,$54,$64,$44,$38,$0,$0,$0
font_char18:	dc.b	$10,$30,$10,$10,$10,$10,$38,$0,$0,$0
font_char19:	dc.b	$38,$44,$4,$8,$10,$20,$7c,$0,$0,$0
font_char20:	dc.b	$38,$44,$4,$18,$4,$44,$38,$0,$0,$0
font_char21:	dc.b	$8,$18,$28,$48,$7c,$8,$8,$0,$0,$0
font_char22:	dc.b	$7c,$40,$78,$4,$4,$44,$38,$0,$0,$0
font_char23:	dc.b	$18,$20,$40,$78,$44,$44,$38,$0,$0,$0
font_char24:	dc.b	$7c,$4,$8,$10,$20,$40,$40,$0,$0,$0
font_char25:	dc.b	$38,$44,$44,$38,$44,$44,$38,$0,$0,$0
font_char26:	dc.b	$38,$44,$44,$3c,$4,$8,$30,$0,$0,$0
font_char27:	dc.b	$0,$0,$18,$18,$0,$18,$18,$0,$0,$0
font_char28:	dc.b	$0,$0,$18,$18,$0,$18,$18,$8,$10,$0
font_char29:	dc.b	$4,$8,$10,$20,$10,$8,$4,$0,$0,$0
font_char30:	dc.b	$0,$0,$7c,$0,$7c,$0,$0,$0,$0,$0
font_char31:	dc.b	$40,$20,$10,$8,$10,$20,$40,$0,$0,$0
font_char32:	dc.b	$38,$44,$4,$8,$10,$0,$10,$0,$0,$0
font_char33:	dc.b	$38,$44,$5c,$54,$5c,$40,$38,$0,$0,$0
font_char34:	dc.b	$38,$44,$44,$7c,$44,$44,$44,$0,$0,$0
font_char35:	dc.b	$78,$44,$44,$78,$44,$44,$78,$0,$0,$0
font_char36:	dc.b	$38,$44,$40,$40,$40,$44,$38,$0,$0,$0
font_char37:	dc.b	$78,$44,$44,$44,$44,$44,$78,$0,$0,$0
font_char38:	dc.b	$7c,$40,$40,$78,$40,$40,$7c,$0,$0,$0
font_char39:	dc.b	$7c,$40,$40,$78,$40,$40,$40,$0,$0,$0
font_char40:	dc.b	$38,$44,$40,$40,$4c,$44,$38,$0,$0,$0
font_char41:	dc.b	$44,$44,$44,$7c,$44,$44,$44,$0,$0,$0
font_char42:	dc.b	$38,$10,$10,$10,$10,$10,$38,$0,$0,$0
font_char43:	dc.b	$4,$4,$4,$4,$4,$44,$38,$0,$0,$0
font_char44:	dc.b	$44,$48,$50,$60,$50,$48,$44,$0,$0,$0
font_char45:	dc.b	$40,$40,$40,$40,$40,$40,$7c,$0,$0,$0
font_char46:	dc.b	$44,$6c,$54,$44,$44,$44,$44,$0,$0,$0
font_char47:	dc.b	$44,$44,$64,$54,$4c,$44,$44,$0,$0,$0
font_char48:	dc.b	$38,$44,$44,$44,$44,$44,$38,$0,$0,$0
font_char49:	dc.b	$78,$44,$44,$78,$40,$40,$40,$0,$0,$0
font_char50:	dc.b	$38,$44,$44,$44,$54,$48,$34,$0,$0,$0
font_char51:	dc.b	$78,$44,$44,$78,$50,$48,$44,$0,$0,$0
font_char52:	dc.b	$38,$44,$40,$38,$4,$44,$38,$0,$0,$0
font_char53:	dc.b	$7c,$10,$10,$10,$10,$10,$10,$0,$0,$0
font_char54:	dc.b	$44,$44,$44,$44,$44,$44,$38,$0,$0,$0
font_char55:	dc.b	$44,$44,$44,$44,$44,$28,$10,$0,$0,$0
font_char56:	dc.b	$44,$44,$44,$44,$54,$54,$28,$0,$0,$0
font_char57:	dc.b	$44,$44,$28,$10,$28,$44,$44,$0,$0,$0
font_char58:	dc.b	$44,$44,$28,$10,$10,$10,$10,$0,$0,$0
font_char59:	dc.b	$7c,$4,$8,$10,$20,$40,$7c,$0,$0,$0
font_char60:	dc.b	$1c,$10,$10,$10,$10,$10,$1c,$0,$0,$0
font_char61:	dc.b	$40,$40,$20,$10,$8,$4,$4,$0,$0,$0
font_char62:	dc.b	$70,$10,$10,$10,$10,$10,$70,$0,$0,$0
font_char63:	dc.b	$10,$28,$44,$0,$0,$0,$0,$0,$0,$0
font_char64:	dc.b	$0,$0,$0,$0,$0,$0,$0,$7c,$0,$0
font_char65:	dc.b	$18,$24,$20,$70,$20,$20,$7c,$0,$0,$0
font_char66:	dc.b	$0,$0,$34,$4c,$44,$4c,$34,$0,$0,$0
font_char67:	dc.b	$40,$40,$78,$44,$44,$44,$78,$0,$0,$0
font_char68:	dc.b	$0,$0,$3c,$40,$40,$40,$3c,$0,$0,$0
font_char69:	dc.b	$4,$4,$3c,$44,$44,$44,$3c,$0,$0,$0
font_char70:	dc.b	$0,$0,$38,$44,$7c,$40,$3c,$0,$0,$0
font_char71:	dc.b	$8,$14,$10,$38,$10,$10,$10,$0,$0,$0
font_char72:	dc.b	$0,$0,$38,$44,$44,$44,$3c,$4,$38,$0
font_char73:	dc.b	$40,$40,$78,$44,$44,$44,$44,$0,$0,$0
font_char74:	dc.b	$10,$0,$10,$10,$10,$10,$8,$0,$0,$0
font_char75:	dc.b	$10,$0,$10,$10,$10,$10,$10,$10,$20,$0
font_char76:	dc.b	$40,$40,$44,$48,$50,$68,$44,$0,$0,$0
font_char77:	dc.b	$10,$10,$10,$10,$10,$10,$8,$0,$0,$0
font_char78:	dc.b	$0,$0,$68,$54,$54,$54,$54,$0,$0,$0
font_char79:	dc.b	$0,$0,$78,$44,$44,$44,$44,$0,$0,$0
font_char80:	dc.b	$0,$0,$38,$44,$44,$44,$38,$0,$0,$0
font_char81:	dc.b	$0,$0,$78,$44,$44,$44,$78,$40,$40,$0
font_char82:	dc.b	$0,$0,$3c,$44,$44,$44,$3c,$4,$4,$0
font_char83:	dc.b	$0,$0,$58,$64,$40,$40,$40,$0,$0,$0
font_char84:	dc.b	$0,$0,$38,$40,$38,$4,$38,$0,$0,$0
font_char85:	dc.b	$10,$10,$38,$10,$10,$10,$c,$0,$0,$0
font_char86:	dc.b	$0,$0,$44,$44,$44,$44,$3c,$0,$0,$0
font_char87:	dc.b	$0,$0,$44,$44,$44,$28,$10,$0,$0,$0
font_char88:	dc.b	$0,$0,$44,$44,$44,$54,$28,$0,$0,$0
font_char89:	dc.b	$0,$0,$44,$28,$10,$28,$44,$0,$0,$0
font_char90:	dc.b	$0,$0,$44,$44,$44,$44,$3c,$4,$38,$0
font_char91:	dc.b	$0,$0,$7c,$8,$10,$20,$7c,$0,$0,$0
font_char92:	dc.b	$8,$10,$10,$20,$10,$10,$8,$0,$0,$0
font_char93:	dc.b	$10,$10,$10,$10,$10,$10,$10,$0,$0,$0
font_char94:	dc.b	$20,$10,$10,$8,$10,$10,$20,$0,$0,$0
font_char95:	dc.b	$14,$28,$0,$0,$0,$0,$0,$0,$0,$0
font_char96:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char97:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char98:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char99:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char100:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char101:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char102:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char103:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char104:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char105:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char106:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char107:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char108:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char109:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char110:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char111:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char112:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char113:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char114:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char115:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char116:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char117:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char118:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char119:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char120:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char121:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char122:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char123:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char124:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char125:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char126:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0
font_char127:	dc.b	$38,$44,$5c,$64,$5c,$44,$38,$0,$0,$0

