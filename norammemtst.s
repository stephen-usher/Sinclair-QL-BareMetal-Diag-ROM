; d2 holds the memory test sequence number
; d3 holds the test value to write
; d4 holds the current value from memory
; 
; a1 used for memory address reference

memtstsizel	equ	$7fff
memtstsizeh	equ	$0001
	
memtest:
	move.l	#0,d2		; clear out the data registers
	move.l	#0,d3
	move.l	#0,d4

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
	move.b	(a1),d4
	cmp.b	d4,d3	; Test if the memory matches the lowest byte of the address.
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

	move.b	#$FF,d3
marchtestp2:
	move.b	(a1),d4
	cmp.b	d4,d3		; Test if what we read is what we wrote.
	bne	memerror	; If it's not error and halt.
	eori.b	#-1,(a1)+	; XOR the memory value with all ones.
	dbf	d0,marchtestp2	; Go around again until all memory has been done.
	dbf	d1,marchtestp2	; Go around again until all memory has been done.

	move.l	#3,d2		; Memory test 3

	move.l	#memtstsizel,d0	; Set up the RAM test loop counters
	move.l	#memtstsizeh,d1

	lea	ramstart,a1	; Load the base of RAM into a1

	move.b	#$00,d3
marchtestp3:
	move.b	(a1),d4
	cmp.b	d4,d3		; Test if what we read is the opposite of what we wrote.
	bne	memerror	; If it's not error and halt.
	eori.b	#-1,(a1)+	; XOR the memory value with all ones.
	dbf	d0,marchtestp3	; Go around again until all memory has been done.
	dbf	d1,marchtestp3	; Go around again until all memory has been done.

	move.l	#4,d2		; Memory test 4

	move.l	#memtstsizel,d0	; Set up the RAM test loop counters
	move.l	#memtstsizeh,d1

	lea	ramstart,a1	; Load the base of RAM into a1

	move.b	#$FF,d3
marchtestp4:
	move.b	(a1),d4
	cmp.b	d4,d3		; Test if what we read is what we wrote originally.
	bne	memerror	; If it's not error and halt.
	eori.b	#-1,(a1)+	; XOR the memory value with all ones.
	dbra	d0,marchtestp4	; Go around again until all memory has been done.
	dbra	d1,marchtestp4	; Go around again until all memory has been done.

memtstend:

	bra	endmemtst	; The memory test has completed successfully.

; memerror
;
;	
; d2 holds the memory test sequence number
; d3 holds the test value to write
; d4 holds the current value from memory
; 
; a1 used for memory address reference
;
; Can't use d4, d6, a4, a5 and a6  as these used by the string printing routine.
;

memerror:
	move.l	d4,d5		; Move the faulting memory value out of d4 for safety

	lea	memerrtext,a5	; Put the address of the memory error text into a5
	lea	memerrorprtret1,a6
	bra	noram_print_string
memerrorprtret1:

	move.l	a1,d0		; d0 now holds the faulting address.

	lsr.l	#8,d0		; Shift the top nibble down
	lsr.l	#8,d0
	lsr.l	#8,d0
	lsr.l	#4,d0

	lea	memerrprtnib1,a6
	bra	nr_nib2hex
memerrprtnib1:
	lea	memerrprtnib1p,a6
	bra	noram_print_chr
memerrprtnib1p:

	move.l	a1,d0		; d0 now holds the faulting address.

	lsr.l	#8,d0		; Shift the top nibble down
	lsr.l	#8,d0
	lsr.l	#8,d0

	lea	memerrprtnib2,a6
	bra	nr_nib2hex
memerrprtnib2:
	lea	memerrprtnib2p,a6
	bra	noram_print_chr
	bra	nr_nib2hex
memerrprtnib2p:

	move.l	a1,d0		; d0 now holds the faulting address.

	lsr.l	#8,d0		; Shift the top nibble down
	lsr.l	#8,d0
	lsr.l	#4,d0

	lea	memerrprtnib3,a6
	bra	nr_nib2hex
memerrprtnib3:
	lea	memerrprtnib3p,a6
	bra	noram_print_chr
	bra	nr_nib2hex
memerrprtnib3p:

	move.l	a1,d0		; d0 now holds the faulting address.

	lsr.l	#8,d0		; Shift the top nibble down
	lsr.l	#8,d0

	lea	memerrprtnib4,a6
	bra	nr_nib2hex
memerrprtnib4:
	lea	memerrprtnib4p,a6
	bra	noram_print_chr
memerrprtnib4p:

	move.l	a1,d0		; d0 now holds the faulting address.

	lsr.l	#8,d0		; Shift the top nibble down
	lsr.l	#4,d0

	lea	memerrprtnib5,a6
	bra	nr_nib2hex
memerrprtnib5:
	lea	memerrprtnib5p,a6
	bra	noram_print_chr
memerrprtnib5p:

	move.l	a1,d0		; d0 now holds the faulting address.

	lsr.l	#8,d0		; Shift the top nibble down
	lsr.l	#8,d0

	lea	memerrprtnib6,a6
	bra	nr_nib2hex
memerrprtnib6:
	lea	memerrprtnib6p,a6
	bra	noram_print_chr
memerrprtnib6p:

	move.l	a1,d0		; d0 now holds the faulting address.

	lsr.l	#4,d0		; Shift the top nibble down

	lea	memerrprtnib7,a6
	bra	nr_nib2hex
memerrprtnib7:
	lea	memerrprtnib7p,a6
	bra	noram_print_chr
memerrprtnib7p:

	move.l	a1,d0		; d0 now holds the faulting address.

	lea	memerrprtnib8,a6
	bra	nr_nib2hex
memerrprtnib8:
	lea	memerrprtnib8p,a6
	bra	noram_print_chr
memerrprtnib8p:

	lea	memerrbitstxt,a5	; Put the address of the memory error text into a5
	lea	memerrorprtret2,a6
	bra	noram_print_string
memerrorprtret2:

; d3 holds test data d5 holds actual data.
; Use d2.b to hold the bad bit mask, use the second byte of d2 to hold the memory bank, 0 or ff (useful later)

	move.l	#0,d2
	move.b	d5,d2
	eor.b	d3,d2

	move.l	ramstart,d4	; Determine where the bottom 64 ends.
	addi.l	#$10000,d4

	cmp.l	a4,d4
	ble	memerrorprtskip1

	addi.w	#$ff00,d2	; Top byte now has 0xff, which can be used later for screen bank display

memerrorprtskip1:

	move.l	#$00,d4		; copy the bad bit mask into our working register, d4
	move.b	d2,d4

	lsr.b	#7,d4		; Shift it down.
	andi.b	#$01,d4		; Mask all but the bottom bit.
	addi.b	#$30,d4		; Add the value of ascii '0'... neat trick.

	move.b	d4,d0
	lea	memerrbbit7ret,a6
	bra	noram_print_chr
memerrbbit7ret:

	move.b	d2,d4

	lsr.b	#6,d4		; Shift it down.
	andi.b	#$01,d4		; Mask all but the bottom bit.
	addi.b	#$30,d4		; Add the value of ascii '0'... neat trick.

	move.b	d4,d0
	lea	memerrbbit6ret,a6
	bra	noram_print_chr
memerrbbit6ret:

	move.b	d2,d4

	lsr.b	#5,d4		; Shift it down.
	andi.b	#$01,d4		; Mask all but the bottom bit.
	addi.b	#$30,d4		; Add the value of ascii '0'... neat trick.

	move.b	d4,d0
	lea	memerrbbit5ret,a6
	bra	noram_print_chr
memerrbbit5ret:

	move.b	d2,d4

	lsr.b	#4,d4		; Shift it down.
	andi.b	#$01,d4		; Mask all but the bottom bit.
	addi.b	#$30,d4		; Add the value of ascii '0'... neat trick.

	move.b	d4,d0
	lea	memerrbbit4ret,a6
	bra	noram_print_chr
memerrbbit4ret:

	move.b	d2,d4

	lsr.b	#3,d4		; Shift it down.
	andi.b	#$01,d4		; Mask all but the bottom bit.
	addi.b	#$30,d4		; Add the value of ascii '0'... neat trick.

	move.b	d4,d0
	lea	memerrbbit3ret,a6
	bra	noram_print_chr
memerrbbit3ret:

	move.b	d2,d4

	lsr.b	#2,d4		; Shift it down.
	andi.b	#$01,d4		; Mask all but the bottom bit.
	addi.b	#$30,d4		; Add the value of ascii '0'... neat trick.

	move.b	d4,d0
	lea	memerrbbit2ret,a6
	bra	noram_print_chr
memerrbbit2ret:

	move.b	d2,d4

	lsr.b	#1,d4		; Shift it down.
	andi.b	#$01,d4		; Mask all but the bottom bit.
	addi.b	#$30,d4		; Add the value of ascii '0'... neat trick.

	move.b	d4,d0
	lea	memerrbbit1ret,a6
	bra	noram_print_chr
memerrbbit1ret:


	move.b	d2,d4

	andi.b	#$01,d4		; Mask all but the bottom bit.
	addi.b	#$30,d4		; Add the value of ascii '0'... neat trick.

	move.b	d4,d0
	lea	memerrbbit0ret,a6
	bra	noram_print_chr
memerrbbit0ret:

	lea	crlftxt,a5
	lea	memerrcrlf1,a6
	bra	noram_print_string
memerrcrlf1:

;
; That's the end of the output to the serial port.
;
; Now lets put something on the screen.
;
; d2.w holds the information we need.
;
; The top byte encodes whether it's the lower (0x00) or upper (0xff) memory bank.
;
; The aim is to write two 10 pixel deep bands across the screen per bit
; 
;	First band, memory bank, white lower, black higher.
;	Second band, bit OK, green yes, red no.

	lea	ramstart,a0

	move.w	d2,d3			; We need to generate the bank pixel value.
	andi.w	#$ff00,d4
	lsr	#8,d4
	mulu.w	#255,d4			; d4 now holds either 0x0000 or 0xffff

	andi.w	#$00ff,d2		; We no-longer need the bank information in d2.

	lea	memerrbankband1,a6
	move.w	d4,d0
	bra	memerrband
memerrbankband1:

	move.b	d2,d3			; Get bit 7 info
	lsr.b	#7,d3
	andi.b	#$01,d3
	cmpi.b	#1,d3			; Do we have a bad bit?
	beq	memerrbit7badpix	; Yes... Set the pixel to red
	move.w	#$ff00,d0
	jmp	memerrbit7skip1
memerrbit7badpix:
	move.w	#$00ff,d0
memerrbit7skip1:
	
	lea	memerrbit7band1,a6
	bra	memerrband
memerrbit7band1:

;	jsr	memerrendloop		; @@@@@@

	move.b	d2,d3			; Get bit 6 info
	lsr.b	#6,d3
	andi.b	#$01,d3
	cmpi.b	#1,d3			; Do we have a bad bit?
	beq	memerrbit6badpix	; Yes... Set the pixel to red
	move.w	#$ff00,d0
	jmp	memerrbit6skip1
memerrbit6badpix:
	move.w	#$00ff,d0
memerrbit6skip1:
	
	lea	memerrbit6band1,a6
	bra	memerrband
memerrbit6band1:

	move.b	d2,d3			; Get bit 5 info
	lsr.b	#5,d3
	andi.b	#$01,d3
	cmpi.b	#1,d3			; Do we have a bad bit?
	beq	memerrbit5badpix	; Yes... Set the pixel to red
	move.w	#$ff00,d0
	jmp	memerrbit5skip1
memerrbit5badpix:
	move.w	#$00ff,d0
memerrbit5skip1:

	lea	memerrbit5band1,a6
	bra	memerrband
memerrbit5band1:

	move.b	d2,d3			; Get bit 4 info
	lsr.b	#4,d3
	andi.b	#$01,d3
	cmpi.b	#1,d3			; Do we have a bad bit?
	beq	memerrbit4badpix	; Yes... Set the pixel to red
	move.w	#$ff00,d0
	jmp	memerrbit4skip1
memerrbit4badpix:
	move.w	#$00ff,d0
memerrbit4skip1:

	lea	memerrbit4band1,a6
	bra	memerrband
memerrbit4band1:

	move.b	d2,d3			; Get bit 3 info
	lsr.b	#3,d3
	andi.b	#$01,d3
	cmpi.b	#1,d3			; Do we have a bad bit?
	beq	memerrbit3badpix	; Yes... Set the pixel to red
	move.w	#$ff00,d0
	jmp	memerrbit3skip1
memerrbit3badpix:
	move.w	#$00ff,d0
memerrbit3skip1:

	lea	memerrbit3band1,a6
	bra	memerrband
memerrbit3band1:

	move.b	d2,d3			; Get bit 2 info
	lsr.b	#2,d3
	andi.b	#$01,d3
	cmpi.b	#1,d3			; Do we have a bad bit?
	beq	memerrbit2badpix	; Yes... Set the pixel to red
	move.w	#$ff00,d0
	jmp	memerrbit2skip1
memerrbit2badpix:
	move.w	#$00ff,d0
memerrbit2skip1:

	lea	memerrbit2band1,a6
	bra	memerrband
memerrbit2band1:

	move.b	d2,d3			; Get bit 1 info
	lsr.b	#1,d3
	andi.b	#$01,d3
	cmpi.b	#1,d3			; Do we have a bad bit?
	beq	memerrbit1badpix	; Yes... Set the pixel to red
	move.w	#$ff00,d0
	jmp	memerrbit1skip1
memerrbit1badpix:
	move.w	#$00ff,d0
memerrbit1skip1:

	lea	memerrbit1band1,a6
	bra	memerrband
memerrbit1band1:

	move.b	d2,d3			; Get bit 0 info
	andi.b	#$01,d3
	cmpi.b	#1,d3			; Do we have a bad bit?
	beq	memerrbit0badpix	; Yes... Set the pixel to red
	move.w	#$ff00,d0
	jmp	memerrbit0skip1
memerrbit0badpix:
	move.w	#$00ff,d0
memerrbit0skip1:

	lea	memerrbit0band1,a6
	bra	memerrband
memerrbit0band1:

memerrendloop:
	jmp	memerrendloop
	
;++++++
; memerrband - put a 10 pixel deep band across the screen, plus one black pixel line.
;
; Inputs:
;	a0 	- starting address
;	d0.w	- Pixel data.
;	a6	- return address
;
; Corrupted registers:
;	a0, d4
;

memerrband:
	move.l	#1279,d4			; The number of words for 10 lines of the display.

	move.l	#$0,d5
	move.w	d0,d5
	lsr.w	#8,d5

memerrbandloop1:
	move.b	d5,(a0)+		; Copy the pixel data into the screen pixel
	move.b	d0,(a0)+		; Copy the pixel data into the screen pixel
	dbf	d4,memerrbandloop1	; Loop around until d4 is zero

	move.l	#127,d4

memerrbandloop2:
	move.b	#0,(a0)+		; Copy the pixel data into the screen pixel
	dbf	d4,memerrbandloop2	; Loop around until d4 is zero

	jmp	(a6)			; return
;
; nr_nib2hex - No-RAM nibble to hex character
;
; Inputs:
;	d0.b - Byte containing the nibble to be translated in lower 4 bits.
;	a6   - Return address
;
;

nr_nib2hex:
	andi.b	#$0f,d0		; Clear out any top bits

	addi.b	#$30,d0		; Add the ascii offset for 0 to the value.

	cmp.b	#$3a,d0		; Is this now the colon character or greater?
	blt	nr_nib2hex_end	; If not then our job is done.

	addi.b	#$27,d0		; Add 0x27 to push it into the alpha characters

nr_nib2hex_end:
	jmp	(a6)
