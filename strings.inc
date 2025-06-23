
;*********************************************************
;
; Text strings.
;
;*********************************************************

escbrack:	dc.b	$1b,"[",$0
clrandhome:	dc.b	$1b,"[2J",$1b,"[H",$0
crlftxt:	dc.b	$0d,$0a,$0
spacetxt:	dc.b	" ",$0

end_basic_test_txt:
	dc.b	"Basic tests completed.",$0d,$0a,0

bannertext:
	dc.b	"QL Bare Metal Diagnostic ROM version 0.10",$0d,$0a,$0
memtststarttxt:
	dc.b	"Starting initial 128K memory test.",$0d,$0a,$0
memerrtext:
	dc.b	"Memory error at address 0x",$0
memerrbitstxt:
	dc.b	" bad bits: ",$0
memtstcomptext:
	dc.b	"Initial memory test complete.",$0d,$0a,$0
init_ipc_start_txt:
	dc.b	"Starting initial IPC tests.",$0d,$0a,$0
init_ipc_reset_txt:
	dc.b	"IPC reset: ",$0
init_ipc_selftest_txt:
	dc.b	"IPC selftest:",$0
init_ipc_beeptest_txt:
	dc.b	"IPC beep test: ",$0
init_ipc_keyreadtest_txt:
	dc.b	"IPC read keyboard test: Press keyboard key within 5 seconds",$0d,$0a,$0
init_ipc_keyreadyes_txt:
	dc.b	"Key pressed was: ",$0
init_ipc_keyreadyesheld_txt:
	dc.b	" and it was held down",$0
init_ipc_keyreadno_txt:
	dc.b	"No key was pressed",$0d,$0a,$0

qtclocktxt:
	dc.b	"Testing clock.",$0d,$0a,$0

sizememtxt:
	dc.b	"Finding extended memory: ",$0
sizememtoptxt:
	dc.b	$0d,$0a,"Top of memory is: ",$0
sizememsizetxt:
	dc.b	$0d,$0a,"Memory size: ",$0

extmemtstbantxt:
	dc.b	"Running expanded memory tests:",$d,$a,$0

extmemtstowntxt1:
	dc.b	"     Own address test: write: ",$0
extmemtstowntxt2:
	dc.b	"     Own address test: read: ",$0

extmemtstmarchtxt1:
	dc.b	"     March test: write: ",$0
extmemtstmarchtxt2:
	dc.b	"     March test: invert 1: ",$0
extmemtstmarchtxt3:
	dc.b	"     March test: invert 2: ",$0
extmemtstmarchtxt4:
	dc.b	"     March test: invert 3: ",$0

extmemtsterr1:
	dc.b	$0d,$0a,"Memory error: Address: ",$0
extmemtsterr2:
	dc.b	" should be: ",$0
extmemtsterr3:
	dc.b	" was: ",$0

donetxt:
	dc.b	"done.",$0d,$0a,$0
completedtxt:
	dc.b	"completed.",$0d,$0a,$0
testingtxt:
	dc.b	"Testing: ",$0
failedtxt:
	dc.b	"Failed.",$0d,$0a,$0
wrotetxt:
	dc.b	" Wrote: ",$0
wrotehextxt:
	dc.b	" Wrote: 0x",$0
readtxt:
	dc.b	" Read: ",$0
readhextxt:
	dc.b	" Read: 0x",$0
spacecolonspacetxt:
	dc.b	" : ",$0
