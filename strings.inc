
;*********************************************************
;
; Text strings.
;
;*********************************************************

escbrack:	dc.b	$1b,"[",$0
clrandhome:	dc.b	$1b,"[2J",$1b,"[H",$0
crlftxt:	dc.b	$0d,$0a,$0

bannertext:
	dc.b	"QL Bare Metal Diagnostic ROM version 0.00",$0d,$0a,$0
memtststarttxt:
	dc.b	"Starting initial 128K memory test.",$0d,$0a,$0
memerrtext:
	dc.b	"Memory test error, halting.",$0d,$0a,$0
memtstcomptext:
	dc.b	"Initial memory test complete.",$0d,$0a,$0
init_ipc_start_txt:
	dc.b	"Starting initial IPC tests.",$0d,$0a,$0
init_ipc_reset_txt:
	dc.b	"IPC reset: ",$0
init_ipc_selftest_txt:
	dc.b	"IPC selftest: ",$0

qtclocktxt:
	dc.b	"Testing clock.",$0d,$0a,$0

donetxt:
	dc.b	"done.",$0d,$0a,$0
completedtxt:
	dc.b	"completed.",$0d,$0a,$0
testingtxt:
	dc.b	"Testing: ",$0
failedtxt:
	dc.b	"Failed.",$0d,$0a,$0
