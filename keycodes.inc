keycode_table:
	dc.l	KEYCODE_INVALID
	dc.l	KEYCODE_INVALID
	dc.l	KEYCODE_INVALID
	dc.l	KEYCODE_X
	dc.l	KEYCODE_V
	dc.l	KEYCODE_SLASH
	dc.l	KEYCODE_N
	dc.l	KEYCODE_COMMA
	dc.l	KEYCODE_8
	dc.l	KEYCODE_2
	dc.l	KEYCODE_6
	dc.l	KEYCODE_Q
	dc.l	KEYCODE_E
	dc.l	KEYCODE_0
	dc.l	KEYCODE_T
	dc.l	KEYCODE_U
	dc.l	KEYCODE_9
	dc.l	KEYCODE_W
	dc.l	KEYCODE_I
	dc.l	KEYCODE_TAB
	dc.l	KEYCODE_R
	dc.l	KEYCODE_MINUS
	dc.l	KEYCODE_Y
	dc.l	KEYCODE_O
	dc.l	KEYCODE_L
	dc.l	KEYCODE_3
	dc.l	KEYCODE_H
	dc.l	KEYCODE_1
	dc.l	KEYCODE_A
	dc.l	KEYCODE_P
	dc.l	KEYCODE_D
	dc.l	KEYCODE_J
	dc.l	KEYCODE_LEFT_BRACKET
	dc.l	KEYCODE_CAPSLOCK
	dc.l	KEYCODE_K
	dc.l	KEYCODE_S
	dc.l	KEYCODE_F
	dc.l	KEYCODE_EQUALS
	dc.l	KEYCODE_G
	dc.l	KEYCODE_SEMICOLON
	dc.l	KEYCODE_RIGHT_BRACKET
	dc.l	KEYCODE_Z
	dc.l	KEYCODE_FULL_STOP
	dc.l	KEYCODE_C
	dc.l	KEYCODE_B
	dc.l	KEYCODE_POUND
	dc.l	KEYCODE_M
	dc.l	KEYCODE_SINGLE_QUOTE
	dc.l	KEYCODE_ENTER
	dc.l	KEYCODE_LEFT
	dc.l	KEYCODE_UP
	dc.l	KEYCODE_ESC
	dc.l	KEYCODE_RIGHT
	dc.l	KEYCODE_BACKSLASH
	dc.l	KEYCODE_SPACE
	dc.l	KEYCODE_DOWN
	dc.l	KEYCODE_F4
	dc.l	KEYCODE_F1
	dc.l	KEYCODE_5
	dc.l	KEYCODE_F2
	dc.l	KEYCODE_F3
	dc.l	KEYCODE_F5
	dc.l	KEYCODE_4
	dc.l	KEYCODE_7

KEYCODE_INVALID	dc.b	"Invalid keycode"
KEYCODE_0:		dc.b	"0",$0
KEYCODE_1:		dc.b	"1",$0
KEYCODE_2:		dc.b	"2",$0
KEYCODE_3:		dc.b	"3",$0
KEYCODE_4:		dc.b	"4",$0
KEYCODE_5:		dc.b	"5",$0
KEYCODE_6:		dc.b	"6",$0
KEYCODE_7:		dc.b	"7",$0
KEYCODE_8:		dc.b	"8",$0
KEYCODE_9:		dc.b	"9",$0
KEYCODE_A:		dc.b	"A",$0
KEYCODE_B:		dc.b	"B",$0
KEYCODE_BACKSLASH:	dc.b	"\",$0
KEYCODE_C:		dc.b	"C",$0
KEYCODE_CAPSLOCK:	dc.b	"Capslock",$0
KEYCODE_COMMA:		dc.b	",",$0
KEYCODE_D:		dc.b	"D",$0
KEYCODE_DOWN:		dc.b	"Down arrow",$0
KEYCODE_E:		dc.b	"E",$0
KEYCODE_ENTER:		dc.b	"Enter",$0
KEYCODE_EQUALS:		dc.b	"=",$0
KEYCODE_ESC:		dc.b	"ESC",$0
KEYCODE_F:		dc.b	"F",$0
KEYCODE_F1:		dc.b	"F1",$0
KEYCODE_F2:		dc.b	"F2",$0
KEYCODE_F3:		dc.b	"F3",$0
KEYCODE_F4:		dc.b	"F4",$0
KEYCODE_F5:		dc.b	"F5",$0
KEYCODE_FULL_STOP:	dc.b	".",$0
KEYCODE_G:		dc.b	"G",$0
KEYCODE_H:		dc.b	"H",$0
KEYCODE_I:		dc.b	"I",$0
KEYCODE_J:		dc.b	"J",$0
KEYCODE_K:		dc.b	"K",$0
KEYCODE_L:		dc.b	"L",$0
KEYCODE_LEFT:		dc.b	"Left arrow",$0
KEYCODE_LEFT_BRACKET:	dc.b	"(",$0
KEYCODE_M:		dc.b	"M",$0
KEYCODE_MINUS:		dc.b	"-",$0
KEYCODE_N:		dc.b	"N",$0
KEYCODE_O:		dc.b	"O",$0
KEYCODE_P:		dc.b	"P",$0
KEYCODE_POUND:		dc.b	"Pound",$0
KEYCODE_Q:		dc.b	"Q",$0
KEYCODE_R:		dc.b	"R",$0
KEYCODE_RIGHT:		dc.b	"Right arrow",$0
KEYCODE_RIGHT_BRACKET:	dc.b	")",$0
KEYCODE_S:		dc.b	"S",$0
KEYCODE_SEMICOLON:	dc.b	";",$0
KEYCODE_SINGLE_QUOTE:	dc.b	"'",$0
KEYCODE_SLASH:		dc.b	"/",$0
KEYCODE_SPACE:		dc.b	"Space",$0
KEYCODE_T:		dc.b	"T",$0
KEYCODE_TAB:		dc.b	"TAB",$0
KEYCODE_U:		dc.b	"U",$0
KEYCODE_UP:		dc.b	"Up arrow",$0
KEYCODE_V:		dc.b	"V",$0
KEYCODE_W:		dc.b	"W",$0
KEYCODE_X:		dc.b	"X",$0
KEYCODE_Y:		dc.b	"Y",$0
KEYCODE_Z:		dc.b	"Z",$0

key_shift_txt:	dc.b	"Shift ",$0
key_ctl_txt:	dc.b	"Ctrl ",$0
key_alt_txt:	dc.b	"Alt ",$0
