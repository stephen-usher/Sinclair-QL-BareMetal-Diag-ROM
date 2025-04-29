VASM=vasmm68k_mot
VASMOPT=-Fbin -m68008 -no-opt
ROM_TEST_DEST=~/Emulators/QL/sQLux-1.0.7b/roms
SOURCE=diagrom.s library.s tests.s strings.inc font.inc

diagrom.bin:	$(SOURCE)
	$(VASM) $(VASMOPT)  -o diagrom.bin diagrom.s

clean:
	rm diagrom.bin

copy: diagrom.bin
	dd conv=sync bs=49152 if=diagrom.bin of=diagromp.bin
	cp diagromp.bin $(ROM_TEST_DEST)/diagrom.bin
	rm diagromp.bin
