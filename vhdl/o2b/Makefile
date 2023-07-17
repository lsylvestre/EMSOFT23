
SRC=dram
DST=gen-platform

STACK_SIZE=6400
HEAP_SIZE=131072


all:
	cp -R ../$(SRC) ../$(DST)

	mkdir -p ../$(DST)/rtl
	for f in rtl/*.vhd; do (cp -R $${f} "../$(DST)/$${f}"); done
	for f in qsys/*_hw.tcl; do (cp -R $${f} "../$(DST)/$${f}"); done
	cat qsys/platform.tcl > "../$(DST)/qsys/platform_hw.tcl"
	for f in qsys/*_ext.tcl; do (cat $${f} >> "../$(DST)/qsys/platform_hw.tcl"); done
	echo "save_system {platform.qsys}\n" >> "../$(DST)/qsys/platform_hw.tcl"

	# '-' ignore les erreurs
	-(cd ../$(DST)/qsys/ ; rm -R platform.qsys; qsys-script --script=platform_hw.tcl) 

	qsys-generate --synthesis=VHDL ../$(DST)/qsys/platform.qsys

	cp bsp/bsp_update.tcl ../$(DST)/bsp/bsp_update.tcl

	(cd ../$(DST); nios2-bsp-update-settings --script=bsp/bsp_update.tcl --bsp-dir=bsp --settings=bsp/settings.bsp)

	(cd ../$(DST) ; make hw-build ; make omicrob ; nios2-bsp-generate-files --bsp-dir=bsp --settings=bsp/settings.bsp)

	cp bsp/bsp_onchip.tcl ../$(DST)/bsp/bsp_onchip.tcl
	cp bsp/bsp_sdram.tcl ../$(DST)/bsp/bsp_sdram.tcl

	echo "\nonchip:\n	nios2-bsp-update-settings --script=bsp/bsp_onchip.tcl --bsp-dir=bsp --settings=bsp/settings.bsp; nios2-bsp-generate-files --bsp-dir=bsp --settings=bsp/settings.bsp\n" >> ../$(DST)/Makefile

	echo "sdram:\n	nios2-bsp-update-settings --script=bsp/bsp_sdram.tcl --bsp-dir=bsp --settings=bsp/settings.bsp; nios2-bsp-generate-files --bsp-dir=bsp --settings=bsp/settings.bsp\n" >> ../$(DST)/Makefile

clean:
	rm -Rf apps c ml qsys
	rm -f bsp/bsp_update.tcl
	rm -f  rtl/main.vhd rtl/main_cc.vhd c/*.c  c/*.h  ml/*.ml  ml/*.mli qsys/*_hw.tcl qsys/*_ext.tcl

check:
	ghdl -a -fno-color-diagnostics rtl/runtime.vhd rtl/main.vhd rtl/main_cc.vhd;
	rm -f *.o *.cf
