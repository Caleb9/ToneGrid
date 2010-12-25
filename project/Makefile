include ../Makefile.include

PRJ_NAME=tonegridproject
PRJ_PATH=.
LPR_FILE=$(PRJ_PATH)/$(PRJ_NAME).lpr
OUT_FILE=$(PRJ_PATH)/$(PRJ_NAME)

project:
	fpc $(LPR_FILE) \
	-MObjFPC -Scgi -O1 -gl -WG -vewnhi -l \
	-Fu../package/lib/$(ARCH_OS)/ \
	-Fu$(LAZ_DIR)/lcl/units/$(ARCH_OS)/ \
	-Fu$(LAZ_DIR)/lcl/units/$(ARCH_OS)/gtk2/ \
	-Fu$(LAZ_DIR)/packager/units/$(ARCH_OS)/ \
	-Fu$(PRJ_PATH) -o$(OUT_FILE) -dLCL -dLCLgtk2

clean:
	rm -fv $(OUT_FILE) *.{bak,ppu,ppw,ppl,o,a} *~

all: project
