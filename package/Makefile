include ../Makefile.include

PKG_FILE=tonegridpkg.pas
OUT_DIR=lib/$(ARCH_OS)

package:
	mkdir -p $(OUT_DIR)

	$(CC) -c cmidi.c -o $(OUT_DIR)/cmidi.o

	fpc $(PKG_FILE) -MObjFPC -Scgi -O1 -gl -vewnhi -l \
	-Fu$(LAZ_DIR)/lcl/units/$(ARCH_OS)/ \
	-Fu$(LAZ_DIR)/lcl/units/$(ARCH_OS)/gtk2/ \
	-Fu$(LAZ_DIR)/packager/units/$(ARCH_OS)/ \
	-Fu. -FU$(OUT_DIR) -dLCL -dLCLgtk2

clean:
	rm -rfv $(OUT_DIR)

all: package
