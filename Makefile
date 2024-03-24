CHEZ_BIN:=chez-scheme
CHEZ_ARCH:=ta6fb
CHEZ_VER:=9.6.4

CHEZ_LIBHOME=${CHEZ_HOME}/lib/csv${CHEZ_VER}/${CHEZ_ARCH}
CFLAGS=-Wall -I$(CHEZ_LIBHOME) -L$(CHEZ_LIBHOME) \
 -DCHEZ_LIBHOME=\"$(CHEZ_LIBHOME)\" -L/usr/local/lib
LFLAGS=-lossp-uuid -liconv -lm -lpthread -lcurses

all: run

linux: CHEZ_ARCH=ta6le
linux: CHEZ_HOME=/home/linuxbrew/.linuxbrew/Cellar/chezscheme/9.5.8a
linux: LFLAGS= -lm -lcurses -lpthread -ldl -luuid
linux: all

run: run.c
	$(CC) $(CFLAGS) -g -m64 -o $@ $< $(CHEZ_LIBHOME)/kernel.o $(LFLAGS)

test:
	#echo "(import (schasm))(test-schasm)(exit)" | $(CHEZ_BIN) -q schasm.ss
	echo "(import (elf))(test-elf)(exit)" | $(CHEZ_BIN) -q schasm.ss

.PHONY: clean
clean:
	-rm run basic.o

basic.o: basic.s
	$(CC) -m64 -c $<

.PHONY: dis
dis: basic.o
	objdump -d basic.o
