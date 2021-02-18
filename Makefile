CHEZ_ARCH:=ta6fb

CHEZ_LIBHOME=${CHEZ_HOME}/lib/csv9.5.4/${CHEZ_ARCH}
CHEZ_EQUATES=${CHEZ_HOME}/../boot/${CHEZ_ARCH}
CFLAGS=-Wall -I$(CHEZ_LIBHOME) -I$(CHEZ_EQUATES) -L$(CHEZ_LIBHOME) \
 -DCHEZ_LIBHOME=\"$(CHEZ_LIBHOME)\" -L/usr/local/lib

all: run

linux: CHEZ_ARCH=ta6le
linux: all

run: run.c
	$(CC) $(CFLAGS) -g -m64 -o $@ $< $(CHEZ_LIBHOME)/kernel.o -lm -lcurses -lpthread -ldl -luuid #-lossp-uuid -liconv

.PHONY: clean
clean:
	-rm run basic.o

basic.o: basic.s
	$(CC) -m64 -c $<

.PHONY: dis
dis: basic.o
	objdump -d basic.o
