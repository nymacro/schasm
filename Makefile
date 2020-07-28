CHEZ_LIBHOME=${CHEZ_HOME}/lib/csv9.5.3/ta6fb
CHEZ_EQUATES=${CHEZ_HOME}/../boot/ta6fb
CFLAGS=-I$(CHEZ_LIBHOME) -I$(CHEZ_EQUATES) -L$(CHEZ_LIBHOME) -L/usr/local/lib

run: run.c
	$(CC) $(CFLAGS) -g -m64 -o $@ $< $(CHEZ_LIBHOME)/kernel.o -lm -lcurses -lpthread -lossp-uuid -liconv

basic.o: basic.s
	$(CC) -m64 -c $<

.PHONY: dis
dis: basic.o
	objdump -d basic.o
