run: run.c
	$(CC) -g -m64 -o $@ $<

basic.o: basic.s
	$(CC) -m64 -c $<

.PHONY: dis
dis: basic.o
	objdump -d basic.o
