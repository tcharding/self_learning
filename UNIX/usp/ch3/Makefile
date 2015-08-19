CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_POSIX_C_SOURCE=200112L

.PHONY: all clean book

all: runsim testsim
book: chain fan

chain: simplechain.o
	$(CC) -o $@ simplechain.o

fan: simplefan.o
	$(CC) -o $@ simplefan.o

runsim: runsim.o makeargv.o restart.o
	$(CC) -o $@ runsim.o makeargv.o restart.o

testsim: testsim.o
	$(CC) -o $@ testsim.o

%.o: %.c
	$(CC) $(CFLAG) -c $< 

clean:
	@for f in *.o runsim testsim chain fan; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
