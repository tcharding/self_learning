CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L../../lib -ltch
INC = -I../../include	
SPLINT = lint.out
EXES = ring ring1 ring2 ring3 ring4 ring5 ringtopology

.PHONY: all clean dev

all: $(EXES) lint

ring: ring.o
	$(CC) -o $@ ring.o $(LFLAG)

ring1: ring1.o
	$(CC) -o $@ ring1.o $(LFLAG)

ring2: ring2.o
	$(CC) -o $@ ring2.o $(LFLAG)

ring3: ring3.o
	$(CC) -o $@ ring3.o $(LFLAG)

ring4: ring4.o
	$(CC) -o $@ ring4.o $(LFLAG)

ring5: ring5.o
	$(CC) -o $@ ring5.o $(LFLAG)

ringtopology: ringtopology.o
	$(CC) -o $@ ringtopology.o $(LFLAG)

lint:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(EXES) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../include -warnposix > $(SPLINT); \
	fi; done

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in *.o $(EXES) ; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

