CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L../../lib -ltch
INC = -I../../include -I../include
SPLINT = lint.out
EXES = ring ring1 ring2 ring3 ring4 ring5 ringtopology ringchildbreak \
	ringnobreak biring sring sbiring torus fib token_mutex hostgen imp \
DEV = hostgen imp 

.PHONY: all clean dev

dev: clean $(DEV) lint_dev

lint_dev:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(DEV) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../include -warnposix -nullderef -nullpass -unrecog \
	-unreachable > $(SPLINT); \
	fi; done

all: $(EXES) lint_all

imp: imp.o tokenring.o
	$(CC) -o $@ imp.o tokenring.o $(LFLAG)

fib: fib.o
	$(CC) -o $@ fib.o $(LFLAG)

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

ringchildbreak: ringchildbreak.o
	$(CC) -o $@ ringchildbreak.o $(LFLAG)

ringnobreak: ringnobreak.o
	$(CC) -o $@ ringnobreak.o $(LFLAG)

biring: biring.o
	$(CC) -o $@ biring.o $(LFLAG)

sring: sring.o
	$(CC) -o $@ sring.o $(LFLAG)

sbiring: sbiring.o
	$(CC) -o $@ sbiring.o $(LFLAG)

torus: torus.o
	$(CC) -o $@ torus.o $(LFLAG)

token_mutex: token_mutex.o
	$(CC) -o $@ token_mutex.o $(LFLAG)

hostgen: hostgen.o 
	$(CC) -o $@ hostgen.o $(LFLAG) -L../lib -lrestart

lint_all:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(EXES) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../include -warnposix -nullderef -nullpass > $(SPLINT); \
	fi; done

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

tokenring: tokenring.c tokenring.h
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in *.o $(EXES) $(SPLINT); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

