CC = gcc
CFLAG = -Wall -pedantic -std=c99 
LFLAG = -L../lib -lapue 
INC = -I../include 
SPLINT = lint.out 
EXES = sig2string multi_alarm sync longsleep readwrite fwrite
DEV = fwrite

dev: $(DEV) readwrite

all: $(EXES) lint_all

fwrite: fwrite.o
	$(CC) -o $@ fwrite.o $(LFLAG)

readwrite: readwrite.o
	$(CC) -o $@ readwrite.o $(LFLAG)

longsleep: longsleep.o
	$(CC) -o $@ longsleep.o $(LFLAG)

sig2string: sig2string.o
	$(CC) -o $@ sig2string.o $(LFLAG)

multi_alarm: multi_alarm.o
	$(CC) -o $@ multi_alarm.o $(LFLAG)

sync: sync.o
	$(CC) -o $@ sync.o $(LFLAG)

prmask: prmask.o
	$(CC) -o $@ prmask.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint_all:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi 
	@for f in $(EXES) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../include -warnposix -nullderef -nullpass -unrecog \
	>> $(SPLINT); \
	fi; done

lint_dev:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi 
	@for f in $(DEV) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../include -warnposix -nullderef -nullpass -unrecog \
	>> $(SPLINT); \
	fi; done

clean:
	@for f in *.o $(EXES); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

