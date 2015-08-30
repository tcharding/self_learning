CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE -g
LFLAG = -L../../../lib -ltch
INC = -I../../../include -I../include
SPLINT = lint.out
EXES =	ppp 
TESTS = tst-vector tst-data tst-processchar tst-parse-config

.PHONY: all clean tests dev lint_all lint_adt

dev: ppp lint_dev
all: $(EXES) $(TESTS) lint_all
tests: $(TESTS)

# test executables (tracer bullets!)
tst-parse-config: string.o data.o vector.o tst-parse-config.o parse-config.o 
	$(CC) -o $@ tst-parse-config.o parse-config.o \
	string.o vector.o data.o $(LFLAG)

tst-data: tst-data.o data.o string.o
	$(CC) -o $@ tst-data.o data.o string.o $(LFLAG)

tst-vector: tst-vector.o vector.o data.o string.o
	$(CC) -o $@ tst-vector.o vector.o data.o string.o $(LFLAG)

tst-processchar: tst-processchar.o processchar.o
	$(CC) -o $@ tst-processchar.o processchar.o $(LFLAG)

# executables
ppp: string.o vector.o data.o parse-config.o ppp.o
	$(CC) -o $@ ppp.o data.o vector.o string.o parse-config.o $(LFLAG)

# object only (library) rules
string: string.h
	$(CC) $(CFLAG) -c $< $(INC)

data: data.h
	$(CC) $(CFLAG) -c $< $(INC)

vector: vector.h
	$(CC) $(CFLAG) -c $< $(INC)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint_all: lint_adt lint_exe lint_dev lint_tests
lint_adt: lint_string lint_data lint_vector

lint_exe:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(EXES) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../../include -warnposix -nullderef -nullpass > $(SPLINT); \
	fi; done

lint_tests:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(TESTS) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../../include -warnposix -nullderef -nullpass > $(SPLINT); \
	fi; done


lint_dev:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(DEV) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../../include -warnposix -nullderef -nullpass > $(SPLINT); \
	fi; done

lint_string:
	splint string.c -I../../../include -warnposix -nullderef -nullpass -unrecog \
	-unreachable -nullret > $(SPLINT);

lint_data:
	splint data.c -I../../../include -warnposix -nullderef -nullpass -unrecog \
	-unreachable -nullret > $(SPLINT);

lint_vector:
	splint vector.c -I../../../include -warnposix -nullderef -nullpass -unrecog \
	-unreachable -nullret > $(SPLINT);

clean:
	@for f in *.o $(EXES) $(TESTS) $(SPLINT); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

