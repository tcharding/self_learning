CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L../../lib -ltch -lreadline
INC = -I../../include	
SPLINT = lint.out
SPLEXE = lint.exe.out
SPLLIB = lint.lib.out
SPLTESTS = lint.tests.out
SRCS = exec.c input.c 
OBJS = $(SRCS:.c=.o)
EXE = main
TESTS = tst-parse

.PHONY: all clean tests

all: main tests lint_lib
main: $(EXE) lint_exe
tests: $(TESTS) lint_tests

main: $(OBJS) main.o ush.h
	$(CC) -o $@ $(OBJS) main.o $(LFLAG)

tst-parse: input.o tst-parse.o
	$(CC) -o $@ input.o tst-parse.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint_exe: 
	@if [ -f "$$SPLEXE" ]; then rm "$$SPLEXE"; fi
	@echo 'lint_exe' > $(SPLEXE)
	@for f in $(EXE) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../include -warnposix -nullderef -nullpass -unrecog \
	-preproc >> $(SPLEXE); \
	fi; done

lint_lib: 
	@if [ -f "$$SPLLIB" ]; then rm "$$SPLLIB"; fi
	@echo 'lint_lib' > $(SPLLIB)
	@for f in $(SRCS) ; do \
	if [ -f $$f ]; then \
	splint $$f -I../../include -warnposix -nullderef -nullpass -unrecog \
	-preproc >> $(SPLLIB); \
	fi; done

lint_tests: 
	@if [ -f "$$SPLTESTS" ]; then rm "$$SPLTESTS"; fi
	@echo 'lint_tests' > $(SPLTESTS)
	@for f in $(TESTS) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../include -warnposix -nullderef -nullpass -unrecog \
	-preproc >> $(SPLTESTS); \
	fi; done

clean:
	@for f in *.o $(EXE) $(TESTS) ; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

