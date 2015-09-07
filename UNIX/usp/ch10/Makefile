CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L../../lib -ltch
INC = -I../../include	
SPLINT = lint.out
SRCS = show.c virtualtimers.c hardwaretimer.c
OBJS = $(SRCS:.c=.o)
EXES = timermain
TESTS = test-runner tst-virtualtimers
DEV = timermain

.PHONY: all clean dev tests

all: $(EXES) lint_all
dev: $(DEV) lint_dev
tests: $(TESTS)

timermain: $(OBJS) timermain.o
	$(CC) -o $@ timermain.o $(OBJS) $(LFLAG)

# tests
test-runner: $(OBJS) test-runner.o
	$(CC) -o $@ $(OBJS) test-runner.o $(LFLAG)

tst-virtualtimers: show.o hardwaretimer.o virtualtimers.o tst-virtualtimers.o
	$(CC) -o $@ show.o virtualtimers.o hardwaretimer.o tst-virtualtimers.o $(LFLAG)

# object only rules
%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

virtualtimers.o: virtualtimers.c virtualtimers.h
	$(CC) $(CFLAG) -c $< $(INC)

hardwaretimer.o: hardwaretimer.c hardwaretimer.h
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint_all:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(EXES) hardwaretimer virtualtimers ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../include -warnposix -nullderef -nullpass -unrecog \
	>> $(SPLINT); \
	fi; done

lint_dev:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(DEV) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../include -warnposix -nullderef -nullpass -unrecog \
	>> $(SPLINT); \
	fi; done

clean:
	@for f in *.o $(EXES) $(DEV) $(TESTS); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

