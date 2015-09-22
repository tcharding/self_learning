CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L../lib -lapue 
INC = -I../include 
SPLINT_LIBS = lint.out.libs
SPLINT_EXES = lint.out.exes
# source files without a main() function
LIBS = initserver.c sig_io.c
OBJS = $(LIBS:.c=.o)
# executables
EXES = endianess sockstat tcp-ruptimed tcp-ruptimed-nodelay
RPS = rpsd rps-cli

.PHONY: all libs clean lint_all rps

all: $(EXES) lint_all
rps: $(RPS) 
libs: $(OBJS) lint_libs

rps-cli: rps-cli.o
	$(CC) -o $@ rps-cli.o $(LFLAG)

rpsd: initserver.o rpsd.o
	$(CC) -o $@ initserver.o rpsd.o $(LFLAG)

tcp-ruptimed: initserver.o tcp-ruptimed.o
	$(CC) -o $@ initserver.o tcp-ruptimed.o $(LFLAG)

sockstat: sockstat.o
	$(CC) -o $@ sockstat.o $(LFLAG)

endianess: endianess.o
	$(CC) -o $@ endianess.o $(LFLAG)

tcp-ruptimed-nodelay: initserver.o tcp-ruptimed-nodelay.o
	$(CC) -o $@ initserver.o tcp-ruptimed-nodelay.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint_all: lint_exe lint_libs

lint_exe:
	@if [ -f "$$SPLINT_EXES" ]; then rm "$$SPLINT_EXES"; fi
	@echo '*** splint: lint_exe ***' > $(SPLINT_EXES)
	@for f in $(EXES) ; do \
	if [ -f $$f ]; then \
	splint $$f.c $(INC) -warnposix -nullderef -nullpass -unrecog \
	>> $(SPLINT_EXES); \
	fi; done

lint_libs: 
	@if [ -f "$$SPLINT_LIBS" ]; then rm "$$SPLINT_LIBS"; fi
	@echo '*** splint: lint_lib ***' > $(SPLINT_LIBS)
	@for f in $(LIBS) ; do \
	if [ -f $$f ]; then \
	splint $$f $(INC) -warnposix -nullderef -nullpass -unrecog \
	>> $(SPLINT_LIBS); \
	fi; done

clean:
	@for f in *.o $(EXES) $(SPLINT_LIBS) $(SPLINT_EXES); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

