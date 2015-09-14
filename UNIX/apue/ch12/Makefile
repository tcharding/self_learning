CC = gcc
CFLAG = -Wall -pedantic -std=c99 
LFLAG = -L../lib -lapue -lpthread
INC = -I../include 
SPLINT_LIB = lint.out.libs
SPLINT_EXES = lint.out.exes
# source files without a main() function
LIBS = putenv_r
OBJS = $(LIBS:.c=.o)
# executables
EXES = pthread_atfork 

.PHONY: all libs clean lint_all # tests dev

all: $(EXES) lint_all
libs: $(OBJS) lint_libs

pthread_atfork: pthread_atfork.o
	$(CC) -o $@ pthread_atfork.o $(LFLAG)

tst-nap: nap.o tst-nap.o 
	$(CC) -o $@ nap.o tst-nap.o $(LFLAG)

putenv_r.o: putenv_r.c 
	$(CC) $(CFLAG) -c $< $(INC)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint_all: lint_exe lint_lib

lint_exe:
	@if [ -f "$$SPLINT_EXES" ]; then rm "$$SPLINT_EXES"; fi
	@echo '*** splint: lint_exe ***' > $(SPLINT_EXES)
	@for f in $(EXES) ; do \
	if [ -f $$f ]; then \
	splint $$f.c $(INC) -warnposix -nullderef -nullpass -unrecog \
	>> $(SPLINT_EXES); \
	fi; done

lint_lib: 
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

