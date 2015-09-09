CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L../lib -lapue -lpthread
INC = -I../include 
SPLINT_LIB = lint.out.libs
SPLINT_EXES = lint.out.exes
SPLINT_DEV = lint.out.dev

# source files without a main() function
LIBS = wrappthread.c barrier.c
OBJS = $(LIBS:.c=.o)
# executables
EXES = return workque thread_ids tst-barrier pthread_barrier
DEV = tst-barrier

.PHONY: all exes libs clean lint_all # tests dev

dev: $(DEV) lint_dev
all: exes libs
libs: $(OBJS) lint_libs
exes: $(EXES) lint_exes

pthread_barrier: wrappthread.o pthread_barrier.o
	$(CC) -o $@ wrappthread.o pthread_barrier.o $(LFLAG)

tst-barrier: wrappthread.o barrier.o tst-barrier.o
	$(CC) -o $@ wrappthread.o barrier.o tst-barrier.o $(LFLAG)

workque: wrappthread.o workque.o 
	$(CC) -o $@ wrappthread.o workque.o $(LFLAG)

thread_ids: thread_ids.o
	$(CC) -o $@ thread_ids.o $(LFLAG)

return: return.o
	$(CC) -o $@ return.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint_all: lint_exe lint_lib

lint_dev:
	@if [ -f "$$SPLINT_DEV" ]; then rm "$$SPLINT_DEV"; fi
	@echo '*** splint: lint_exe ***' > $(SPLINT_DEV)
	@for f in $(DEV) ; do \
	if [ -f $$f ]; then \
	splint $$f.c $(INC) -warnposix -nullderef -nullpass -unrecog \
	-paramuse -noret \
	>> $(SPLINT_DEV); \
	fi; done

lint_exes:
	@if [ -f "$$SPLINT_EXES" ]; then rm "$$SPLINT_EXES"; fi
	@echo '*** splint: lint_dev ***' > $(SPLINT_EXES)
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
	@for f in *.o $(EXES) $(SPLINT_LIBS) $(SPLINT_EXES) $(SPLINT_DEV); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

