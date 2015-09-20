CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L../lib -lapue -pthread
INC = -I../include 
SPLINT_LIBS = lint.out.libs
SPLINT_EXES = lint.out.exes
# source files without a main() function
LIBS = 
OBJS = $(LIBS:.c=.o)
# executables
EXES = cp_to_pager popen coprocess add2stdio add2 select-close-write \
	poll-close-write select-close-read poll-close-read msgque \
	counter-mmapdev0 counter-XSIshmem  counter-reclock counter-POSIXsem

.PHONY: all libs clean lint_all # tests dev

all: $(EXES) lint_all
libs: $(OBJS) lint_libs

counter-mmapdev0: counter-mmapdev0.o
	$(CC) -o $@ counter-mmapdev0.o $(LFLAG)

counter-XSIshmem: counter-XSIshmem.o
	$(CC) -o $@ counter-XSIshmem.o $(LFLAG)

counter-reclock: counter-reclock.o
	$(CC) -o $@ counter-reclock.o $(LFLAG)

counter-XSIsem: counter-XSIsem.o
	$(CC) -o $@ counter-XSIsem.o $(LFLAG)

counter-POSIXsem: counter-POSIXsem.o
	$(CC) -o $@ counter-POSIXsem.o $(LFLAG)

msgque: msgque.o
	$(CC) -o $@ msgque.o $(LFLAG)

poll-close-read: poll-close-read.o
	$(CC) -o $@ poll-close-read.o $(LFLAG)

select-close-read: select-close-read.o
	$(CC) -o $@ select-close-read.o $(LFLAG)

poll-close-write: poll-close-write.o
	$(CC) -o $@ poll-close-write.o $(LFLAG)

select-close-write: select-close-write.o
	$(CC) -o $@ select-close-write.o $(LFLAG)

add2: add2.o
	$(CC) -o $@ add2.o $(LFLAG)

coprocess: coprocess.o
	$(CC) -o $@ coprocess.o $(LFLAG)

popen: popen.o
	$(CC) -o $@ popen.o $(LFLAG)

cp_to_pager: cp_to_pager.o
	$(CC) -o $@ cp_to_pager.o $(LFLAG)

add2stdio: add2stdio.o
	$(CC) -o $@ add2stdio.o $(LFLAG)

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
