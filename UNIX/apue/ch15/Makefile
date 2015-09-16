CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L../lib -lapue 
INC = -I../include 
SPLINT_LIBS = lint.out.libs
SPLINT_EXES = lint.out.exes
# source files without a main() function
LIBS = 
OBJS = $(LIBS:.c=.o)
# executables
EXES = cp_to_pager popen coprocess add2stdio add2

.PHONY: all libs clean lint_all # tests dev

all: $(EXES) lint_all
libs: $(OBJS) lint_libs

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

