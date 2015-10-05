CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L.. -lunp 
INC = -I../lib

# lint output file
SPLINT = lint.out

SRCS =  daytimetcpcli-ex-4.2.c daytimetcpsrv1-ex-4.4.c daytimetcpsrv1-ex-4.5.c
OBJS = $(SRCS:.c=.o)
EXES =  daytimetcpcli-ex-4.2 daytimetcpsrv1-ex-4.4 daytimetcpsrv1-ex-4.5

.PHONY: all clean 
all: $(EXES) lint

daytimetcpsrv1-ex-4.5: daytimetcpsrv1-ex-4.5.o
	$(CC) -o $@ daytimetcpsrv1-ex-4.5.o $(LFLAG)

daytimetcpsrv1-ex-4.4: daytimetcpsrv1-ex-4.4.o
	$(CC) -o $@ daytimetcpsrv1-ex-4.4.o $(LFLAG)

daytimetcpcli-ex-4.2: daytimetcpcli-ex-4.2.o
	$(CC) -o $@ daytimetcpcli-ex-4.2.o $(LFLAG)

#foo: foo.o
#	$(CC) -o $@ foo.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@echo '*** splint: lint.out ***' > $(SPLINT)
	@for f in $(SRCS) ; do \
	if [ -f $$f ]; then \
	splint $$f $(INC) -warnposix -nullderef -nullpass -paramuse -unrecog  \
	>> $(SPLINT); \
	fi; done

clean:
	@for f in *.o $(EXES) $(SPLINT); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

