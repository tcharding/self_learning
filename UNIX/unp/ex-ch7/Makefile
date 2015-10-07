CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L.. -lunp 
INC = -I../lib

# lint output file
SPLINT = lint.out

SRCS = buff-opts.c daytimetcpcli-ex-7.2.c
OBJS = $(SRCS:.c=.o)
EXES = buff-opts daytimetcpcli-ex-7.2

.PHONY: all clean 
all: $(EXES) lint

daytimetcpcli-ex-7.2: daytimetcpcli-ex-7.2.o
	$(CC) -o $@ daytimetcpcli-ex-7.2.o $(LFLAG)

buff-opts: buff-opts.o
	$(CC) -o $@ buff-opts.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@echo '*** splint: lint.out ***' > $(SPLINT)
	@for f in $(SRCS) ; do \
	if [ -f $$f ]; then \
	splint $$f $(INC) -warnposix -nullderef -nullpass -unrecog -paramuse \
	>> $(SPLINT); \
	fi; done

clean:
	@for f in *.o $(EXES) $(SPLINT); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

