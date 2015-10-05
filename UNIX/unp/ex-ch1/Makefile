CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L.. -lunp 
INC = -I../lib

# lint output file
SPLINT = lint.out

SRCS = daytimetcpcli-ex-1.3.c daytimetcpcli-ex-1.4.c daytimetcpsrv-ex-1.5.c
OBJS = $(SRCS:.c=.o)
EXES = daytimetcpcli-ex-1.3  daytimetcpcli-ex-1.4 daytimetcpsrv-ex-1.5

.PHONY: all clean 
all: $(EXES) lint

daytimetcpsrv-ex-1.5: daytimetcpsrv-ex-1.5.o
	$(CC) -o $@ daytimetcpsrv-ex-1.5.o $(LFLAG)

daytimetcpcli-ex-1.4: daytimetcpcli-ex-1.4.o
	$(CC) -o $@ daytimetcpcli-ex-1.4.o $(LFLAG)

daytimetcpcli-ex-1.3: daytimetcpcli-ex-1.3.o
	$(CC) -o $@ daytimetcpcli-ex-1.3.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@echo '*** splint: lint.out ***' > $(SPLINT)
	@for f in $(SRCS) ; do \
	if [ -f $$f ]; then \
	splint $$f $(INC) -warnposix -nullderef -nullpass -unrecog  >> $(SPLINT); \
	fi; done

clean:
	@for f in *.o $(EXES) $(SPLINT); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

