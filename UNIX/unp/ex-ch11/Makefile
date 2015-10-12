CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L.. -lunp 
INC = -I../lib

# lint output file
SPLINT = lint.out

SRCS =  hostent-ex-11.1.c hostent-ex-11.2.c daytimetcpcli1-ex-11.3.c daytimetcp.c dgcliaddr-ex-11.6.c daytimetcpcli1-ex-11.8.c daytimeudpsrv2-ex-11.9.c udpcli.c
OBJS = $(SRCS:.c=.o)
EXES =  hostent-ex-11.1 hostent-ex-11.2 daytimetcpcli1-ex-11.3  udpcli02  daytimetcpcli1-ex-11.8 daytimeudpsrv2-ex-11.9 udpcli

.PHONY: all clean 
all: $(EXES) lint

daytimeudpsrv2-ex-11.9: daytimeudpsrv2-ex-11.9.o
	$(CC) -o $@ daytimeudpsrv2-ex-11.9.o $(LFLAG)

daytimetcpcli1-ex-11.8: daytimetcpcli1-ex-11.8.o
	$(CC) -o $@ daytimetcpcli1-ex-11.8.o $(LFLAG)

udpcli02: udpcli02.o dgcliaddr-ex-11.6.o
		${CC} -o $@ udpcli02.o dgcliaddr-ex-11.6.o ${LFLAG}

daytimetcpcli1-ex-11.3: daytimetcpcli1-ex-11.3.o
	$(CC) -o $@ daytimetcpcli1-ex-11.3.o $(LFLAG)

hostent-ex-11.2: hostent-ex-11.2.o
	$(CC) -o $@ hostent-ex-11.2.o $(LFLAG)

hostent-ex-11.1: hostent-ex-11.1.o
	$(CC) -o $@ hostent-ex-11.1.o $(LFLAG)


udpcli: udpcli.o  dgcliaddr-ex-11.9.o
	$(CC) -o $@  dgcliaddr-ex-11.9.o udpcli.o $(LFLAG)

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
	splint $$f $(INC) -warnposix -nullderef -nullpass -unrecog -paramuse \
	>> $(SPLINT); \
	fi; done

clean:
	@for f in *.o $(EXES) $(SPLINT); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

