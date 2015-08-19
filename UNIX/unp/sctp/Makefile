CC = gcc
CFLAG = -Wall -pedantic -std=c99
LFLAG = -lsock -lsctp
LIB = -L../lib 
INC = -I..

SRC = sctp_wrapper.c # sctp_getnostrm.c sctp_addr_to_associd.c
OBJ = $(SRC:.c=.o)

.PHONY: all clean

all: srv cli 

srv: sctpserv01.o $(OBJ)
	$(CC) -o $@ sctpserv01.o $(OBJ) $(LIB) $(LFLAG)

cli: sctpcli01.o sctp_strcli.o $(OBJ)
	$(CC) -o $@ sctpcli01.o sctp_strcli.o $(OBJ) $(LIB) $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in $(OBJ) cli srv *.o; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done


