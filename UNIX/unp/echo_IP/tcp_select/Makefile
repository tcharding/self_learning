CC = gcc
CFLAG = -Wall -pedantic -std=c99
LFLAG = -lsock

INC= -I../..
LIB= -L../../lib
#SRV_OBJ = srv.o str_echo.o 
CLI_OBJ = cli.o str_cli.o


.PHONY: all clean

all: srv-select cli #srv-poll

cli: $(CLI_OBJ)
	$(CC) -o $@ $(CLI_OBJ) $(LFLAG) $(LIB)

srv-select: srv-select.o str_echo.o
	$(CC) -o $@ srv-select.o str_echo.o $(LFLAG) $(LIB)

#srv-poll: srv-poll.o str_echo.o
#	$(CC) -o $@ srv-poll.o str_echo.o $(LFLAG) $(LIB)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in srv-select cli $(CLI_OBJ) $(SRV_OBJ); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
