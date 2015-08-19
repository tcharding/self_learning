CC = gcc
CFLAG = -Wall -pedantic -std=c99 
LFLAG = -L../lib -lapue 
INC = -I../include 
EXES = session

all: $(EXES)

session: session.o
	$(CC) -o $@ session.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in *.o $(EXES); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

