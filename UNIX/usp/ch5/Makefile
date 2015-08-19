CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
INC = -I../../include
LFLAG = -L../../lib -ltch
EXES = which biff du

.PHONY: all clean 

all: $(EXES)

which: which.o 
	$(CC) -o $@ which.o $(LFLAG)

biff: biff.o
	$(CC) -o $@ biff.o $(LFLAG)

du: du.o stack.o
	$(CC) -o $@ du.o stack.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in *.o $(EXES); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

