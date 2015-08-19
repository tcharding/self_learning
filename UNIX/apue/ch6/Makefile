yCC = gcc
CFLAG = -Wall -pedantic -std=c99 
LFLAG = -L../lib -lapue 
INC = -I../include 
EXES = spent utsname time

all: $(EXES)

spent: spent.o
	$(CC) -o $@ spent.o $(LFLAG)

utsname: utsname.o
	$(CC) -o $@ utsname.o $(LFLAG)

time: time.o
	$(CC) -o $@ time.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in *.o $(EXES); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

