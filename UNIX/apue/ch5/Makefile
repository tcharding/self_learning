CC = gcc
CFLAG = -Wall -pedantic -std=c99 
LFLAG = -L../lib -lapue
INC = -I../include
EXES = cp

all: $(EXES)

cp: cp.o
	$(CC) -o $@ cp.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in *.o $(EXES); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

