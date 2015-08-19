CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_POSIX_C_SOURCE=200112L

.PHONY: all clean book

all: chainopenfork chainopenfork_ex cat

chainopenfork: chainopenfork.o
	$(CC) -o $@ chainopenfork.o

chainopenfork_ex: chainopenfork-47.o
	$(CC) -o $@ chainopenfork-47.o

cat: cat.o restart.o
	$(CC) -o $@ cat.o restart.o 

%.o: %.c
	$(CC) $(CFLAG) -c $< 

clean:
	@for f in *.o chainopenfork cat chainopenfork_ex; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
