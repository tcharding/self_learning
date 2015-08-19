CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_POSIX_C_SOURCE=200112L

.PHONY: all clean

all: keeplog getpaths doenv

keeplog: keeplog.o keeploglib.o listlib.o
	$(CC) -o $@ keeplog.o keeploglib.o listlib.o

getpaths: getpaths.o makeargv.o
	$(CC) -o $@ getpaths.o makeargv.o

doenv: doenv.o vector.o
	$(CC) -o $@ doenv.o vector.o

%.o: %.c
	$(CC) $(CFLAG) -c $< 

clean:
	@for f in *.o keeplog getpaths doenv; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
