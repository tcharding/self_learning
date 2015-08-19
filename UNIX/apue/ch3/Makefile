CC = gcc
CFLAGS = -Wall -pedantic -std=c99 -D_POSIX_C_SOURCE=200112L

all: dup-fun lseek-fun

dup-fun: dup-fun.c
	$(CC) $(FLAGS) -o dup-fun dup-fun.c

lseek-fun: lseek-fun.c
	$(CC) $(FLAGS) -o lseek-fun lseek-fun.c

clean:
	@for f in *.o dup-fun lseek-fun; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
