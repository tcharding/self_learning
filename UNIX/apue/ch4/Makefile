CC = gcc
CFLAG = -Wall -pedantic -std=c99 
LFLAG = -L../lib -lapue
INC = -I../include
EXES = type umask-fun type-mode copy-hole copy-hole create-hole copy-no-hole \
	traverse traverse-chdir dir-loop

all: $(EXES)

dir-loop: dir-loop.o
	$(CC) -o $@ dir-loop.o $(LFLAG)

traverse: traverse.o
	$(CC) -o $@ traverse.o $(LFLAG)

traverse-chdir: traverse-chdir.o
	$(CC) -o $@ traverse-chdir.o $(LFLAG)

create-hole: create-hole.o
	$(CC) -o $@ create-hole.o $(LFLAG)

copy-hole: copy-hole.o
	$(CC) -o $@ copy-hole.o $(LFLAG)

copy-no-hole: copy-no-hole.o
	$(CC) -o $@ copy-no-hole.o $(LFLAG)

umask-fun: umask-fun.o
	$(CC) -o $@ umask-fun.o $(LFLAG)

type: type.o
	$(CC) -o $@ type.o $(LFLAG)

type-mode: type-mode.o
	$(CC) -o $@ type-mode.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in *.o $(EXES); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
