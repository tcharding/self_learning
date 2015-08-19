CC = gcc
CFLAG = -Wall -pedantic -std=c99
LFLAG = -lm

SRC = cat.c fopen.c fclose.c flushbuf.c fillbuf.c
OBJ = $(SRC:.c=.o)
EXE = cat

.PHONY: clean

cat: stdio.h $(OBJ)
	$(CC) $(CFLAG) -o $(EXE) $(OBJ) $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< 

clean:
	@for f in *.o $(EXE); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
