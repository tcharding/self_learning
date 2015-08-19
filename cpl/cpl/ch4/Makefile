CC = gcc
CFLAG = -Wall -pedantic -std=c99
LFLAG = -lm

SRC = calc.c getop.c math.c stack.c getch.c
OBJ = $(SRC:.c=.o)

.PHONY: clean

calc: calc.h $(OBJ)
	$(CC) $(CFLAG) -o calc $(OBJ) $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< 

clean:
	@for f in *.o calc; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
