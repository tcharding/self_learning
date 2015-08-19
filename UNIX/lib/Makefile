CC = gcc						
CFLAG = -Wall -pedantic -std=c99 -fpic
LIB = libtch.a
INC = -I../include

SRC = \
	makeargv.c \
	error.c \

OBJ = $(SRC:.c=.o)

.PHONY: all clean 

all: clean $(LIB)
	@echo
	@echo Compiled library: $(LIB)

$(LIB): ${OBJ}
	ar rcs $(LIB) $(OBJ)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in $(LIB) $(OBJ); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

