CC = gcc						
CFLAG = -Wall -pedantic -std=c99 -fpic -D_GNU_SOURCE 
LIB = libtch.a
INC = -I../include

SRC = \
	buildpath.c \
	pathalloc.c \
	wrappthread.c \
	writen.c \
	readn.c \
	string.c \
	test_framework.c \
	out.c \
	tellwait.c \
	makeargv.c \
	error.c \
	wrapstdio.c \
	wrapunix.c \
	restart.c \
	signal.c \
	signalintr.c 

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

