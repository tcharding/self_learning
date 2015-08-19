CC = gcc
CFLAG = -Wall -pedantic -std=c99
LFLAG = -lm

SRC = sort.c readlines.c atof.c
OBJ = $(SRC:.c=.o)

.PHONY: clean all

all: sort dcl # undcl

# sort program
sort: $(OBJ) sort.h
	$(CC) $(CFLAG) -o sort $(OBJ) $(LFLAG)

# declaration converter
DCL= dcl.o gettoken.o getch.o 
dcl: $(DCL) dcl.h
	$(CC) $(CFLAG) -o dcl $(DCL)

# convert word descripton to declarator
#UNDCL = undcl.o gettoken.o getch.o
#undcl: $(UNDCL) dcl.h
#	$(CC) $(CFLAG) -o undcl $(UNDCL)

# auxilary rules
%.o: %.c
	$(CC) $(CFLAG) -c $< 

clean:
	@for f in *.o sort undcl dcl; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
