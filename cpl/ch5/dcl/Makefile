CC = gcc
CFLAG = -Wall -pedantic -std=c99
LFLAG = -lm

#SRC = sort.c readlines.c atof.c
#OBJ = $(SRC:.c=.o)

.PHONY: clean all

all: dcl word_dcl

# declaration converter
DCL= dcl.o gettoken.o getch.o 
dcl: $(DCL) dcl.h
	$(CC) $(CFLAG) -o dcl $(DCL)

# convert word descripton to declarator
WDCL = word_dcl.o gettoken.o getch.o
word_dcl: $(UNDCL) dcl.h
	$(CC) $(CFLAG) -o word_dcl $(UNDCL)

# auxilary rules
%.o: %.c
	$(CC) $(CFLAG) -c $< 

clean:
	@for f in *.o sort word_dcl dcl; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
