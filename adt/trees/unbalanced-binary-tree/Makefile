EXE = tree
INC = -I../../include
LFLAG = -L../../lib 

CFLAG = -std=c99 -D_XOPEN_SOURCE=600 \
	        -D_DEFAULT_SOURCE \
		-pedantic \
		-Wall \
		-W \
		-Wmissing-prototypes \
		-Wno-sign-compare \
		-Wno-unused-parameter

all : ${EXE}

tree: tree.o 
	$(CC) -o $@ tree.o $(LFLAG)

%.o: %.c tree.h
	$(CC) $(CFLAG) -c $< $(INC)

clean : 
	${RM} ${EXE} *.o

showall :
	@ echo ${EXE}
