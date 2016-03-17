EXE = pstree 
INC = -I../../lib
LFLAG = -L../.. -ltlpi

CFLAG = -std=c99 -D_XOPEN_SOURCE=600 \
	        -D_DEFAULT_SOURCE \
		-g \
		-pedantic \
		-Wall \
		-W \
		-Wmissing-prototypes \
		-Wno-sign-compare \
		-Wno-unused-parameter

all : ${EXE}

#test: node.o
#	$(CC) -o $@ test.o node.o $(LFLAG)

pstree: main.o node.o
	$(CC) -o $@ main.o node.o $(LFLAG)

%.o: %.c pstree.h
	$(CC) $(CFLAG) -c $< $(INC)

clean : 
	${RM} ${EXE} *.o

showall :
	@ echo ${EXE}
