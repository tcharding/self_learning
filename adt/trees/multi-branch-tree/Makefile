EXE = test_node
INC = -I../../include
LFLAG = -L../../lib -ltlpi

CFLAG = -std=c99 -D_XOPEN_SOURCE=600 \
	        -D_DEFAULT_SOURCE \
		-pedantic \
		-Wall \
		-W \
		-Wmissing-prototypes \
		-Wno-sign-compare \
		-Wno-unused-parameter

all : ${EXE}

test_node: test_node.o node.o
	$(CC) -o $@ test_node.o node.o $(LFLAG)

%.o: %.c node.h
	$(CC) $(CFLAG) -c $< $(INC)

clean : 
	${RM} ${EXE} *.o

showall :
	@ echo ${EXE}
