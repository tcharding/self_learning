EXE = sharedlibs
INC = -I../../lib
LFLAG = -L../.. -ltlpi -ldl

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

sharedlibs: main.o 
	$(CC) -o $@ main.o libone.so libtwo.so $(LFLAG)

%.o: %.c 
	$(CC) $(CFLAG) -c $< $(INC)

clean : 
	${RM} ${EXE} *.o

showall :
	@ echo ${EXE}
