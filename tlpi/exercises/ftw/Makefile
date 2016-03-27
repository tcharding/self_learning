EXE = ftw
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

ftw: ftw.o stack.o
	$(CC) -o $@ ftw.o stack.o $(LFLAG)

%.o: %.c stack.h
	$(CC) $(CFLAG) -c $< $(INC)

clean : 
	${RM} ${EXE} *.o

showall :
	@ echo ${EXE}
