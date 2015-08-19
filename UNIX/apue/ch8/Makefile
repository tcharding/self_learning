CC = gcc
CFLAG = -Wall -pedantic -std=c99 
LFLAG = -L../lib -lapue 
INC = -I../include 
EXES = vfork stack_smash wait waitid echoarg exec zombie close-on-exec

all: $(EXES)

vfork: vfork.o
	$(CC) -o $@ vfork.o $(LFLAG)

stack_smash: stack_smash.o
	$(CC) -o $@ stack_smash.o $(LFLAG)

wait: wait.o
	$(CC) -o $@ wait.o $(LFLAG)

waitid: waitid.o
	$(CC) -o $@ waitid.o $(LFLAG)

echoarg: echoarg.o
	$(CC) -o $@ echoarg.o $(LFLAG)

exec: exec.o
	$(CC) -o $@ exec.o $(LFLAG)

zombie: zombie.o
	$(CC) -o $@ zombie.o $(LFLAG)

close-on-exec: close-on-exec.o
	$(CC) -o $@ close-on-exec.o $(LFLAG)
 
%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in *.o $(EXES); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

