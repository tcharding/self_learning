CC = gcc
CFLAG = -Wall -pedantic -D_GNU_SOURCE 
LFLAG = -L../../lib -ltch
INC = -I../../include

USP = parentwritepipe synchronizefan pipeserver pipeclient # text book programs
BR = br_srv br_cli		# barrier client/server
RR = rr_srv rr_cli		# request/reply client/server

.PHONY: all clean book barrier request_reply

all: $(USP) $(BR) $(RR)
book: $(USP)
barrier: $(BR)
request_reply: $(RR)

# barrier client/server
br_srv: br_server.o barrier.o
	$(CC) -o $@ br_server.o barrier.o $(LFLAG)

br_cli: br_fanclient.o barrier.o
	$(CC) -o $@ br_fanclient.o barrier.o $(LFLAG)

barrier.o: barrier.c barrier.h
	$(CC) $(CFLAG) -c $< $(INC)

# request/reply client/server
rr_srv: rr_server.o request_reply.o
	$(CC) -o $@ rr_server.o request_reply.o $(LFLAG)

rr_cli: rr_client.o request_reply.o
	$(CC) -o $@ rr_client.o request_reply.o $(LFLAG)

request_reply: request_reply.o request_reply.h
	$(CC) $(CFLAG) -c $< $(INC)

# text book programs
parentwritepipe: parentwritepipe.o
	$(CC) -o $@ parentwritepipe.o $(LFLAG)

synchronizefan: synchronizefan.o restart.o
	$(CC) -o $@ synchronizefan.o restart.o $(LFLAG)

pipeserver: pipeserver.o restart.o
	$(CC) -o $@ pipeserver.o restart.o $(LFLAG)

pipeclient: pipeclient.o restart.o
	$(CC) -o $@ pipeclient.o restart.o $(LFLAG)


%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in *.o $(USP) $(BR) $(RR) ; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
