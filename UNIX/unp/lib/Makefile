CC = gcc						
CFLAG = -Wall -pedantic -std=c99 -fpic
LIB	= libsock.a 
INC = -I..

SRC = \
	error.c \
	readn.c \
	readline.c \
	wrapsock.c \
	wrapstdio.c \
	wrapunix.c \
	writen.c \
	signal.c \
	sock_bind_wild.c \
	sock_cmp_addr.c \
	sock_cmp_port.c \
	sock_get_port.c \
	sock_set_addr.c \
	sock_set_port.c \
	sock_set_wild.c \
	host_serv.c \
	tcp_connect.c \
	tcp_listen.c \
	sock_ntop_host.c \
	sock_ntop.c \
	udp_client.c \
	udp_connect.c \
	udp_server.c \

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

arc:

