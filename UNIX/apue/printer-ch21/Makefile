ROOT=..
PLATFORM=$(shell $(ROOT)/systype.sh)
include $(ROOT)/Make.defines.$(PLATFORM)
EXTRA=

ifeq "$(PLATFORM)" "solaris"
  EXTRALIBS=-lsocket -lnsl -lrt -lpthread
else
  EXTRALIBS=-pthread
endif

PROGS = print printd
HDRS = print.h ipp.h

all:	$(PROGS) 

util.o:		util.c $(HDRS)

print.o:	print.c $(HDRS)

printd.o:	printd.c $(HDRS)

print:		print.o util.o connect_retry.o $(LIBAPUE)
		$(CC) $(CFLAGS) -o print print.o util.o connect_retry.o $(LDFLAGS) $(LDDIR) $(LDLIBS)

printd:	printd.o util.o connect_retry.o init_server.o $(LIBAPUE)
	$(CC) $(CFLAGS) -o printd printd.o util.o connect_retry.o init_server.o \
	$(LDFLAGS) $(LDDIR) $(LDLIBS)

clean:
	rm -f $(PROGS) $(TEMPFILES) *.o

include $(ROOT)/Make.libapue.inc
