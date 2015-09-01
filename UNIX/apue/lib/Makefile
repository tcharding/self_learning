#
# Makefile for misc library.
#
ROOT=..
PLATFORM=$(shell $(ROOT)/systype.sh)
include $(ROOT)/Make.defines.$(PLATFORM)

LIBMISC	= libapue.a
OBJS = error.o pathalloc.o openmax.o mem_stream.o prexit.o msg.o prmask.o \
	wrapunix.o wrapstdio.o tellwait.o signal.o signalintr.o
#OBJS   = bufargs.o cliconn.o clrfl.o \
#			daemonize.o  errorlog.o lockreg.o locktest.o \
#			popen.o prexit.o
#			ptyfork.o ptyopen.o readn.o recvfd.o senderr.o sendfd.o \
#			servaccept.o servlisten.o setfd.o setfl.o  \
#			sleepus.o spipe.o ttymodes.o writen.o

all:	$(LIBMISC) #sleep.o

$(LIBMISC):	$(OBJS)
	$(AR) rv $(LIBMISC) $?
	$(RANLIB) $(LIBMISC)


clean:
	rm -f *.o a.out core temp.* $(LIBMISC)

include $(ROOT)/Make.libapue.inc
