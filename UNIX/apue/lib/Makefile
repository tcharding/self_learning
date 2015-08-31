#
# Makefile for misc library.
#
ROOT=..
PLATFORM=$(shell $(ROOT)/systype.sh)
include $(ROOT)/Make.defines.$(PLATFORM)

LIBMISC	= libapue.a
OBJS = error.o pathalloc.o openmax.o mem_stream.o prexit.o msg.o prmask.o \
	wrapunix.o wrapstdio.o
#OBJS   = bufargs.o cliconn.o clrfl.o \
#			daemonize.o error.o errorlog.o lockreg.o locktest.o \
#			popen.o prexit.o prmask.o \
#			ptyfork.o ptyopen.o readn.o recvfd.o senderr.o sendfd.o \
#			servaccept.o servlisten.o setfd.o setfl.o signal.o signalintr.o \
#			sleepus.o spipe.o tellwait.o ttymodes.o writen.o

all:	$(LIBMISC) #sleep.o

$(LIBMISC):	$(OBJS)
	$(AR) rv $(LIBMISC) $?
	$(RANLIB) $(LIBMISC)


clean:
	rm -f *.o a.out core temp.* $(LIBMISC)

include $(ROOT)/Make.libapue.inc
