ROOT=..
PLATFORM=$(shell $(ROOT)/systype.sh)
include $(ROOT)/Make.defines.$(PLATFORM)

LIBMISC	= libapue_db.a
COMM_OBJ = db.o

ifeq "$(PLATFORM)" "solaris"
  LDCMD=$(LD) -64 -G -Bdynamic -R/lib/64:/usr/ucblib/sparcv9 -o libapue_db.so.1 -L/lib/64 -L/usr/ucblib/sparcv9 -L$(ROOT)/lib -lapue db.o
  EXTRALD=-m64 -R.
else
  LDCMD=$(CC) -shared -Wl,-dylib -o libapue_db.so.1 -L$(ROOT)/lib -lapue -lc db.o
endif
ifeq "$(PLATFORM)" "linux"
  EXTRALD=-Wl,-rpath=.
endif
ifeq "$(PLATFORM)" "freebsd"
  EXTRALD=-R.
endif
ifeq "$(PLATFORM)" "macos"
  EXTRALD=-R.
endif

all: libapue_db.so.1 test_db $(LIBMISC)

libapue_db.a:	$(COMM_OBJ) $(LIBAPUE)
		$(AR) rsv $(LIBMISC) $(COMM_OBJ)
		$(RANLIB) $(LIBMISC)

libapue_db.so.1: db.c $(LIBAPUE)
		$(CC) -fPIC $(CFLAGS) -c db.c
		$(LDCMD)
		ln -s libapue_db.so.1 libapue_db.so

test_db:	$(LIBAPUE)
		$(CC) $(CFLAGS) -c -I. test_db.c
		$(CC) $(EXTRALD) -o test_db test_db.o -L$(ROOT)/lib -L. -lapue_db -lapue

clean:
	rm -f *.o a.out core temp.* $(LIBMISC) test_db libapue_db.so.* *.dat *.idx libapue_db.so

include $(ROOT)/Make.libapue.inc
