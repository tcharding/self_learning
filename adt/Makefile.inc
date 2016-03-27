# Makefile.inc - common definitions used by all makefiles

ADT_DIR = ..
ADT_LIB = ${ADT_DIR}/libtlpi.a
ADT_INCL_DIR = ${ADT_DIR}/include

# "-Wextra" is a more descriptive synonym for "-W", but only
# available in more recent gcc versions

IMPL_CFLAGS = -std=c99 -D_XOPEN_SOURCE=600 \
	        -D_DEFAULT_SOURCE \
		-g -I${ADT_INCL_DIR} \
		-pedantic \
		-Wall \
		-W \
		-Wmissing-prototypes \
		-Wno-sign-compare \
		-Wno-unused-parameter

CFLAG = ${IMPL_CFLAGS}
LFLAG = -L${ADT_DIR} -ltlpi
INC = -I${ADT_INCL_DIR}
