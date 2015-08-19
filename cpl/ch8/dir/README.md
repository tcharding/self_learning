dirent.c dir.c fsize.c are taken from the text. Compile errors: cannot locate
difinitions for S_IF* (can compile using __S_IF*, defined in sys/stat.h).

fsize does not produce output, if not linked to dir.o (i.e using stlib
functions for readdir, opendir etc) directory read returns garbage. I can only
assume this is because of the declarations in dirent.h not matching system
library.


