Collected Notes and Functions 
=============================

* For final word on Linux/GNU/Linux/liGNUx see section 1.2

* After using standard library io and terminating loop on EOF, check for stream
error with ferror(stream).

* When using printf to print unknow data types (e.g pid_t) cast to the largest
type that it might use, guaranteed by the standard.  
	printf("Pid: %ld\n", (long)getpid());


misc.c
------


