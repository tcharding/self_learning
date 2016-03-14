Answers to Non-coding Questions
===============================

_No exercises for chapters one and two._

ch3
---
1. The magic numbers used by reboot(2) are the birth dates of Linus and his
three daughters.

ch4
---
1. tee.c
2. copy.c

ch5
---
1. Noop (requires 32 bit system).
2. append.c
3. atomic_append.c
   File opened with O_APPEND causes simultaneous writes to be atomically
   appended to end of file. Therefore size is double that of the run without
   O_APPEND. Without append flag, simultaneous writes clobber each other.
4. dup.c
5. shared_fd.c
6. 
	fd1 = open(file, O_RDWR | O_CREAT | O_TRUNC, S_UIRUSR | S_IWUSR);
	fd2 = dup(fd1);
	fd3 = open(file, O_RDWR);
	write(fd1, "Hello," 6); <!-- 1 -->
	write(fd2, " world" 6);	<!-- 2 -->
    lseek(fd2, 0, SEEK_SET);
	write(fd1, "HELLO," 6); <!-- 3 -->
	write(fd3, "Gidday," 6); <!-- 4 -->

	File contents after each write:
		1. Hello,
		2. Hello, world
		3. HELLO, world
		4. Gidday,world

7. readwritev.c
		
ch6
---
1. The executable does not contain the 10MB array since it is a static
   uninitialized variable i.e it is contained within the bss segment
   (uninitialized data segment).
2. broken_longjmp.c, trying different things leads me to say that longjmp into
   function that has returned results in undefined behaviour, as would be
   expected.
3. env.c

ch7
---
1. free_and_sbrk.c
2. memory.c
