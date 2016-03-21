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

ch8
---
1. Same username twice because getpwuid() uses a static buffer.

2. getpwnam.c

ch9
---
1. Since running commands in sequence results in all but the first having no
   effect we will assume initial credential state before each command is run.

Initial state of process credentials:
		real=1000 effective=0 saved=0 file-system=0

	State after call:
	a) setuid(2000) - real=2000 effective=2000 saved=2000 file-system=2000
		When a privileged process calls setuid with a non-zero
		argument then all credentials are changed. This change is permanent and
		non-reversible. (See statement at start of question).
		
		
	b) setreuid(-1, 2000) - real=1000 effective=2000 saved=2000 file-system=2000

	c) seteuid(2000) - real=1000 effective=2000 saved=0 file-system=2000

	d) setfsuid(2000) - real=1000 effective=0 saved=0 file-system=2000

	e) setresuid(-1, 2000, 3000) - real=1000 effective=2000 saved=3000 file-system=2000

_see idshow.c for code showing above results_

2. No it is not privileged because privilege is based on effective ID. However
   this process has the option to become privileged (since real-UID is 0). i.e
   this process is an instance of an executable owned by root that is running in
   unprivileged mode.

3. initgroups.c

4. initial state: rID=X eID=Y sID=Y

	a) suspend and resume ID (ignoring errors)
		uid_t saved;

		saved = geteuid();
		seteuid(getuid());
		...
		seteuid(saved);

	b) permanently drop set-user-ID
		uid_t ruid = getuid();
		if (setresuid(-1, ruid, ruid) == -1)
			handleError();

5. initial state: rID=X eID=0 sID=0

	a) suspend and resume ID (ignoring errors)
		uid_t saved;

		saved = geteuid();
		seteuid(getuid());
		...
		seteuid(saved);

	b) permanently drop set-user-ID
	if (setuid(getuidlg()) == -1)
			handleError();

ch10
----
1. Calculations completed on paper.

ch11
----
1. No output.

2. No output.

ch12
----
1. ps.c

2. pstree/main.c

3. pathopen.c

ch13
----
1. No output.

2. No output.

3. fflush(fp) causes all data in the fp buffer to be written to the kernel
   cache. fsync(fp) causes all data in the kernel cache to be written to disk,
   including all metadata (achieves synchronized IO file integrity state).

4. stdio buffering differs for terminals and files so the output varies.

5. tail.c
