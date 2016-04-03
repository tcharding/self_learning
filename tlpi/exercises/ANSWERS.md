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

ch14
----
1. manyfiles.c

ch15
----
1. No output.

2. Stat() should not update file times (atime, mtime, or ctime) since it does
   not modify the inode or the file.

3. Failed: could not get nanosecond timestamps from stat. Tried:

    printf("Last file access:         sec:%ld usec:%ld\n",
		(long) sb->st_atime, (long) sb->st_atime->st_atime_nsec);

4. access.c

5. 	umask(mask = umask(0));

6. readX.c

7. chattr.c

ch16
----
1. setfattr.c

ch17
----
1. getacl.c

ch18
----
1. gcc unlinks the executable. This removes the file name entry from the
   directory and reduces the link count by one. The kernel will not remove the
   inode though because it still has an open file descriptor, held by the
   process running the executable.

   The newly compiled executable has a new inode, it is unrelated to the
   original except that the directory entry happens to contain the same filename.

2. chmod follows links so the call tries to set the permissions of 'myfile' to
   S_IRUSR, however the process already has this file open with write
   permissions so the call fails.

3. realpath.c

4. list_files.c

5. getcwd.c

6. ../dirs_links/nftw_dir_tree -d

7. filetypes.c

8. ftw.c

9. We would expect fchdir to be quicker since the call has less system call
   overhead than chdir since the dir is already open there is no need to stat()
   it first. See multi_chdir.c

ch19
----
1. notify.c

ch20
----
1. sig_receiver.c

2. sig_ignore.c

3. sig_flags.c

4. sig_interupt.c

ch21
----
1. abort.c

ch22
----
1. sig_stop.c

2. sig_catchall.c

3. sig_speed_sigwaitinfo.c

4. sig_systemV.c

ch23
----
1. alarm.c

2. Sleeps indefinitely since timer rounding up causes timer to increase on every
   signal received. Only occurs when signals are received at a higher frequency
   than jiffies per second. See t_nanosleep.c

3. posix_timer.c

4. ptmr_sigev_signal.c

ch24
----
1. After three consecutive calls to fork() there will be eight processes in
   total, or seven new processes.

2. vfork.c

3. In order to get a core dump of a running process without terminating we could
   fork() and abort() the child.

4. No output.

5. Process synchronization such as this can be achieved by alternating calls to
   kill() and sigsuspend() in the parent and child process.

ch25
----
1. Exit status of -1 will result in parent receiving an exit status of 255. This
   is because only the least significant 8 bits are returned. These are all 1's
   in a twos compliment number system, hence 255.

ch26
----
1. noparent.c

2. My guess: after the parent exits. Answer is correct, see grandparent.c

3. child_status.c

4. make_zombie.c

ch27
----
1. The command fails with error ENOPERM since path is searched in order the
   directory dir1 is searched first. File xyz is found and it does not have
   execute permissions set.

2. execlp.c

3. We would see the file output on stdout (with line numbers). See cat.script
   and exec.c

4. The effect of this code snippet is to create a running in the background i.e
   not attached to the current terminal. With a bit of extra bookkeeping this is
   one way to start a daemon process.

5. No output is produced because of stdio buffering.

6. sig_chld_wait.c

ch28
----
1. forktime.c

ch29
----
1. pthread_join_self.c: pthread_join(pthread_self(), NULL) returns EDEADLOCK
   error. Avoid this condition by code like;

	if (!pthread_equal(tid, pthread_self())
		pthread_join(tid, NULL);

2. The problem is that buf is declared on the stack of main(), after calling
   pthread_exit (in main) this memory address is no longer valid.

ch30
----
1. thread_incr.c

2. See ../../adt/trees/unbalanced-binary-tree/

