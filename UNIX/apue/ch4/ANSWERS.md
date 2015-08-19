Chapter 4
=========
1. Using stat instead of stat reports file type of file linked to instead of
   symbolic link, as was expected. see ch4/type.c
2. If mask is set to 777 all permission bits will be masked off, i.e user,
   group, and other will have read/write/execute permissions turned off.
3. term
4. If file foo already exists then `creat("foo", MODE)` does nothing (it does
   not return an error) and permissions are unchanged. see ch4/umask-fun.c
5. We should never see a file size of 0 for a symbolic link since the file size
   is the number of bytes in the file name pointed to (a symbolic link cannot
   point to no where, at least not on Linux). A directory size of zero is
   possible for special directories (e.g /sys).
6. could not complete (Q -> write copy function that does not copy file holes)
7. see create-hole.c for file creation. Check file size with `ls -l` (note block
   count). View file `od -c hole.file`. Then see copy-hole and copy-no-hole.
8. We use `df` in this example because `du` needs to open a file to get its
   size?
9. ulink() changes the changed status time (st_ctime) because it decrements the
   link count in the inode.
10. myftw() recursively calls dopath which opens each subdirectory. Therefore if
    the tree contains more levels than the system maximum for open files then
	the call to opendir will fail and the subtree below this point will not be
    processed. 
11. traverse-chdir.c - speed improvement of approximately 25%
12. Example use case for chroot(); Setting up a new disk a a book disk, chroot
    into it allows usual system commands to execute within the chroot environment.
13. One of the new time values can be left unchanged by passing NULL as the
    argument to utimes().
14. creat() does not update the access time of the parernt directory, therefore
    if new mail arrives in a directory the last access time of the mail file (or
    spool) will differ from (be temporally later) than the access time of the
    enclosing directory.
15. Both tar(1) and cpio(1) store only mtime for each file transferred. Access
    time would be set to the time the transfer takes palce. Modification time
    can be controlled by an option to be either set to the mtime of the source
    file or current time (same as access time). 
16. The depth of a directory tree is limited to the maximum number of i-nodes
    that a system supports (32 bit on current system. see
    /usr/include/bits/stat.h). Calls to getcwd fail once absolute pathname
    exceeds PATH_MAX with errno set to ENAMETOOLONG.
17. Call to create() fails because no process has write permissions on /dev/fd.

