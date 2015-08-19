#include "tch.h"
#include <stdio.h>
#include <sys/stat.h>
#define DEBUG 1
#define EXECUTABLE (S_IXUSR | S_IXGRP | S_IXOTH)

/* Macro to print debugging info */
#define PUT(fmt, ...) \
            do { if (DEBUG) fprintf(stderr, fmt, __VA_ARGS__); } while (0)

void try_exe(char *dir, char *exe);
void print_path(char *dir, char *file);
void t_print_path();
int we_ingroup(gid_t gid);

/* which: output first path to executable for input */
int main(int argc, char *argv[])
{
	FILE *fp;
	char *buf;
	size_t size;
	char *dir;		/* directory to look in */
	char *exe;		/* file we are looking for */
	char *delim = ":";
	char *p;

	if (argc != 2)
		err_quit("Usage: %s file", argv[0]);
	exe = argv[1];

	if ((fp = open_memstream(&buf, &size)) == NULL)
		err_sys("open_memstream failed");
	
	if ((p = getenv("PATH")) == NULL)
		err_sys("getenv failed");

	if (fprintf(fp, "%s", p) < 0)
		err_quit("ms_pf failed");
	if (fclose(fp) != 0)
		err_sys("fclose error");
	
	if ((dir = strtok(buf, delim)) != NULL) { 
		do {
			try_exe(dir, exe);
		} while ((dir = strtok(NULL, delim)) != NULL);
	}
	
	return 0;
}

void try_exe(char *dir, char *exe)
{
	struct stat statbuf;

	if (chdir(dir) != 0)
		return;		/* implicit check for 'x' perms on dir */
	if (stat(exe, &statbuf) < 0)   /* implicit check for 'r' perms on dir */
		return;		       /* it's not here */
	if (!S_ISREG(statbuf.st_mode)) /* stat(3) follows symbolic links */
		return;		       /* it's here but not a regular file */

				/* check if process is owner and perms u+x */
	if ((statbuf.st_mode & S_IXUSR) &&
	    (statbuf.st_mode & S_IRUSR) &&
	    (statbuf.st_uid == geteuid())) {
		print_path(dir, exe);
		return;
	}
				/* check if process is in group and perms g+x */
	if ((statbuf.st_mode & S_IXGRP) &&
	    (statbuf.st_mode & S_IRGRP) &&
	    we_ingroup(statbuf.st_gid)) {
		print_path(dir, exe);
		return;
	}
				/* check if perms o+x */
	if (statbuf.st_mode & S_IXOTH)
		print_path(dir, exe);
	
}
		
/* print_path: print path from dir and file */
void print_path(char *dir, char *file)
{
	FILE *fp;
	char *buf;
	size_t size;
	
	if ((fp = open_memstream(&buf, &size)) == NULL)
		err_sys("open_memstream failed");

	fprintf(fp, "%s", dir);
	fflush(fp);

	if (*(buf + strlen(buf) - 1) != '/')
		fprintf(fp, "/");
	fprintf(fp, "%s", file);
	fclose(fp);
	printf("%s\n", buf);
}

/* t_print_path: test print_path */
void t_print_path()
{
	char *dir, *file;

	dir = "/etc";
	file = "gemrc";
	fprintf(stderr, "%s %s", dir, file);
	print_path(dir, file);

	dir = "/etc/";
	file = "gemrc";
	fprintf(stderr, "%s %s", dir, file);
	print_path(dir, file);

	dir = "/";
	file = "no-exist";
	fprintf(stderr, "%s %s", dir, file);
	print_path(dir, file);

}

/* we_ingroup: return true if process is in group gid */
int we_ingroup(gid_t gid)
{
	int setsize;
	gid_t *list;
	int i;
	
	if (gid == getegid())
		return 1;	      /* true */
	setsize = getgroups(0, NULL); /* get setsize */
	if ((list = calloc(setsize, sizeof(gid_t))) == NULL)
		err_sys("malloc failed");
	if (getgroups(setsize, list) == -1)
		err_sys("getgroups failed");

	for (i = 0; i < setsize; i++)
		if (gid == list[i])
			return 1; /* true */
	return 0;		  /* not in group */
}
