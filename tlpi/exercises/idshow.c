/************************************************************************* \
 *                  Copyright (C) Michael Kerrisk, 2015.                   *
 *                                                                         *
 * This program is free software. You may use, modify, and redistribute it *
 * under the terms of the GNU General Public License as published by the   *
 * Free Software Foundation, either version 3 or (at your option) any      *
 * later version. This program is distributed without any warranty.  See   *
 * the file COPYING.gpl-v3 for details.                                    *
\*************************************************************************/

/* Listing 9-1 */

/* idshow.c

   Display all user and group identifiers associated with a process.

   Note: This program uses Linux-specific calls and the Linux-specific
   file-system user and group IDs.
*/
#define _GNU_SOURCE
#include <unistd.h>
#include <sys/fsuid.h>
#include <limits.h>
#include "ugid_functions.h"   /* userNameFromId() & groupNameFromId() */
#include "tlpi_hdr.h"

#define SG_SIZE (NGROUPS_MAX + 1)

void show_ucreds(void);
void show_gcreds(void);
void run_and_show(int err, const char *fnName);

int
main(int argc, char *argv[])
{
	printf("Initial creds:\n");
	show_ucreds();
	show_gcreds();
	
	/* run the commands in Exercise 9.1 */
/*	run_and_show(setuid(2000), "setuid(2000)"); */
/*	run_and_show(setreuid(-1, 2000), "setreuid(-1, 2000)"); */
/*    	run_and_show(seteuid(2000), "seteuid(2000)"); */
    	run_and_show(setresuid(-1, 2000, 3000), "setresuid(-1,2000, 3000)");

	exit(EXIT_SUCCESS);
}

void show_ucreds(void)
{
	uid_t ruid, euid, suid, fsuid;
	char *p;

	if (getresuid(&ruid, &euid, &suid) == -1)
		errExit("getresuid");

	fsuid = setfsuid(0);
    
	printf("UID: ");
	p = userNameFromId(ruid);
	printf("real=%s (%ld); ", (p == NULL) ? "???" : p, (long) ruid);
	p = userNameFromId(euid);
	printf("eff=%s (%ld); ", (p == NULL) ? "???" : p, (long) euid);
	p = userNameFromId(suid);
	printf("saved=%s (%ld); ", (p == NULL) ? "???" : p, (long) suid);
	p = userNameFromId(fsuid);
	printf("fs=%s (%ld); ", (p == NULL) ? "???" : p, (long) fsuid);
	printf("\n");
	
}

void show_gcreds(void)
{
	gid_t rgid, egid, sgid, fsgid;
	gid_t suppGroups[SG_SIZE];
	int numGroups, j;
	char *p;

	if (getresgid(&rgid, &egid, &sgid) == -1)
		errExit("getresgid");

	fsgid = setfsgid(0);
    
	printf("GID: ");
	p = groupNameFromId(rgid);
	printf("real=%s (%ld); ", (p == NULL) ? "???" : p, (long) rgid);
	p = groupNameFromId(egid);
	printf("eff=%s (%ld); ", (p == NULL) ? "???" : p, (long) egid);
	p = groupNameFromId(sgid);
	printf("saved=%s (%ld); ", (p == NULL) ? "???" : p, (long) sgid);
	p = groupNameFromId(fsgid);
	printf("fs=%s (%ld); ", (p == NULL) ? "???" : p, (long) fsgid);
	printf("\n");

	numGroups = getgroups(SG_SIZE, suppGroups);
	if (numGroups == -1)
		errExit("getgroups");

	printf("Supplementary groups (%d): ", numGroups);
	for (j = 0; j < numGroups; j++) {
		p = groupNameFromId(suppGroups[j]);
		printf("%s (%ld) ", (p == NULL) ? "???" : p, (long) suppGroups[j]);
	}
	printf("\n");
	
}

void run_and_show(int err, const char *fnName)
{
	if (err == -1)
		printf("call failed\n");

	printf("\ncalled: %s\n", fnName);
	show_ucreds();
}
