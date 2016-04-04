/* Exercise 9.3 -  must be run as root (or use set-UID-root) */
#define _GNU_SOURCE
#include <unistd.h>
#include <sys/fsuid.h>
#include <limits.h>
#include <grp.h>
#include <pwd.h>
#include "ugid_functions.h"   /* userNameFromId() & groupNameFromId() */
#include "tlpi_hdr.h"

#define SG_SIZE (NGROUPS_MAX + 1)

void show_ucreds(void);
void show_gcreds(void);
void run_and_show(int err, const char *fnName);
int restoreSuppGroups(void);
int my_initgroups(const char *user, gid_t group);
Boolean is_member(const char *user, char **members);
void print_groups(int ngroups, gid_t groups[]);

int
main(int argc, char *argv[])
{
	gid_t suppGroupsBefore[SG_SIZE], suppGroupsAfter[SG_SIZE];
	int numGroupsBefore, numGroupsAfter;
	int err;
	
	numGroupsBefore = getgroups(SG_SIZE, suppGroupsBefore);
	if (numGroupsBefore == -1)
		errExit("getgroups");

	err = setgroups(0, NULL);
	if (err == -1)
		errExit("setgroups: clear");
	
	numGroupsAfter = getgroups(SG_SIZE, suppGroupsAfter);
	if (numGroupsAfter == -1)
		errExit("getgroups");

	if (numGroupsAfter != 0)
		errExit("clear of groups failed");

	err = restoreSuppGroups();
	if (err == -1)
		errExit("restoreSuppGroups");

	numGroupsAfter = getgroups(SG_SIZE, suppGroupsAfter);
	if (numGroupsAfter == -1)
		errExit("getgroups");

	if (numGroupsAfter != numGroupsBefore) {
		errMsg("restore of groups failed");
		print_groups(numGroupsBefore,  suppGroupsBefore);
		print_groups(numGroupsAfter, suppGroupsAfter);
	}
	
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

#define MAX_USER_NAME 32

/* tests [my_]initgroups */
int restoreSuppGroups(void)
{
	struct passwd *pwd;
	gid_t gid;
	char buf[MAX_USER_NAME];
	int err;
	
	pwd = getpwuid(getuid());
	gid = pwd->pw_gid;
	strncpy(buf, pwd->pw_name, MAX_USER_NAME);
	if (strlen(pwd->pw_name) >= MAX_USER_NAME)
		buf[MAX_USER_NAME-1] = '\0'; /* Golang anyone? */

/*	err = initgroups(buf, gid); */
	err = my_initgroups(buf, gid);
	if (err == -1) {
		errMsg("my_initgroups failed");
		return -1;
	}
	return 0;
}

int
my_initgroups(const char *user, gid_t group)
{
	int numGroups;
	gid_t groups[SG_SIZE];
	struct group *grp;
	int err;

	numGroups = 0;
	while ((grp = getgrent()) != NULL) {
		if (is_member(user, grp->gr_mem)) {
			groups[numGroups] = grp->gr_gid;
			numGroups++;
		}
	}
	endgrent();

	err = setgroups(numGroups, groups);
	if (err == -1) {
		errMsg("setgroup");
		return -1;
	}
	return 0;
}

Boolean
is_member(const char *user, char **members)
{
	char **ptr;

	for (ptr = members; *ptr != NULL; ptr++)
		if (strcmp(*ptr, user) == 0)
			return TRUE;

	return FALSE;
}

void print_groups(int ngroups, gid_t groups[])
{
	int i;

	printf("Number of groups: %d\n", ngroups);
	for (i = 0; i < ngroups; i++) {
		printf("%d ", (int) groups[i]);
	}
	printf("\n");
}
