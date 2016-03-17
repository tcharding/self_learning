/* Exercise 12.2 */
#include <pwd.h>
#include <dirent.h>
#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "tlpi_hdr.h"
#include "pstree.h"

#define MAX_TOKEN 64

struct listNode {
	struct node *n;
	struct listNode *next;
};

/* print a complete process tree (includes all running processes) */

static struct node *buildProcTree(void);

static Boolean isPidDir(const char *name);
static void doPidDir(struct listNode **list, const char *psName);
static struct node *listToTree(struct listNode *list);

static ssize_t buildPsPath(char *buf, const char *name);
static FILE *statusFileFromPsName(const char *name);
static Boolean lineStartsWith(const char *line, const char *startsWith);
static int extractToList(struct listNode **list, const char *ppidLine,
			 const char *pidLine, const char *cmdLine);

static char *getToken2(char *buf, const char *line);


int main(void)
{
	struct node *root;

	/*
	root = buildTestTree();
	printTree(root);
	freeTree(root);
	*/
	
/*	testNode();
	testListToTree();
	exit(EXIT_SUCCESS);
*/
	root = buildProcTree();
	if (root == NULL)
		errExit("buildProcTree");
	
	printTree(root);
	freeTree(root);

	exit(EXIT_SUCCESS);
}

/* buildProcTree: parse /proc/ and build process tree */
static struct node *
buildProcTree(void)
{
	DIR * dirp;
	struct dirent *dp;
	struct listNode *list = NULL;
	char *procDir = "/proc";

	dirp = opendir(procDir);
	if (dirp == NULL)
		errExit("Error opening %s", procDir);

	for (;;) {
		errno = 0;
		dp = readdir(dirp);
		if (dp == NULL)
			break;

		if (strcmp(dp->d_name, ".") == 0 || strcmp(dp->d_name, "..") == 0)
			continue;

		if (isPidDir(dp->d_name))
			doPidDir(&list, dp->d_name);
	}
/*	printSiblings(list); */

	return listToTree(list);
}



/* isPidDir: true if dir name is all digits */
static Boolean
isPidDir(const char *name)
{
	char path[PATH_MAX];
	struct stat sb;
	
	if (buildPsPath(path, name) == -1)
		return FALSE;
				/* check is dir (and present) */
	if (stat(path, &sb) == -1)
		return FALSE;
	if (!S_ISDIR(sb.st_mode))
		return FALSE;
				/* check is all digits */
	for ( ;*name != '\0'; ++name)
		if (!isdigit(*name))
			return FALSE;
	return TRUE;
}

/* psDoDir: process a /proc/PID directory */
static void
doPidDir(struct listNode **list, const char *psName)
{
	FILE *status;
	char line[255];
	char *pidLine, *ppidLine, *cmdLine;
	int err;
	
	status = statusFileFromPsName(psName);
	if (status == NULL)
		return;		/* process may have exited */
	
	while (fgets(line, sizeof(line), status)) {
		if (lineStartsWith(line, "PPid:")) {
			ppidLine = alloca(strlen(line+1));
			if (ppidLine == NULL)
				errExit("alloca");
			strcpy(ppidLine, line);
		}
		if (lineStartsWith(line, "Pid:")) {
			pidLine = alloca(strlen(line+1));
			if (pidLine == NULL)
				errExit("alloca");
			strcpy(pidLine, line);
		}
			
		if (lineStartsWith(line, "Name:")) {
			cmdLine = alloca(strlen(line+1));
			if (cmdLine == NULL)
				errExit("alloca");
			strcpy(cmdLine, line);
		}
	}
	err = extractToList(list, ppidLine, pidLine, cmdLine);
	if (err)
		errExit("extractAndAddProc:\n %s %s %s",
			ppidLine, pidLine, cmdLine);
}

/* listToTree: convert an unsorted list of nodes into a tree */
static struct node *
listToTree(struct listNode *list)
{
	struct node *root;
	struct listNode *unadded;
	struct listNode *current, *previous;

	root = node(0, 0, "Root Process Node");
	unadded = list;
	current = unadded;
	previous = current;
	
	for (;;) {
		if (unadded == NULL)
			break;

		if (canAdd(root, current->n)) {
			if (addChild(&root, current->n))
				errExit("addChild");

			if (previous == current) {
				current = current->next;
				previous = current;
				unadded = current;
			} else {
				current = current->next;
				previous->next = current;
			}
		} else {
			current = current->next;
		}
		if (current == NULL)
			current = unadded;
	}

	return root;
}

/* psBuildPath: builds path /proc/PID in buf using name */
static ssize_t
buildPsPath(char *buf, const char *name)
{
	char *proc = "/proc/";
	ssize_t len;
	
	if (name == NULL || *name == '\0')
		return -1;

	len = strlen(proc) + strlen(name) + 1;
	if (len > PATH_MAX)
		return -1;
	
	strcpy(buf, proc);
	strcpy(buf+strlen(proc), name);
	buf[len-1] = '\0';

	return len-1;
}

/* open stream for file /pid/<name>/status */
static FILE *
statusFileFromPsName(const char *name)
{
	char buf[PATH_MAX];
	ssize_t len;

	len = buildPsPath(buf, name); /* /proc/PID */
	if (len == -1)
		return NULL;

	strcpy(buf+len, "/status");
	return fopen(buf, "r");
}

/* lineStartsWith: true if first characters of line are startsWith */
static Boolean
lineStartsWith(const char *line, const char *startsWith)
{
	const char *ptr;

	for (ptr = startsWith; *ptr != '\0'; ptr++, line++) {
		if (*ptr != *line)
			return FALSE;
	}
	return TRUE;
}

/* extractAndAddProc: extract details from strings and add process to root
    return non-null on error */
static int
extractToList(struct listNode **list, const char *ppidLine,
			     const char *pidLine, const char *cmdLine)
{
	pid_t ppid, pid;
	char token[MAX_TOKEN];
	struct listNode *new;

	new = malloc(sizeof(struct listNode));
	if (new == NULL)
		errExit("malloc");

	getToken2(token, ppidLine);
	ppid = (pid_t) strtol(token, NULL, 0);

	getToken2(token, pidLine);
	pid = (pid_t) strtol(token, NULL, 0);

	getToken2(token, cmdLine);

	new->n = node(ppid, pid, token);
	if (*list == NULL) {
		*list = new;
	} else {
		struct listNode *ptr = *list;
		while (ptr->next != 0)
			ptr = ptr->next;
		ptr->next = new;
	}

	return 0;
}

/* getToken2: puts second token (word) of line in buf
   line is tab separated */
static char *
getToken2(char *buf, const char *line)
{
	char *cpy;
	char *token;

	if (line == NULL || *line == '\0')
		return NULL;

	cpy = alloca(strlen(line)+1);
	strcpy(cpy, line);

	token = strtok (cpy, "\t");
	token = strtok (NULL, " ");
	strcpy(buf, token);
	
	return buf;
}

/* static int */
/* listLength(struct listNode *list) */
/* { */
/* 	int num = 0; */

/* 	for ( ;list != NULL; list = list->next) */
/* 		num++; */

/* 	return num; */
/* } */
/* c */
/* static void */
/* testListToTree(void) */
/* { */
/* 	struct node *list, *root, *ptr; */
	
/* 	list = node((pid_t) 0, (pid_t) 1, "zsh"); */
/* 	root = listToTree(list); */
/* 	printTree(root); */
/* 	freeTree(root); */

	
/* 	list = node((pid_t) 0, (pid_t) 1, "init"); */
/* 	ptr = list; */
/* 	list->next = node((pid_t) 1, (pid_t) 11, "win"); */

/* 	root = listToTree(list); */
/* 	printTree(root); */
	
/* /\* */
/* 	list = buildTestList(); */
/* 	root = listToTree(list); */
/* 	printTree(root); */
/* *\/ */
/* } */

/* static void */
/* debugLists(const char *msg, struct node *unadded, */
/* 			struct node *current, struct node *previous) */
/* { */
/* 	fprintf(stderr, "%s ( u, c, p)\n %d \t %d \t %d\n", msg, */
/* 		listLength(unadded), listLength(current), listLength(previous)); */
/* } */


/* static struct */
/* node *buildTestList() */
/* { */
/* 	struct node *list, *ptr; */
	
/* 	list = node((pid_t) 0, (pid_t) 1, "root process"); */
/* 	ptr = list; */
	
/* 	ptr->next = node((pid_t) 1, (pid_t) 11, "win"); */
/* 	ptr = ptr->next; */
/* 	ptr->next = node((pid_t) 1, (pid_t) 12, "loose"); */
/* 	ptr = ptr->next; */
/* 	ptr->next = node((pid_t) 1, (pid_t) 13, "draw"); */
/* 	ptr = ptr->next; */

/* 	ptr->next = node((pid_t) 11, (pid_t) 111, "profit"); */
/* 	ptr = ptr->next; */
/* 	ptr->next = node((pid_t) 11, (pid_t) 112, "zsh"); */
/* 	ptr = ptr->next; */
/* 	ptr->next = node((pid_t) 11, (pid_t) 113, "zsh"); */
/* 	ptr = ptr->next; */

/* 	ptr->next = node((pid_t) 13, (pid_t) 131, "zsh"); */
/* 	ptr = ptr->next; */
/* 	ptr->next = node((pid_t) 13, (pid_t) 132, "zsh"); */
/* 	ptr = ptr->next; */

/* 	return list; */
/* } */
