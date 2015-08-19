/* attribution: UNIX Systems Programming - Robbins and Robbins */
/* Program 2.9 */
#include <stdio.h>
#include <stdlib.h>
#include "listlib.h"

/* runproc: execute cmd; store cmd and time in history list */
int runproc(char *cmd)
{
	data_t execute;

	if (time(&(execute.time)) == -1)
		return -1;
	execute.s = cmd;
	if (system(cmd) == -1)	/* could not run cmd */
		return -1;
	return adddata(execute);
}

/* showhistory: output history list of file  */
void showhistory(FILE *f)
{
	data_t data;
	int key;

	key = accessdata();
	if (key == -1) {
		fprintf(stderr, "No history\n");
		return;
	}
	while (!getdata(key, &data) && (data.s != NULL)) {
		fprintf(f, "Command: %s\nTime: %s\n", data.s, ctime(&(data.time)));
		free(data.s);
	}
}
