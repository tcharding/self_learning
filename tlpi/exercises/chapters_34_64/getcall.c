/* Exercise 45.3 */
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/stat.h>
#include "tlpi_hdr.h"

/* verify Sys V get* algorithm behaviour */

#define MQ_PERMS S_IRUSR | S_IWUSR | S_IWGRP
#define NLOOPS 5

int
main(int argc, char *argv[]) {
	char *path = "/etc/drirc";
	key_t key;
	int msqid;
	int i;
	
	key = ftok(path, 1);
	if (key == -1)
		errExit("ftok");

	for (i = 0; i < NLOOPS; i++) {
		msqid = msgget(key, IPC_CREAT | MQ_PERMS);
		if (msqid == -1)
			errExit("msgget");
		printf("msqid: %d\n", msqid);

		if (msgctl(msqid, IPC_RMID, NULL) == -1)
			errExit("msgctl rmid");
	}

	exit(EXIT_SUCCESS);
}
