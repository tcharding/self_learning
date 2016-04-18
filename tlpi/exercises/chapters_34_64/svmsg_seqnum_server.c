/*************************************************************************\
 *                  Copyright (C) Michael Kerrisk, 2015.                   *
 *                                                                         *
 * This program is free software. You may use, modify, and redistribute it *
 * under the terms of the GNU General Public License as published by the   *
 * Free Software Foundation, either version 3 or (at your option) any      *
 * later version. This program is distributed without any warranty.  See   *
 * the file COPYING.gpl-v3 for details.                                    *
\*************************************************************************/

/* Modified to complete Exercise 46.2, Tobin Harding */
#include "svmsg_seqnum.h"

static void             /* SIGCHLD handler */
grimReaper(int sig)
{
	int savedErrno;

	savedErrno = errno;                 /* waitpid() might change 'errno' */
	while (waitpid(-1, NULL, WNOHANG) > 0)
		continue;
	errno = savedErrno;
}

static void             /* Executed in child process: serve a single client */
serveRequest(const struct requestMsg *req, int seqNum)
{
	struct responseMsg resp;

	resp.mtype = 1;
	resp.seqNum = seqNum;
	
	msgsnd(req->clientId, &resp, sizeof(int), 0);         /* Zero-length mtext */
}

int
main(int argc, char *argv[])
{
	struct requestMsg req;
	pid_t pid;
	ssize_t msgLen;
	int serverId;
	int fd;
	int nread;
	int seqNum;
	struct sigaction sa;
	char *file = "/tmp/seqnum.txt";
	
	/* Create server message queue */

	serverId = msgget(SERVER_KEY, IPC_CREAT |
			  S_IRUSR | S_IWUSR | S_IWGRP);

	if (serverId == -1)
		errExit("msgget");

	/* Establish SIGCHLD handler to reap terminated children */

	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_RESTART;
	sa.sa_handler = grimReaper;
	if (sigaction(SIGCHLD, &sa, NULL) == -1)
		errExit("sigaction");

	fd = open(file, O_RDWR | O_DSYNC,  S_IRUSR | S_IWUSR);
	if (fd != -1) {
		nread = read(fd, &seqNum, sizeof(int));
		if (nread == -1)
			errExit("read");
	} else if (fd == -1 && errno == ENOENT) {
		seqNum = 0;
		fd = open(file, O_RDWR | O_CREAT | O_DSYNC, S_IRUSR | S_IWUSR);
	}

	/* catch error on either open's */
	if (fd == -1)
		errExit("open");
	
	/* Read requests, handle each in a separate child process */

	for (;;) {
		msgLen = msgrcv(serverId, &req, REQ_MSG_SIZE, 0, 0);
		if (msgLen == -1) {
			if (errno == EINTR)         /* Interrupted by SIGCHLD handler? */
				continue;               /* ... then restart msgrcv() */
			errMsg("msgrcv");           /* Some other error */
			break;                      /* ... so terminate loop */
		}

		pid = fork();                   /* Create child process */
		if (pid == -1) {
			errMsg("fork");
			break;
		}

		if (pid == 0) {                 /* Child handles request */
			serveRequest(&req, seqNum);
			_exit(EXIT_SUCCESS);
		}

		seqNum += req.seqLen;
		/* Parent loops to receive next client request */
	}

	/* If msgrcv() or fork() fails, remove server MQ and exit */

	if (msgctl(serverId, IPC_RMID, NULL) == -1)
		errExit("msgctl");
	exit(EXIT_SUCCESS);
}

