/*************************************************************************\
 *                  Copyright (C) Michael Kerrisk, 2015.                   *
 *                                                                         *
 * This program is free software. You may use, modify, and redistribute it *
 * under the terms of the GNU General Public License as published by the   *
 * Free Software Foundation, either version 3 or (at your option) any      *
 * later version. This program is distributed without any warranty.  See   *
 * the file COPYING.gpl-v3 for details.                                    *
\*************************************************************************/

/* Listing 46-8 */

/* Modified to complete Exercise 46.4, Tobin Harding */

/* svmsg_file_server.c

   A file server that uses System V message queues to handle client requests
   (see svmsg_file_client.c). The client sends an initial request containing
   the name of the desired file, and the identifier of the message queue to be
   used to send the file contents back to the child. The server attempts to
   open the desired file. If the file cannot be opened, a failure response is
   sent to the client, otherwise the contents of the requested file are sent
   in a series of messages.

   This application makes use of multiple message queues. The server maintains
   a queue (with a well-known key) dedicated to incoming client requests. Each
   client creates its own private queue, which is used to pass response
   messages from the server back to the client.

   This program operates as a concurrent server, forking a new child process to
   handle each client request while the parent waits for further client requests.
*/
#include <sys/ipc.h>
#include <syslog.h>
#include <signal.h>
#include "become_daemon.h"
#include "svmsg_file.h"

static void
handler(int sig)
{
	closelog();
	unlink(SERVER_KEY_FILE);
	signal(sig, SIG_DFL);
	raise(sig);		/* terminates process */
}

static void
sigintHandler(int sig)
{
	;
}

static void             /* SIGCHLD handler */
grimReaper(int sig)
{
	int savedErrno;

	savedErrno = errno;                 /* waitpid() might change 'errno' */
	while (waitpid(-1, NULL, WNOHANG) > 0)
		continue;
	errno = savedErrno;
}

#define TIMEOUT 1

static void             /* Executed in child process: serve a single client */
serveRequest(const struct requestMsg *req)
{
	int fd;
	ssize_t numRead;
	struct responseMsg resp;
	char *ident = "svmsg_file_server (child)";
	int options = LOG_PERROR;
	struct sigaction sa;
	int err;

	openlog(ident, options, LOG_USER);

	sigemptyset(&sa.sa_mask);
	sa.sa_flags = 0;
	sa.sa_handler = sigintHandler;
	if (sigaction(SIGALRM, &sa, NULL) == -1) {
		syslog(LOG_USER, "sigaction");
		exit(EXIT_FAILURE);
	}

	fd = open(req->pathname, O_RDONLY);
	if (fd == -1) {                     /* Open failed: send error text */
		resp.mtype = RESP_MT_FAILURE;
		snprintf(resp.data, sizeof(resp.data), "%s", "Couldn't open");
		msgsnd(req->clientId, &resp, strlen(resp.data) + 1, 0);
		syslog(LOG_USER, "failed to open %s", req->pathname);
		exit(EXIT_FAILURE);             /* and terminate */
	}

	/* Transmit file contents in messages with type RESP_MT_DATA. We don't
	   diagnose read() and msgsnd() errors since we can't notify client. */

	resp.mtype = RESP_MT_DATA;

	while ((numRead = read(fd, resp.data, RESP_MSG_SIZE)) > 0) {
		alarm(TIMEOUT);
		err = msgsnd(req->clientId, &resp, numRead, 0);
		if (err == -1 && errno == EINTR) {
			if (msgctl(req->clientId, IPC_RMID, NULL) == -1) 
				syslog(LOG_USER, "msgctl failed to rmid client msg queue");
			syslog(LOG_USER, "client appears to have gone ... aborting");
		} else {
			break;	/* some other error, let the client clean up */
		}
		alarm(0);
	}
	/* Send a message of type RESP_MT_END to signify end-of-file */

	resp.mtype = RESP_MT_END;
	msgsnd(req->clientId, &resp, 0, 0);         /* Zero-length mtext */
	
	closelog();
}

#define PERMS S_IRUSR | S_IWUSR | S_IRGRP

int
main(int argc, char *argv[])
{
	struct requestMsg req;
	pid_t pid;
	ssize_t msgLen;
	int serverId;
	struct sigaction sa;
	int nwritten;
	int fd;
	char *ident = "svmsg_file_server";
	int options = LOG_PERROR;
	int flags = BD_NO_UMASK0;

	if (becomeDaemon(flags) == -1)
		errExit("failed to daemonize");

	openlog(ident, options, LOG_USER);

	sigemptyset(&sa.sa_mask);
	sa.sa_handler = handler;
	sa.sa_flags = 0;
	if (sigaction(SIGINT, &sa, NULL) == -1 ||
	    sigaction(SIGTERM, &sa, NULL) == -1) {
		syslog(LOG_USER, "sagaction");
		exit(EXIT_FAILURE);
	}

	fd = open(SERVER_KEY_FILE, O_WRONLY | O_CREAT | O_EXCL, PERMS);
	if (fd == -1 && errno == EEXIST) {
		unlink(SERVER_KEY_FILE);
		fd = open(SERVER_KEY_FILE, O_WRONLY | O_CREAT | O_EXCL, PERMS);
	}
	if (fd == -1) {
		syslog(LOG_USER, "failed to open %s", SERVER_KEY_FILE);
		exit(EXIT_FAILURE);
	}

	/* Create server message queue */

	serverId = msgget(IPC_PRIVATE, IPC_CREAT | IPC_EXCL |
			  S_IRUSR | S_IWUSR | S_IWGRP);
	if (serverId == -1) {
		syslog(LOG_USER, "msgget");
		exit(EXIT_FAILURE);
	}

	nwritten = write(fd, &serverId, sizeof(serverId));
	if (nwritten != sizeof(serverId)) {
		syslog(LOG_USER, "write");
		exit(EXIT_FAILURE);
	}

	/* Establish SIGCHLD handler to reap terminated children */

	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_RESTART;
	sa.sa_handler = grimReaper;
	if (sigaction(SIGCHLD, &sa, NULL) == -1) {
		syslog(LOG_USER, "sigaction");
		exit(EXIT_FAILURE);
	}

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
			serveRequest(&req);
			_exit(EXIT_SUCCESS);
		}

		/* Parent loops to receive next client request */
	}

	/* If msgrcv() or fork() fails, remove server MQ and exit */

	if (msgctl(serverId, IPC_RMID, NULL) == -1) {
		syslog(LOG_USER, "msgctl");
		exit(EXIT_FAILURE);
	}

	if (close(fd) == -1) {
		syslog(LOG_USER, "close");
		exit(EXIT_FAILURE);
	}

	exit(EXIT_SUCCESS);
}
