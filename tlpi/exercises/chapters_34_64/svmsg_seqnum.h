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

#ifndef SVMSG_SEQNUM_H
#define SVMSG_SEQNUM_H

#include <sys/types.h>
#include <sys/msg.h>
#include <sys/stat.h>
#include <stddef.h>                     /* For definition of offsetof() */
#include <limits.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/wait.h>
#include "tlpi_hdr.h"

#define SERVER_KEY 0x1aaaaaa1           /* Key for server's message queue */

struct requestMsg {                     /* Requests (client to server) */
	long mtype;                         /* Unused */
	int  clientId;                      /* ID of client's message queue */
	int seqLen;
};
/* REQ_MSG_SIZE computes size of 'mtext' part of 'requestMsg' structure.
   We use offsetof() to handle the possibility that there are padding
   bytes between the 'clientId' and 'seqLen' fields. */

#define REQ_MSG_SIZE (offsetof(struct requestMsg, seqLen) - \
                      offsetof(struct requestMsg, clientId) + sizeof(int))

#define RESP_MSG_SIZE 8192

struct responseMsg {                    /* Responses (server to client) */
	long mtype;                         /* One of RESP_MT_* values below */
	long seqNum;
};

#endif				/* SVMSG_SEQNUM_H */
