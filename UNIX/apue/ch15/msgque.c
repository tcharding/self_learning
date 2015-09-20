#include "apue.h"
#include <sys/msg.h>
#include <sys/ipc.h>

#define NUM 5

struct local_msgbuf {
	long mtype;       /* message type, must be > 0 */
	char mtext[1];    /* message data */
};

/* Exercise 15.12 */
int main(void)
{
	int i;
	int key;
	struct local_msgbuf msg;

	for (i = 0; i < NUM; i++) {
		key = msgget(IPC_PRIVATE, 0);
		fprintf(stderr, "key: %d\n", key);
		if (msgctl(key, IPC_RMID, NULL) < 0)
			err_msg("msgctl error");
	}
	msg.mtype = 1;		/* ? */
	msg.mtext[0] = 'a';
	
	for (i = 0; i < NUM; i++) {
		key = msgget(IPC_PRIVATE, 0600);
		fprintf(stderr, "2key: %d\n", key);
		if (msgsnd(key, &msg, 1, 0) < 0)
			err_sys("msgsnd failed\n");
	}
	return 0;
}
