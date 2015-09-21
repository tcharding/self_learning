#include "tch.h"
#include "buffer.h"
#include "restart.h"
#include <pthread.h>

/* consumer thread function */
void *th_consumer(void *args)
{
	(void)args;		/* quiet lint */
	buffer_t item;
	int retval;
	
	while ((retval = getitem(&item)) == 0) {
		if (copyfile(item.infd, item.outfd) == -1)
			err_quit("copyfile error");
		fprintf(stdout, "copy done: %s\n", item.file);
	}
	if (retval == -1) {
		err_msg("getitem error");
		pthread_exit(NULL);
	}
	pthread_exit(NULLy);
}
