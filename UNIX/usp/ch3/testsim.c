#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
	int m;			/* time */
	int k;			/* loop */
	int i;
	
	if (argc != 3) {
		fprintf(stderr, "Usage: %s time loop\n", argv[0]);
		return 1;
	}
	m = atoi(*++argv);
	k = atoi(*++argv);

	for (i = 0; i < k; i++) {
		sleep(m);
		fprintf(stderr, "pid:%ld\n", (long)getpid());
	}
	return 0;
}
