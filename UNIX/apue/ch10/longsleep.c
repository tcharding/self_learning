#include "apue.h"
#define LOOPS_PER_LOG 5
#define SLEEP 60

/* infinite sleep, log time every 5 minutes */
int main(void)
{
	char *file = "/var/log/longsleep.log";
	FILE *stream;
	int i;
	struct tm *tm;
	time_t t;

	stream = Fopen(file, "w");

	for ( ; ; ) {
		for (i = 0; i < LOOPS_PER_LOG; i++) {
			(void)sleep(SLEEP); /* 1 minute */
		}
		t = time(NULL);
		tm = localtime(&t);
		fprintf(stream, "%d\n", tm->tm_sec);
		(void)fflush(stream);
	}
}
