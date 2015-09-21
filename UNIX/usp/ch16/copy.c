#include "tch.h"
#include "buffer.h"
#include <pthread.h>
#include <sys/times.h>

void *th_producer(void *args);
void *th_consumer(void *args);
static void pr_times(clock_t real, struct tms *tmsstart, struct tms *tmsend);

int main(int argc, char *argv[])
{
	pthread_t *tids;
	pthread_t tid;		/* producer thread */
	int numthreads, i;
	char *args[2];
	struct tms tmsstart, tmsend;
	clock_t start, end;
	
	if (argc != 4)
		err_quit("Usage: %s n src dst", argv[0]);

	if ((start = times(&tmsstart)) == -1)	/* starting values */
		err_sys("times error");
	
	tid = 0;
	numthreads = atoi(argv[1]);
	args[0] = s_dup(argv[2]);
	args[1] = s_dup(argv[3]);
	
	if ((tids = (pthread_t *)calloc((size_t)numthreads,
					sizeof(pthread_t))) == NULL)
		err_sys("calloc error");
				/* create producer */
	Pthread_create(&tid, NULL, th_producer, (void *)args);
				/* create consumers */
	for (i = 0; i < numthreads; i++) 
		Pthread_create(tids + i, NULL, th_consumer, NULL);

				/* wait for threads to finish */
	Pthread_join(tid, NULL); /* producer */
	for (i = 0; i < numthreads; i++) /* consumers */
		Pthread_join(tids[i], NULL);

	if ((end = times(&tmsend)) == -1)		/* ending values */
		err_sys("times error");

	msg("aoeu");
	pr_times(end-start, &tmsstart, &tmsend);
	return 0;
}


static void pr_times(clock_t real, struct tms *tmsstart, struct tms *tmsend)
{
	static long clktck = 0;

	if (clktck == 0)	/* fetch clock ticks per second first time */
		if ((clktck = sysconf(_SC_CLK_TCK)) < 0)
			err_sys("sysconf error");

	printf("  real:  %7.2f\n", real / (double) clktck);
	printf("  user:  %7.2f\n",
	  (tmsend->tms_utime - tmsstart->tms_utime) / (double) clktck);
	printf("  sys:   %7.2f\n",
	  (tmsend->tms_stime - tmsstart->tms_stime) / (double) clktck);
	/* printf("  child user:  %7.2f\n", */
	/*   (tmsend->tms_cutime - tmsstart->tms_cutime) / (double) clktck); */
	/* printf("  child sys:   %7.2f\n", */
	/*   (tmsend->tms_cstime - tmsstart->tms_cstime) / (double) clktck); */
}
