#include "apue.h"
#include <pthread.h>

struct foo {
	int a, b, c, d;
};
static void printfoo(const char *s, const struct foo *fp);
static void *thr_fn1(void *arg);
static void *thr_fn2(void *arg);
static void printids(const char *s);

int main(void)
{
	int err;
	pthread_t tid1, tid2;
	struct foo *fp;

	bzero(&tid1, sizeof(pthread_t));
	bzero(&tid2, sizeof(pthread_t));
	bzero(&fp, sizeof(struct foo **));
	err = pthread_create(&tid1, NULL, thr_fn1, NULL);
	if (err != 0)
		err_exit(err, "failed to create thread 1");
	err = pthread_join(tid1, (void *)&fp);
	if (err != 0)
		err_exit(err, "failed to join with thread 1");
	(void)sleep(1);
	printf("parent starting second thread\n");
	err = pthread_create(&tid2, NULL, thr_fn2, NULL);
	if (err != 0)
		err_exit(err, "failed to create thread 2");
	(void)sleep(1);
	printids("parent:  ");
	printfoo("parent:\n", fp);
	free(fp);
	exit(0);
}

static void printfoo(const char *s, const struct foo *fp)
{
	printf("%s", s);
	printf(" structure at 0x%lx\n", (unsigned long)fp);
	printf(" foo.a = %d\n", fp->a);
	printf(" foo.b = %d\n", fp->b);
	printf(" foo.c = %d\n", fp->c);
	printf(" foo.d = %d\n", fp->d);
}

static void *thr_fn1(void *arg)
{
	struct foo *fp;

	fp = Malloc(sizeof(struct foo));
	fp->a = 1;
	fp->b = 2;
	fp->c = 3;
	fp->d = 4;

	printids("thread 1:");
	printfoo("thread 1:\n", fp);
	pthread_exit((void *)fp);
}

static void *thr_fn2(void *arg)
{
	printids("thread 2:");
	pthread_exit((void *)0);
}

static void printids(const char *s)
{
	pthread_t tid;

	tid = pthread_self();
	fprintf(stderr, "%s tid %lu (0x%lx)\n", s,
	       (unsigned long)tid, (unsigned long)tid);
}
