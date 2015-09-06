#include "tch.h"

void write_tspec(struct timespec *tp)
{
	fprintf(stderr, "[%d:%d]", (int)tp->tv_sec, (int)tp->tv_nsec);
}
