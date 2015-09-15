#include "apue.h"
#define MILLION 1000000

/* sleep for usec microseconds */
int sleep_us(unsigned usec)
{
	struct timeval tv;

	tv.sec = usec / MILLION;
	tv.usec = usec % MILLION;
	select(0, NULL, NULL, NULL, &tv);
}
