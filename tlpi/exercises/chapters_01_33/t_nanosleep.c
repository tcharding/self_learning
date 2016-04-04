/*************************************************************************\
*                  Copyright (C) Michael Kerrisk, 2015.                   *
*                                                                         *
* This program is free software. You may use, modify, and redistribute it *
* under the terms of the GNU General Public License as published by the   *
* Free Software Foundation, either version 3 or (at your option) any      *
* later version. This program is distributed without any warranty.  See   *
* the file COPYING.gpl-v3 for details.                                    *
\*************************************************************************/

/* Listing 23-3 */

/* Modified to use clock_gettime(): Exercise 23.2, Tobin Harding. */

/* t_nanosleep.c

   Demonstrate the use of nanosleep() to sleep for an interval
   specified in nanoseconds.

   See also t_clock_nanosleep.c.
*/
#define _POSIX_C_SOURCE 199309
#include <sys/time.h>
#include <time.h>
#include <signal.h>
#include "tlpi_hdr.h"

static void
sigintHandler(int sig)
{
	return;                     /* Just interrupt nanosleep() */
}

int
main(int argc, char *argv[])
{
	struct timeval start, finish;
	struct timespec request;
	struct sigaction sa;
	int s;

	if (argc != 3 || strcmp(argv[1], "--help") == 0)
		usageErr("%s secs nanosecs\n", argv[0]);

	if (clock_gettime(CLOCK_REALTIME, &request) == -1)
		errExit("clock_gettime");
	request.tv_sec += getLong(argv[1], 0, "secs");
	request.tv_nsec += getLong(argv[2], 0, "nanosecs");
    
	/* Allow SIGINT handler to interrupt nanosleep() */

	sigemptyset(&sa.sa_mask);
	sa.sa_flags = 0;
	sa.sa_handler = sigintHandler;
	if (sigaction(SIGINT, &sa, NULL) == -1)
		errExit("sigaction");

	if (gettimeofday(&start, NULL) == -1)
		errExit("gettimeofday");
    
	for (;;) {
		s = clock_nanosleep(CLOCK_REALTIME, TIMER_ABSTIME, &request, NULL);
		if (s == -1 && errno != EINTR)
			errExit("nanosleep");

		if (gettimeofday(&finish, NULL) == -1)
			errExit("gettimeofday");
		printf("Slept for: %9.6f secs\n", finish.tv_sec - start.tv_sec +
		       (finish.tv_usec - start.tv_usec) / 1000000.0);

		if (s == 0)
			break;                      /* clock_nanosleep() completed */
	}

	printf("Sleep complete\n");
	exit(EXIT_SUCCESS);
}
