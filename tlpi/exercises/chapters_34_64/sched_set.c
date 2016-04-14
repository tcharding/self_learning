/*************************************************************************\
*                  Copyright (C) Michael Kerrisk, 2015.                   *
*                                                                         *
* This program is free software. You may use, modify, and redistribute it *
* under the terms of the GNU General Public License as published by the   *
* Free Software Foundation, either version 3 or (at your option) any      *
* later version. This program is distributed without any warranty.  See   *
* the file COPYING.gpl-v3 for details.                                    *
\*************************************************************************/

/* Listing 35-2 */

/* sched_set.c

   Usage: sched_set policy priority pid...

   Sets the policy and priority of all process specified by the 'pid' arguments.

   See also sched_view.c.

   The distribution version of this code is slightly different from the code
   shown in the book in order to better fix a bug that was present in the code
   as originally shown in the book. See the erratum for page 743
   (http://man7.org/tlpi/errata/index.html#p_743).
*/
#include <sched.h>
#include <sys/capability.h>
#include "tlpi_hdr.h"

static int dropAllCaps(void);
static int modifyCap(int capability, int setting);
static int raiseCap(int capability);


int
main(int argc, char *argv[])
{
    int j, pol;
    struct sched_param sp;

    if (argc < 3 || strchr("rfo"
#ifdef SCHED_BATCH              /* Linux-specific */
                "b"
#endif
#ifdef SCHED_IDLE               /* Linux-specific */
                "i"
#endif
                , argv[1][0]) == NULL)
        usageErr("%s policy priority [pid...]\n"
                "    policy is 'r' (RR), 'f' (FIFO), "
#ifdef SCHED_BATCH              /* Linux-specific */
                "'b' (BATCH), "
#endif
#ifdef SCHED_IDLE               /* Linux-specific */
                "'i' (IDLE), "
#endif
                "or 'o' (OTHER)\n",
                argv[0]);

    pol = (argv[1][0] == 'r') ? SCHED_RR :
                (argv[1][0] == 'f') ? SCHED_FIFO :
#ifdef SCHED_BATCH              /* Linux-specific, since kernel 2.6.16 */
                (argv[1][0] == 'b') ? SCHED_BATCH :
#endif
#ifdef SCHED_IDLE               /* Linux-specific, since kernel 2.6.23 */
                (argv[1][0] == 'i') ? SCHED_IDLE :
#endif
                SCHED_OTHER;

    sp.sched_priority = getInt(argv[2], 0, "priority");

        /* Only raise CAP_DAC_READ_SEARCH for as long as we need it */

    if (raiseCap(CAP_SYS_NICE))
	    errExit("raiseCap() failed");

    for (j = 3; j < argc; j++)
        if (sched_setscheduler(getLong(argv[j], 0, "pid"), pol, &sp) == -1)
            errExit("sched_setscheduler");

    /* At this point, we won't need any more capabilities,
       so drop all capabilities from all sets */

    if (dropAllCaps() == -1)
        fatal("dropAllCaps() failed");

    exit(EXIT_SUCCESS);
}

static int
modifyCap(int capability, int setting)
{
    cap_t caps;
    cap_value_t capList[1];

    /* Retrieve caller's current capabilities */

    caps = cap_get_proc();
    if (caps == NULL)
        return 1;

    /* Change setting of 'capability' in the effective set of 'caps'. The
       third argument, 1, is the number of items in the array 'capList'. */

    capList[0] = capability;
    if (cap_set_flag(caps, CAP_EFFECTIVE, 1, capList, setting) == -1) {
        cap_free(caps);
        return 2;
    }

    /* Push modified capability sets back to kernel, to change
       caller's capabilities */

    if (cap_set_proc(caps) == -1) {
        cap_free(caps);
        return 3;
    }

    /* Free the structure that was allocated by libcap */

    if (cap_free(caps) == -1)
        return 4;

    return 0;
}

static int              /* Raise capability in caller's effective set */
raiseCap(int capability)
{
    return modifyCap(capability, CAP_SET);
}

/* An analogous dropCap() (unneeded in this program), could be
   defined as: modifyCap(capability, CAP_CLEAR); */

static int              /* Drop all capabilities from all sets */
dropAllCaps(void)
{
    cap_t empty;
    int s;

    empty = cap_init();
    if (empty == NULL)
        return -1;

    s = cap_set_proc(empty);

    if (cap_free(empty) == -1)
        return -1;

    return s;
}
