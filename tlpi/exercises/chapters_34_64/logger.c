/* Exercise 37.1 */
#include <syslog.h>
#include "tlpi_hdr.h"

#define FACILITY LOG_LOCAL0

static void usage(void);
static int intLevel(const char *s);
static void logMsg(const char *msg, int level);

int
main(int argc, char *argv[]) {
	char *msg;
	int level;

	level = 0;
	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usage();

	if (argc == 2) {
		msg = argv[1];
	} else if (argc == 4) {
		if (strcmp(argv[1], "-l") != 0)
			usage();
		level = intLevel(argv[2]);
		if (level == -1)
			errExit("intLevel");
		msg = argv[3];
	} else {
		usage();
	}

	logMsg(msg, level);

	exit(EXIT_SUCCESS);
}

/* usage: error message and exit */
static void usage(void)
{
	usageErr("logger [-l level] msg\n");
}

/* intLevel: return integer level from string */
static int
intLevel(const char *s)
{
	if (s == NULL)
		return 0;

	if (strcmp(s, "emerg") == 0)
		return LOG_EMERG;
	if (strcmp(s, "alert") == 0)
		return LOG_ALERT;
	if (strcmp(s, "crit") == 0)
		return LOG_CRIT;
	if (strcmp(s, "err") == 0)
		return LOG_ERR;

	return -1;
}

/* logMsg: write msg to syslog */
static void
logMsg(const char *msg, int level)
{
	int priority = 0;

	priority |= FACILITY;
	priority |= level;

	syslog(priority, "%s", msg);
}
