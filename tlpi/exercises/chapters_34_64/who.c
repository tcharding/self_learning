/* Exercise 40.4 */
#include <time.h>
#include <utmpx.h>
#include "tlpi_hdr.h"

#define LOGIN_NAME_MAX_DEFAULT 256
/* implement who(1) */
int
main(int argc, char *argv[]) {
	char *login;
	int lnmax;
	struct utmpx *ut;
	time_t time;

	lnmax = sysconf(_SC_LOGIN_NAME_MAX);
	if (lnmax == -1)
		lnmax = LOGIN_NAME_MAX_DEFAULT;

	login = malloc(lnmax);
	if (login == NULL)
		errExit("malloc");

	setutxent();

	while ((ut = getutxent()) != NULL) {
		if ((strstr(ut->ut_line, "tty") != NULL) &&
		    strcmp(ut->ut_user, "LOGIN")) {
			time = ut->ut_tv.tv_sec;
		printf("%s\t%s\t%s", ut->ut_user, ut->ut_line, ctime(&time));
		}
	}

	exit(EXIT_SUCCESS);
}
