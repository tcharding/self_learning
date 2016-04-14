/* Exercise 40.1 */
#include <utmpx.h>
#include "tlpi_hdr.h"

static char *myGetlogin(void);


/* test implementation of getlogin */
int
main(int argc, char *argv[]) {
	char *login;
	char *exp = "tobin";

	setbuf(stdin, NULL);
	
	login = myGetlogin();
	if (login == NULL) {
		printf("Failed to get login name\n");
		exit(EXIT_FAILURE);
	}
	if (strcmp(login, exp) != 0) {
		printf("Error: getlogin failed: %s\n", login);
		exit(EXIT_FAILURE);
	}
	printf("login: %s\n", login);
	exit(EXIT_SUCCESS);
}

#define LOGIN_NAME_MAX_DEFAULT 256

/* myGetlogin: implement getlogin(3) */
static char *
myGetlogin(void)
{
	char *login;
	int lnmax;
	char *tname;
	struct utmpx *ut;

	lnmax = sysconf(_SC_LOGIN_NAME_MAX);
	if (lnmax == -1)
		lnmax = LOGIN_NAME_MAX_DEFAULT;

	login = malloc(lnmax);
	if (login == NULL)
		errExit("malloc");

	tname = ttyname(STDIN_FILENO);
	if (tname == NULL)
		errExit("ttyname");
	printf("tname: %s\n", tname);

	setutxent();

	while ((ut = getutxent()) != NULL) {
		printf("ut_line: %s\n", ut->ut_line);
		if (strcmp(ut->ut_line, "tty1") == 0) {
			strncpy(login, ut->ut_user, lnmax);
			login[lnmax-1] = '\0';
			return login;
		}
	}
	return NULL;
}
