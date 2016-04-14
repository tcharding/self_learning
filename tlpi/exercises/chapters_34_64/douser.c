/* Exercise 38.2 */
#define _GNU_SOURCE
#include <sys/wait.h>
#include <termios.h>
#include <pwd.h>
#include <shadow.h>
#include "tlpi_hdr.h"

static Boolean isAuthorized(const char *user, uid_t euidOrig);
static int getPass(char *buf, int bufsiz);

/* This program is set-UID-root */

int
main(int argc, char *argv[]) {
	char **cmd;
	char *user;
	uid_t euidOrig;
	struct passwd *pwd;
	uid_t uid;
	
	cmd = NULL;
	user = NULL;
	
				/* temporarily drop privileges */
	euidOrig = geteuid();
	if (seteuid(getuid()) == -1)
		errExit("seteuid");
	
	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s [-u user] cmd arg ...\n", argv[0]);

	if (argv[1][0] == '-') {
		if (strcmp(argv[1], "-u") == 0) {
			user = argv[2];
			cmd = &argv[3];
		} else {
			fprintf(stderr, "unknown option: %s\n", argv[1]);
			exit(EXIT_FAILURE);
		}
	} else {
		cmd = &argv[1];
		user = "root";
	}

	printf("Authorization required for user: %s\n", user);
	if (!isAuthorized(user, euidOrig))
		fatal("Authorization failed\n");


	if (strcmp(user, "root") == 0) {
				/* regain privileges */
		if (seteuid(euidOrig) == -1)
			errExit("failed to regain privileges");	
	} else {
		pwd = getpwnam(user);
		if (pwd == NULL)
			fatal("could not get password record");
		uid = pwd->pw_uid;

				/* regain privileges */
		if (seteuid(euidOrig) == -1)
			errExit("failed to regain privileges");
				/* permanently set privileges to user */
		if (setresuid(uid, uid, uid) == -1)
			errExit("seteuid user uid");
	}

	switch (fork()) {
	case -1:
		errExit("fork");
	case 0:			/* child */
		execvp(*cmd, cmd);
		errExit("exec failed");
	default:
				/* permanently drop privileges */
		uid = getuid();
		if (setresuid(uid, uid, uid) == -1)
			errExit("failed to drop privileges");
		
		wait(NULL);
	}

	exit(EXIT_SUCCESS);
}

#define PASS_SIZE 64

/* isAuthorized: prompt for and check password for user */
static Boolean isAuthorized(const char *user, uid_t euidOrig)
{
	char *encrypted;
	char *p;
	struct passwd *pwd;
	struct spwd *spwd;
	char pass[PASS_SIZE]; 
	int err;

	pwd = getpwnam(user);
	if (pwd == NULL)
		fatal("could not get password record");

	/* regain privileges */
	if (seteuid(euidOrig) == -1)
		errExit("failed to regain privileges");

	spwd = getspnam(user);
	if (spwd != NULL)
		pwd->pw_passwd = spwd->sp_pwdp;

	/* temporarily drop privileges */
	if (seteuid(getuid()) == -1)
		errExit("seteuid");

	
	err = getPass(pass, PASS_SIZE);
	if (err)
		fatal("getPass: %d", err);

	encrypted = crypt(pass, pwd->pw_passwd);
	for (p = pass; *p != '\0'; p++)
		*p = '\0';

	if (encrypted == NULL)
		errExit("crypt");

	return (strcmp(encrypted, pwd->pw_passwd) == 0);
}

/* getPass: read password from terminal */
static int
getPass(char *buf, int bufsiz)
{
    struct termios oflags, nflags;

    /* disabling echo */
    tcgetattr(fileno(stdin), &oflags);
    nflags = oflags;
    nflags.c_lflag &= ~ECHO;
    nflags.c_lflag |= ECHONL;

    if (tcsetattr(fileno(stdin), TCSANOW, &nflags) != 0) {
        perror("tcsetattr");
	return 1;
    }

    printf("password: ");
    if (fgets(buf, bufsiz, stdin) == NULL)
	    return 2;
    if (buf[strlen(buf)-1] == '\n')
	    buf[strlen(buf)-1] = '\0';

    /* restore terminal */
    if (tcsetattr(fileno(stdin), TCSANOW, &oflags) != 0) {
        perror("tcsetattr");
	return 3;
    }

    return 0;
}

