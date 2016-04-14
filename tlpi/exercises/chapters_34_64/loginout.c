#define _GNU_SOURCE
#include <time.h>
#include <utmpx.h>
#include <paths.h>
#include <lastlog.h>
#include <fcntl.h>
#include "tlpi_hdr.h"
#include "ugid_functions.h"

static int updateLastlog(const char *user);
static void myLogin(struct utmpx *ut);
static int myLogout(const char *utline);
static char *userName(void);

static void myLogwtmp(const char *line, const char *name, const char *host);

#define SLEEP 5

/* test myLogin and myLogout */
int
main(int argc, char *argv[])
{
	struct utmpx ut;

	bzero(&ut, sizeof(ut));
	myLogin(&ut);
	sleep(SLEEP);
	myLogout(ut.ut_line);

	myLogwtmp("", "tobin", "eros");
	
	exit(EXIT_SUCCESS);
}

/* myLogin: implement login(3) */
static void myLogin(struct utmpx *ut)
{
	char *utline;
	char *devName;
	char *user;

	user = userName();
	if (user == NULL)
		fatal("userName");

	ut->ut_type = USER_PROCESS;
	ut->ut_pid = getpid();

	utline = ((devName = ttyname(STDIN_FILENO)) != NULL) ? devName :
		((devName = ttyname(STDOUT_FILENO)) != NULL) ? devName :
		((devName = ttyname(STDERR_FILENO)) != NULL) ? devName : "???";
	
	strncpy(ut->ut_line, utline+5, sizeof(ut->ut_line)); /* +5 for /dev/ */
	ut->ut_line[sizeof(ut->ut_line)-1] = '\0'; /* not required by sus3 */
	strncpy(ut->ut_user, user, sizeof(ut->ut_user));
/*	if (time((time_t *) &ut->ut_tv.tv_sec) == -1)
		errExit("time"); 
*/
	setutxent();                        /* Rewind to start of utmp file */
	if (pututxline(ut) == NULL)        /* Write login record to utmp */
		errExit("pututxline");
	updwtmpx(_PATH_WTMP, ut);          /* Append login record to wtmp */

	updateLastlog(user);
}

/* myLogout: implement logout(3) */
static int
myLogout(const char *utline)
{
	struct utmpx ut;

	bzero(&ut, sizeof(ut));
	
	ut.ut_type = DEAD_PROCESS;          /* Required for logout record */
	time((time_t *) &ut.ut_tv.tv_sec);  /* Stamp with logout time */
	strncpy(ut.ut_line, utline, sizeof(ut.ut_line));
	
	setutxent();                        /* Rewind to start of utmp file */
	if (pututxline(&ut) == NULL)        /* Overwrite previous utmp record */
		errExit("pututxline");
	updwtmpx(_PATH_WTMP, &ut);          /* Append logout record to wtmp */

	endutxent();
	exit(EXIT_SUCCESS);
}

/* updateLastlog: add entry to lastlog */
static int
updateLastlog(const char *user)
{
	struct lastlog llog;
	int fd;
	uid_t uid;
	int nwritten;
	
	bzero(&llog, sizeof(llog));
	
	fd = open(_PATH_LASTLOG, O_RDWR);
	if (fd == -1)
		errExit("lastlog");

	uid = userIdFromName(user);
	if (uid == -1)
		fatal("userIdFromName: %s\n", user);
    
	llog.ll_time = time(NULL);
	strncpy(llog.ll_line, ttyname(STDIN_FILENO), UT_NAMESIZE);
	llog.ll_line[UT_NAMESIZE-1] = '\0';

	if (lseek(fd, uid * sizeof(struct lastlog), SEEK_SET) == -1)
		errExit("lseek");

	if ((nwritten = write(fd, &llog, sizeof(struct lastlog))) <= 0)
		errExit("can't write lastlog");

	return nwritten;
}

static char *
userName(void)
{
	static char buf[10];
	
	strcpy(buf, "tobin");
	return buf;
}

static void
myLogwtmp(const char *line, const char *name, const char *host)
{
	struct utmp ut;

	bzero(&ut, sizeof(ut));
	strncpy(ut.ut_line, line, sizeof(ut.ut_line));
	strncpy(ut.ut_user, name, sizeof(ut.ut_user));
	strncpy(ut.ut_host, host, sizeof(ut.ut_host));
	ut.ut_pid = getpid();
	ut.ut_time = time(NULL);
	updwtmp(_PATH_WTMP, &ut);
}
