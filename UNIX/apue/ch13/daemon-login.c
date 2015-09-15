#include "apue.h"
#include <sys/syslog.h>

/* Exercise 13.4 */
int main(int argc, char *argv[])
{
	char *user;
	
	daemonize(argv[0]);
	user = getlogin();
	if (user == NULL)
		syslog(LOG_ERR, "We are a daemon, no logged user");
	else
		syslog(LOG_ERR, "User: %s", user);
}
