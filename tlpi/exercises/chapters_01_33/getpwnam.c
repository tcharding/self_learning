/* Exercise 8.2 */
#include <pwd.h>
#include "tlpi_hdr.h"

struct passwd *my_getpwnam(const char *name);

/* test my_getpwnam */
int
main(void)
{
	char *name = "tobin";
	struct passwd *pwd;
	int libUID, myUID;	/* confirm these are the same */

	pwd = getpwnam(name);
	libUID = pwd->pw_uid;
		
	pwd = my_getpwnam(name);
	myUID = pwd->pw_uid;

	if (libUID != myUID)
		errExit("test failed");

	exit(EXIT_SUCCESS);
}

struct passwd *
my_getpwnam(const char *name)
{
	struct passwd *pwd;

	while((pwd = getpwent()) != NULL) {
		if (!strcmp(pwd->pw_name, name))
			return pwd; /* racy! */
	}
	
	return NULL;
}
