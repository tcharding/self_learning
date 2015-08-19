#include "apue.h"
#include <shadow.h>

static void dump_user(char *user);
static void dump_all_user(void);

int main(int argc, char *argv[])
{
	int all = 0;		/* 1 if option '-a' set */
	int c;
	
	while (--argc > 0 && **++argv == '-') {
		while ( (c = *++argv[0])) {
			switch (c) {
			case 'a':
				all = 1;
				break;
			default:
				printf("sort: illegal option %c\n", c);
				argc = 0;
				break;
			}
		}
	}
	if (all)
		dump_all_user();
	else
		dump_user(*argv);
	return 0;
}


/* dump_user: write user name and encrypted password */
static void dump_user(char *user)
{
	struct spwd *ptr;
	
	void setspent();	/* rewind for good measure */
	ptr = getspnam(user);

	fprintf(stderr, "Login Name: %s\tPsswd:%s\n", ptr->sp_namp, ptr->sp_pwdp);
}

/* dump_all_users: write user name and encrypted password (users on system) */
static void dump_all_user(void)
{
	struct spwd *ptr;
	
	void setspent();	/* rewind for good measure */
	while ((ptr = getspent()) != NULL)
		fprintf(stderr, "Login Name: %s\tPsswd:%s\n",
			ptr->sp_namp, ptr->sp_pwdp);

}
