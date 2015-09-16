#include "apue.h"

/* test popen with non-existant cmd */
int main(void)
{
	char *cmd = "non-existant";
	FILE *fp;
	char buf[BUFSIZ];
	
	if ((fp = popen(cmd, "r")) == NULL)
		err_sys("popen error");
	if(ferror(fp) != 0)
		err_sys("ferror 1");
	if (fgets(buf, BUFSIZ, fp) == NULL)
		err_msg("fgets returned null");
	if(ferror(fp) != 0)
		err_sys("ferror 2");
	exit(0);
}
	
