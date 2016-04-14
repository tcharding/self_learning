#include <dlfcn.h>
#include "tlpi_hdr.h"

#define DEFAULT_SLEEP 3

int
main(int argc, char *argv[])
{
	void fn1(void);		/* from one.c */
	void fn2(void);		/* from two.c */
	void *handle1, *handle2;
	int t;
	
	if (argc > 1 && strcmp(argv[1], "--help") == 0)
		usageErr("%s [time]\n", argv[0]);

	if (argc > 1)
		t = getInt(argv[1], GN_NONNEG, "time");
	else
		t = DEFAULT_SLEEP;
	
	setbuf(stdout, NULL);
	
	fn1();
	fn2();
	handle1 = dlopen("libone.so", RTLD_LAZY);
	if (handle1 == NULL)
		fatal("dlopen: %s", dlerror());

	handle2 = dlopen("libtwo.so", RTLD_LAZY);
	if (handle2 == NULL)
		fatal("dlopen: %s", dlerror());

	printf("opened libone and libtwo, sleeping ...\n");
	sleep(t);

	if (dlclose(handle1) == -1)
		errExit("dlclose");
	printf("closed libone, sleeping ...\n");
	sleep(t);

	handle1 = dlopen("libone.so", RTLD_LAZY);
	if (handle1 == NULL)
		fatal("dlopen: %s", dlerror());
	
	if (dlclose(handle2) == -1)
		errExit("dlclose");
	printf("closed libtwo, sleeping ...\n");

	if (dlclose(handle1) == -1)
		printf("%s\n", dlerror());
	if (dlclose(handle1) == -1)
		printf("%s\n", dlerror());
	
	printf("closed libone, sleeping ...\n");
	sleep(t);

/*	
	dlclose(handle1);
	dlclose(handle2);
	printf("closed libone and libtwo, sleeping ...\n");
	sleep(t);
*/	
	exit(EXIT_SUCCESS);
}
