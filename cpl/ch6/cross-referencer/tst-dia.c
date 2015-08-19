#include "dia.h"
#include <stdio.h>

/* test dia */
int main(void)
{
	struct dia *p;

	p = dia_creat();
	printf("Adding values 1 9 3 1\n");
	dia_add(p, 1);
	dia_add(p, 9);
	dia_add(p, 3);
	dia_add(p, 1);
	dia_print(p);
	return 0;
}
