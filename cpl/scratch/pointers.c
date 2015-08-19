#include <stdio.h>

int main()
{
	char *vector[] = {"first", "second", "third"};
	char **v = vector;
	
	printf("char *vector[] = {\"first\", \"second\", \"third\"}\n");
	printf("char **v = vector\n");
	puts("");
	
	/* pre-fix and postfix increment pointer then fetch what p points to */
	printf("*v: %s\n", *v);
	printf("*++v: %s\n", *++v);
	printf("*v: %s\n", *v);
	printf("*v++: %s\n", *v++);
	printf("*v: %s\n", *v);

	puts("");

	/* pre-fix and postfix increment what p points to */
	printf("*v: %s\n", *v);
	printf("++*v: %s\n", ++*v);
	printf("*v: %s\n", *v);
	printf("(*v)++: %s\n", (*v)++); /* ++ has higher precedence than '*' */
	printf("*v: %s\n", *v);
	
	return 0;
}
