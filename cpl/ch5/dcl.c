#include "dcl.h"

/* prototypes */
void dcl(void);
void dirdcl(void);

/* convert declaration to words */
int main(void)
{
	while (gettoken() != EOF) {	 /* 1st token on line */
		strcpy(datatype, token); /* is the data type */
		out[0] = '\0';
		dcl();		/* parse rest of line */
		if (tokentype != '\n')
			fprintf(stderr, "syntax error\n");
		printf("%s: %s %s\n", name, out, datatype);
	}
	return 0;
}

/* dcl: parse a declarator */
void dcl(void)
{
	int ns;

	for (ns = 0; gettoken() == '*'; ) /* count 0's */
		ns++;
	dirdcl();
	while (ns-- > 0)
		strcat(out, " pointer to");
}

/* dirdcl: parse a direct declarator */
void dirdcl(void)
{
	int type;

	if (tokentype == '(') {	/* ( dcl ) */
		dcl();
		if (tokentype != ')')
			fprintf(stderr, "error: missing )\n");
	} else if (tokentype == NAME) /* variable name */
		strcpy(name, token);
	else
		fprintf(stderr, "error: expected name or (dcl)\n");
	while ( (type = gettoken()) == PARENS || type == BRACKETS)
		if (type == PARENS)
			strcat(out, " function returning");
		else {
			strcat(out, " array");
			strcat(out, token);
			strcat(out, " of");
		}
}
