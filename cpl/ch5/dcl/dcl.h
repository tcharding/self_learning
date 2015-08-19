#ifndef _DCL_H
#define _DCL_H

#include <stdio.h>
#include <string.h>
#include <ctype.h>

enum {	NAME, PARENS, BRACKETS, IDENTIFIER, DCL, DIR_DCL,
	PARAM_TYPE, IDEN_TYPE,  };
enum {
	MAXTOKEN = 100,
	MAXOUT = 1024
};

int gettoken(void);
int tokentype;			/* type of last token */
char token[MAXTOKEN];		/* last token string */
char name[MAXTOKEN];		/* identifier name */
char datatype[MAXTOKEN];	/* data type = char, int, etc. */
char out[MAXOUT];		/* output string */

#endif	/* _DCL_H */
