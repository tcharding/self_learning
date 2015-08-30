#include "tch.h"
#include "vector.h"
#include "data.h"
#include "string.h"
#include <ctype.h>

/* parse_config: parse config file putting transformation's into v */
int parse_config(const char *file, vec_t *v)
{
	char *rdline, *ptr;	
	FILE *stream;
	size_t len;
	ssize_t read;
	int lineno;
	char c, *t;		/* character and transformation string */
	data_t *dp;
	
	rdline = NULL, len = 0;
	lineno = 0;
	if ((stream = fopen(file, "r")) == NULL)
		err_sys("fopen failed with %s", file);
	
	while ((read = getline(&rdline, &len, stream)) != -1) {
		lineno++;
		if (*rdline == '#') {
			continue; /* skip comment lines */
		}
		ptr = rdline;
		if (*(ptr + strlen(ptr) - 1) == '\n') /* remove newline */
			*(ptr + strlen(ptr) - 1) = '\0';

		while (isspace(*ptr))	/* skip whitespace */
			ptr++;	
		if (*ptr == '\0') { /* skip blank lines */
			continue; 
		}
		c = *ptr++;
		while (isspace(*ptr))	/* skip whitespace */
			++ptr;	
		t = ptr;
		if (*t == '\0') {
			err_msg("config read error an line: %d\n", lineno);
			free(rdline);
			return -1;
		}
				/* data_t to hold transformation */
		if ((dp = adt_creat(c, t)) == NULL) 
			err_quit("adt_creat error");
		if (v_add(v, dp) == -1)
			err_sys("v_add error");
	}
	
	if (ferror(stream)) 
		err_msg("get_trans: stream error");
	if (fclose(stream) != 0)
		err_msg("fclose error");
	free(rdline);
	return v->cnt;
}
