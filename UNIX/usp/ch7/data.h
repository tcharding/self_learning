#ifndef TCH_DATA_H
#define TCH_DATA_H

/* Abstract Data Type */

typedef struct {
	char c;
	char *s;
} data_t;

data_t *adt_alloc(void);
data_t *adt_creat(char c, const char *s);
data_t *adt_dup(data_t *d);
void adt_free(data_t *d);

char *adt_tostring(data_t *d);
void adt_err_sys(data_t *d, char *fmt, ...);
void adt_err_msg(data_t *d, char *fmt, ...);

int adt_pprint(data_t *d);
#endif	/* TCH_DATA_H */
