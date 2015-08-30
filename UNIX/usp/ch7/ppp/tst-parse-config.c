#include "tch.h"
#include "vector.h"
#include "data.h"

int parse_config(const char *file, vec_t *v);


int main(void)
{
	char *file = "ppp.config";
	vec_t v;

	v_init(&v);
	
	if (parse_config(file, &v) == -1)
		err_sys("parse_config eror");
	/* print vector */
	if (v_foreach(&v, adt_pprint) == -1)
		err_sys("v_foreach error");
	return 0;
}
