#include "tch.h"

/* buildpath: return pathname dir/file, must be free'd */
char *buildpath(const char *dir, const char *file)
{
	char *path;
	size_t size, total;

	total = 0;
	path = path_alloc(&size);
	total = strlen(dir) + strlen(file) + 1; /* +1 for '\0' */
	if (*(dir + strlen(dir) - 1) != '/')
		total++;
	if (total > size) {
		errno = ENAMETOOLONG;
		return NULL;
	}
	strcpy(path, dir);
	if (*(dir + strlen(dir) - 1) != '/')
		strcat(path, "/");
	strcat(path, file);
	return path;
}
