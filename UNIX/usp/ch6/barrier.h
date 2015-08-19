#ifndef _BARRIER_H
#define _BARRIER_H

int waitbarrier(char *name);
int br_pipenames(const char *name, char **request, char **release); 
int br_createpipes(const char *request, const char *release); 
int br_rmpipes(const char *request, const char *release); 

#endif	/* _BARRIER_H */
