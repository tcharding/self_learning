#define _DEFAULT_SOURCE
#include "socket.h"
/*
 * hostent-mod.c - modifications to complete ch 11 exercises
 */
int main(int argc, char *argv[])
{
	char *ptr, **pptr;
	char str[INET_ADDRSTRLEN];
	struct hostent *hptr, *resp;

	
	while (--argc > 0) {
		ptr = *++argv;
		if ( (hptr = gethostbyname(ptr)) == NULL) {
			err_msg("gethostbyname error for host: %s: %s",
				ptr, hstrerror(h_errno));
			continue;
		}
		printf("official hostname: %s\n", hptr->h_name);

		for (pptr = hptr->h_aliases; *pptr != NULL; pptr++)
			printf("\talias: %s\n", *pptr);

		switch (hptr->h_addrtype) {
		case AF_INET:
			pptr = hptr->h_addr_list;
			for ( ; *pptr != NULL; pptr++) {
				Inet_ntop(hptr->h_addrtype,
					  *pptr, str, sizeof(str));
				printf("\taddress: %s\n", str);
				resp = gethostbyaddr(*pptr,
						     sizeof(struct in_addr),
						     AF_INET);
				if (resp == NULL) {
					err_msg("no PTR record");
					continue;
				}
				printf("h_name: %s\n", resp->h_name);
				
			}
			break;
		default:
			err_ret("unknown address type");
			break;
		}
	}
	exit(0);
}
