#include "unp.h"

#define INET6_ADDRLEN 16	/* nbytes in IPv6 address */

enum ENDIAN { BIG, LITTLE };

static int endian(void);
static void ip_map_addr(void *dst, struct in_addr *addr);

/* convert presentation format to network byte order */
int inet_pton_loose(int af, const char *src, void *dst)
{
	int retval;
	
	switch (af) {
	case AF_INET:
		if ((retval = inet_pton(af , src, dst)) != 0)
			return retval;
		else
			return inet_aton(src, dst);
		break;
	case AF_INET6:
		if ((retval = inet_pton(af , src, dst)) != 0)
			return retval;
		else {
			struct in_addr addr;
			retval = inet_aton(src, &addr);
			if (retval == 0)
				return 0; /* fail */
			else {
				ip_map_addr(dst, &addr);
				return 1; /* success */
			}
		}
		break;
	default:
		return inet_pton(af, src, dst);
	}
}


/* map addr to dst in network byte order, addr is in network byte order */
static void ip_map_addr(void *dst, struct in_addr *addr)
{
	char *ptr;
        char fill[] = "0xFFFF";

	ptr = (char *)dst;	/* needed for pointer arithmetic */
	if (endian() == BIG) {		/* put addr at back (big end) */
		bzero(ptr, INET6_ADDRLEN);
		ptr += INET6_ADDRLEN - (sizeof(fill) + sizeof(struct in_addr));
		memcpy(ptr, fill, sizeof(fill));
		ptr += sizeof(fill);
		memcpy(ptr, addr, sizeof(struct in_addr)); 		
	} else { /* put addr at front (little end) */
		bzero(ptr, INET6_ADDRLEN);
		memcpy(ptr, addr, sizeof(struct in_addr)); 		
		ptr += sizeof(struct in_addr);
		memcpy(ptr, fill, sizeof(fill));
		ptr += sizeof(fill);
	}
}

/* return machine endian-ness */
static int endian(void) {
	union {
		short s;
		char c[sizeof(short)];
	} un;

	un.s = 0x0102;
	printf("%s: ", CPU_VENDOR_OS);
	if (sizeof(short) == 2) {
		if (un.c[0] == 1 && un.c[1] == 2)
			return BIG;
		else if (un.c[0] == 2 && un.c[1] == 1)
			return LITTLE;
	} 
	fprintf(stderr, "sizeof(short) = %d\n", (int)sizeof(short));
	return -1;
}
