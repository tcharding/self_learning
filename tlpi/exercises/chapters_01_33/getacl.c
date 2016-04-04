/* Exercise 17.1 */
#include <sys/acl.h>
#include <acl/libacl.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "tlpi_hdr.h"
#include "ugid_functions.h"

static void getGroupACL(int fd, const char *group);
static void getUserACL(int fd, const char *user);
static void retrieveAndDisplayPerms(acl_entry_t entry);


int
main(int argc, char *argv[]) {
	int fd;
	char *file;
	char *user, *group;

	user = group = NULL;
	
	if (argc < 4 || strcmp(argv[1], "--help") == 0)
		usageErr("%s (-g group | -u user) file", argv[0]);

	if (strcmp(argv[1], "-g") == 0) {
		group = argv[2];
	} else if (strcmp(argv[1], "-u") == 0) {
		user = argv[2];
	} else {
		eprintf("unknown option: %s\n", argv[1]);
		exit(EXIT_FAILURE);
	}

	file = argv[3];

	fd = open(file, O_RDWR);
	if (fd == -1)
		errExit("open");

	if (user)
		getUserACL(fd, user);
	else
		getGroupACL(fd, group);

	if (close(fd) == -1)
		errExit("close");

	exit(EXIT_SUCCESS);
}

/* getGroupACL: get ACL for fd pertaining to group */
static void getGroupACL(int fd, const char *group)
{
	
}

/* getUserACL: get ACL for fd pertaining to user */
static void getUserACL(int fd, const char *user)
{
	acl_t acl;
	acl_entry_t entry;
	acl_tag_t tag;
	uid_t *uidp;
	char *name;
	int entryId;

	acl = acl_get_fd(fd);
	if (acl == NULL)
		errExit("acl_get_fd");

	/* Walk through entries in this ACL */
	for (entryId = ACL_FIRST_ENTRY; ;entryId = ACL_NEXT_ENTRY) {
		if (acl_get_entry(acl, entryId, &entry) == -1)
			break;	/* error or no more entries */
		if (acl_get_tag_type(entry, &tag) == -1)
			errExit("acl_get_tag_type");

		if (tag == ACL_USER) {
			uidp = acl_get_qualifier(entry);
			if (uidp == NULL)
				errExit("acl_get_qualifier");

			name = userNameFromId(*uidp);
			if (name == NULL)
				errExit("userNameFromID: %d", (int)*uidp);
			if (strcmp(name, user) == 0) {
				retrieveAndDisplayPerms(entry);
			}
		}
	}
	if (acl_free(acl) == -1)
		errExit("acl_free");

}

/* retrieveAndDisplayPerms: print ACL permissions */
static void
retrieveAndDisplayPerms(acl_entry_t entry)
{
	acl_permset_t permset;
	int permVal;
	
	if (acl_get_permset(entry, &permset) == -1)
            errExit("acl_get_permset");

        permVal = acl_get_perm(permset, ACL_READ);
        if (permVal == -1)
            errExit("acl_get_perm - ACL_READ");
        printf("%c", (permVal == 1) ? 'r' : '-');
        permVal = acl_get_perm(permset, ACL_WRITE);
        if (permVal == -1)
            errExit("acl_get_perm - ACL_WRITE");
        printf("%c", (permVal == 1) ? 'w' : '-');
        permVal = acl_get_perm(permset, ACL_EXECUTE);
        if (permVal == -1)
            errExit("acl_get_perm - ACL_EXECUTE");
        printf("%c", (permVal == 1) ? 'x' : '-');

        printf("\n");
}



