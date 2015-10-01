#include "apue.h"
#include "apue_db.h"
#include <fcntl.h>

#define MAX_KEY_LEN 64
#define MAX_DATA_LEN 512

#define NUM_RECORDS 10000
#define NHASH 160

int
main(void)
{
	DBHANDLE	db;
	char data[MAX_DATA_LEN];
	char key[MAX_KEY_LEN];
	char *key_prefix = "Key-";
	char *data_prefix = "Auto-generated data record : ";
	int i, j, val;
	int hash_cnt[NHASH];	/* count of hash chain hits */

	bzero(hash_cnt, sizeof(hash_cnt));
	if ((db = db_open(NHASH, "test_db", O_RDWR | O_CREAT | O_TRUNC,
	  FILE_MODE)) == NULL)
		err_sys("db_open error");

	for (i = 0; i < NUM_RECORDS; i++) {
		sprintf(key, "%s%d", key_prefix, i);
		sprintf(data, "%s%d", data_prefix, i);
		if (db_store(db, key, data, DB_INSERT) != 0)
			err_quit("db_store error for i: %d\n", i);
	}
	db_rewind(db);
				/* iterate over database */
	while (db_nextrec(db, key) != NULL) {
		val = db_hash(db, key);

		hash_cnt[val]++;
	}
				/* print histogram */
	for (i = 0; i < NHASH; i++) {
		printf("%d | ", i);
		for (j = 0; j < hash_cnt[i]; j++)
			printf("*");
		printf("\n");
	}
	db_close(db);
	exit(0);
}
