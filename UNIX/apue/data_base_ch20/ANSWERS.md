Chapter 
=========
1. If you don't acquire the lock before calling _db_writedat then the data file
   may contain all zeros whilst the chain pointer is still valid (indicating
   that the record is valid). A process that then iterates over the data base
   would get a record of all zeros returned at some stage.
2. If _db_nextrec did not read lock the free list it would get an all zero
   record for exactly the same reason as above.
3. Switching to mandatory locking should not have any differing affect than
   advisory locking. This is true because all processes accessing the data base
   are 'co-operating processes', i.e each correctly acquires/releases read/write
   locks when accessing the database.
4. Any time a write is done to either the .dat or .idx file one could call fsync
   to force a disk write. This would harden the system against inconsistencies
   when there is a system crash.
5. If one writes the index file first then there is a window in which a system
   crash could leave the database in an inconsistent state, i.e there will be an
   index entry without a data entry to back it. However done as we have, data
   write then index write, if there is a system crash the index will not have
   been updated so the write can be re-done. This may result in a 'hole' in the
   data file the size of the pre-crash record written.
6. histogram.c: the hash function is not particularly even in distribution,
   values at the center of the table are approximately half as likely as values
   at the start or end of the table.
7. modified db.c: added dynamic memory allocation and used db->nhash instead of
   NHASH_DEF. 
8. Did not complete, requires second host with NFS share configured.
9. Implemented. Now holes will develop in the data file. To combat this there
   are multiple methods. One would be to create a new index record with the left
   over disk space and add it to the free list pointer. Another would be to keep
   track of holes and re-combine them when a record became free next to a
   hole.
10. Did not complete.   
