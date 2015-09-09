Chapter 
=========
0. thread_ids.c
1. return.c
2. In order for the master thread to modify a job one would need a mutex on the
   que. However this would not be any more than the exclusion required at
   present for the master thread to add jobs to the que while the workers remove
   jobs.
3. work_que.c, uses mutex jobq and condition qready.
4. You may signal the condition after unlocking only if code can tolerate race
   condition on the state that triggered the signal i.e the condition may have
   changed to untrue again.
5. pthread_barrier.c, tst-barrier.c, barrier.c
   


key 
---
term - completed at terminal
