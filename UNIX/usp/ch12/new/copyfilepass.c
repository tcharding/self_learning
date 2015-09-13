/* attribution: UNIX Systems Programming - Robbins and Robbins */

/* copyfilepass: copy file, can be called by thread */
void copyfilepass(void *arg)
{
	int *argint;

	argint = (int *)arg;
	argint[2] = copyfile(argint, argint[1]);
	r_close(argint[0]);
	r_close(argint[1]);
	return (argint + 2)
}
