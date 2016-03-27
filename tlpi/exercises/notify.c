/* Exercise 91.1 */
#include <sys/inotify.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <limits.h>
#include "tlpi_hdr.h"

#define PERMS S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP
#define BUF_LEN (10 * (sizeof(struct inotify_event) + NAME_MAX + 1))

static void monitor(int fd, const char *path);
static void logEvent(int fd, struct inotify_event *i);

int
main(int argc, char *argv[]) {
	char *path, *logfile;
	int fd;
	
	if (argc < 3 || strcmp(argv[1], "--help") == 0)
		usageErr("%s logfile path", argv[0]);
	logfile = argv[1];
	path = argv[2];
	
	fprintf(stderr, "monitoring %s\n", path);
	fprintf(stderr, "\tall file creations, deletions and renames logged to: ");
	fprintf(stderr, "%s\n", logfile);
	
	if (access(path, F_OK) == -1)
		errExit("path dose not seem accessible: %s", path);

	fd = open(logfile, O_WRONLY | O_APPEND | O_CREAT, PERMS);
	if (fd == -1)
		errExit("open: %s", logfile);
	
	monitor(fd, path);

	exit(EXIT_SUCCESS);
}

/* monitor: log to fd all file creations, deletions and renames"); */
static void
monitor(int fd, const char *path)
{
	int inotifyFd, wd;
	char buf[BUF_LEN] __attribute__((aligned(8)));
	ssize_t nbytes;
	char *p;
	struct inotify_event *event;

	inotifyFd = inotify_init();
	if (inotifyFd == -1)
		errExit("inotify_event");

	wd = inotify_add_watch(inotifyFd, path, IN_CREATE | IN_DELETE | IN_MOVE);
	if (wd == -1)
		errExit("wd");

	fprintf(stderr, "watching: %s\n", path);

	for (;;) {
		nbytes = read(inotifyFd, buf, BUF_LEN);
		if (nbytes == 0)
			fatal("read() from inotify fd returned 0!");

		if (nbytes == -1)
			errExit("read");

		for (p = buf; p < buf + nbytes; ) {
			event = (struct inotify_event *) p;
			logEvent(fd, event);

			p += sizeof(struct inotify_event) + event->len;
		}
	}
}

/* logEvent: log event to fd */
static void
logEvent(int fd, struct inotify_event *i)
{
	dprintf(fd, "logging event \t wd=%2d ", i->wd);
	if (i->cookie > 0)
		dprintf(fd, "cookie = %4d; ", i->cookie);
	
	dprintf(fd, "mask = ");
	if (i->mask & IN_ACCESS)        dprintf(fd, "IN_ACCESS ");
	if (i->mask & IN_ATTRIB)        dprintf(fd, "IN_ATTRIB ");
	if (i->mask & IN_CLOSE_NOWRITE) dprintf(fd, "IN_CLOSE_NOWRITE ");
	if (i->mask & IN_CLOSE_WRITE)   dprintf(fd, "IN_CLOSE_WRITE ");
	if (i->mask & IN_CREATE)        dprintf(fd, "IN_CREATE ");
	if (i->mask & IN_DELETE)        dprintf(fd, "IN_DELETE ");
	if (i->mask & IN_DELETE_SELF)   dprintf(fd, "IN_DELETE_SELF ");
	if (i->mask & IN_IGNORED)       dprintf(fd, "IN_IGNORED ");
	if (i->mask & IN_ISDIR)         dprintf(fd, "IN_ISDIR ");
	if (i->mask & IN_MODIFY)        dprintf(fd, "IN_MODIFY ");
	if (i->mask & IN_MOVE_SELF)     dprintf(fd, "IN_MOVE_SELF ");
	if (i->mask & IN_MOVED_FROM)    dprintf(fd, "IN_MOVED_FROM ");
	if (i->mask & IN_MOVED_TO)      dprintf(fd, "IN_MOVED_TO ");
	if (i->mask & IN_OPEN)          dprintf(fd, "IN_OPEN ");
	if (i->mask & IN_Q_OVERFLOW)    dprintf(fd, "IN_Q_OVERFLOW ");
	if (i->mask & IN_UNMOUNT)       dprintf(fd, "IN_UNMOUNT ");
	dprintf(fd, "\n");

	if (i->len > 0)
		dprintf(fd, "        name = %s\n", i->name);

}
