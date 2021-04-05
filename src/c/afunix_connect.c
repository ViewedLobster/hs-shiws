#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>

#include <sys/epoll.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>

int
main(void)
{
	int res, sfd;
	struct sockaddr_un addr;

	sfd = -1;

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	snprintf(addr.sun_path, sizeof(addr.sun_path), "/home/ellen/listensocket");
	addr.sun_path[0] = '\0';

	sfd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (sfd == -1) {
		fprintf(stderr, "socket failed: %s\n", strerror(errno));
		exit(EXIT_FAILURE);
	}

	res = connect(sfd, (struct sockaddr *)&addr, sizeof(addr));
	if (res == -1) {
		fprintf(stderr, "connect failed: %s\n", strerror(errno));
		exit(EXIT_FAILURE);
	}

	printf("connected!\n");

	if (sfd != -1) {
		close(sfd);
	}

	return (0);
}

