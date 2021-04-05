#include <stdio.h>
#include <errno.h>
#include <stdlib.h>


#include <sys/epoll.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>

int
main(void)
{

	int efd, sfd, res;
	struct sockaddr_un addr;
	struct epoll_event evts[10];

	efd = epoll_create(1);
	sfd = socket(AF_UNIX, SOCK_STREAM, 0);
	
	if (efd == -1 || sfd == -1) {
		fprintf(stderr, "failed to create epoll or socket\n");
		exit(EXIT_FAILURE);
	}

	addr.sun_family = AF_UNIX;
	memset(addr.sun_path, 0, sizeof(addr.sun_path));
	snprintf(addr.sun_path, sizeof(addr.sun_path), "/home/ellen/listensocket");
	addr.sun_path[0] = '\0';


	res = bind(sfd, (struct sockaddr *)&addr, sizeof(addr));
	if (res == -1) {
		fprintf(stderr, "failed to bind socket: %s\n", strerror(errno));
		exit(EXIT_FAILURE);
	}

	res = listen(sfd, 10);
	if (res == -1) {
		fprintf(stderr, "failed to call listen on socket: %s\n", strerror(errno));
		exit(EXIT_FAILURE);
	}

	evts[0].events = EPOLLIN | EPOLLET;
	evts[0].data.fd = sfd;
	res = epoll_ctl(efd, EPOLL_CTL_ADD, sfd, evts);

	if (res == -1) {
		fprintf(stderr, "epoll_ctl(EPOLL_CTL_ADD) failed: %s\n", strerror(errno));
		exit(EXIT_FAILURE);
	}

	res = epoll_wait(efd, evts, 10, 1000);
	if (res == -1) {
		fprintf(stderr, "epoll_wait failed: %s\n", strerror(errno));
		exit(EXIT_FAILURE);
	}

	printf("Got epoll events: %d\n", res);

	return (0);
}
