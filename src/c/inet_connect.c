#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <sys/epoll.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <arpa/inet.h>

int
main(void)
{
	int res, sfd;
	struct sockaddr_in addr;
	char buf[1024];

	sfd = -1;

	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(1234);
	res = inet_aton("127.0.0.1", &addr.sin_addr);

	sfd = socket(AF_INET, SOCK_STREAM, 0);
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

	res = send(sfd, "hehehe", strlen("hehehe"), 0);
	if (res == -1) {
		fprintf(stderr, "send failed: %s\n", strerror(errno));
		exit(EXIT_FAILURE);
	}

	memset(buf, 0, sizeof(buf));
	res = recv(sfd, buf, sizeof(buf), 0);
	if (res == -1) {
		fprintf(stderr, "recv failed: %s\n", strerror(errno));
		exit(EXIT_FAILURE);
	}

	if (res == 0) {
		fprintf(stderr, "peer closed socket\n");
		exit(EXIT_FAILURE);
	}

	buf[sizeof(buf) - 1] = '\0';
	printf("received: %s\n", buf);

	if (sfd != -1) {
		close(sfd);
	}

	return (0);
}

