/* Host values for the flags a non-blocking connect has to recognise.
   The numeric values are not the same on every Unix. */
#include <errno.h>
#include <fcntl.h>
#include <sys/socket.h>

int capnp_posix_o_nonblock(void) { return O_NONBLOCK; }

int capnp_posix_eintr(void) { return EINTR; }

int capnp_posix_einprogress(void) { return EINPROGRESS; }

/* send() that reports a closed peer as EPIPE instead of raising SIGPIPE,
   which would end the process. */
long capnp_posix_send(int fd, const void *buf, size_t n)
{
#ifdef MSG_NOSIGNAL
    return (long)send(fd, buf, n, MSG_NOSIGNAL);
#else
#ifdef SO_NOSIGPIPE
    int one = 1;
    (void)setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof one);
#endif
    return (long)send(fd, buf, n, 0);
#endif
}
