/* Host values for the flags a non-blocking connect has to recognise.
   The numeric values are not the same on every Unix. */
#include <errno.h>
#include <fcntl.h>

int capnp_posix_o_nonblock(void) { return O_NONBLOCK; }

int capnp_posix_eintr(void) { return EINTR; }

int capnp_posix_einprogress(void) { return EINPROGRESS; }
