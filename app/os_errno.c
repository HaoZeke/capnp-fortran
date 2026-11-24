#include <errno.h>

/* The C library names the errno accessor differently on Linux and on
   BSD. The interrupted-call value comes from errno.h. */
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__DragonFly__)
int *capnp_errno_loc(void) { return __error(); }
#else
int *capnp_errno_loc(void) { return __errno_location(); }
#endif

int capnp_eintr(void) { return EINTR; }
