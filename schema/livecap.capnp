@0xc4e7a91b02d8f001;
# A promise that stays unresolved until a later call fulfills it.
# The Fortran client checks that dropping the resolved capability
# drops the C++ server's live count to zero.

interface Box @0xb10c0a9900112233 {
  getPromise @0 () -> (cap :Live);
  resolveNow @1 ();
  live @2 () -> (n :Int64);
}

interface Live @0xb10c0a9900112244 {
}
