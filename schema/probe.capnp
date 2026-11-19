@0xb10c0a9900ee0001;
# Calls a Fortran vat makes against a Cap'n Proto 1.4.0 peer.
# The peer hosts Box. The Fortran vat hosts Echo and Worker.

struct EchoResult {
  n @0 :Int32;
}

struct Empty {
}

struct WorkerResult {
  w @0 :Worker;
}

interface Echo @0xb10c0a9900ee0011 {
  echo @0 (n :Int32) -> EchoResult;
  failMe @1 () -> Empty;
  getWorker @2 () -> WorkerResult;
}

interface Worker @0xb10c0a9900ee0012 {
  ping @0 (n :Int32) -> (n :Int32);
}

interface Live @0xb10c0a9900ee0013 {
}

interface Box @0xb10c0a9900ee0014 {
  getPromise @0 () -> (cap :Live);
  resolveNow @1 ();
  live @2 () -> (n :Int64);
  setPeer @3 (echo :Echo);
  tailEcho @4 (n :Int32) -> EchoResult;
  tailFail @5 () -> Empty;
  sameRemote @6 () -> (a :Live, b :Live);
  oneRemote @7 () -> (cap :Live);
  promiseOfPeer @8 () -> (cap :Echo);
  resolvePeer @9 (dropRef :Bool);
  tailWorker @10 () -> WorkerResult;
  begin @11 ();
}
