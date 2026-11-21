@0xd4a6b0c0e0000001;

annotation methodNote @0xe2a9dc02b84a8e5d(method) :Text;

struct Dynamic {
  capability @0 :Capability;
  anyStruct @1 :AnyStruct;
  anyList @2 :AnyList;
  choice :union {
    number @3 :Int32;
    label @4 :Text;
    items @5 :List(Int32);
  }
}

interface AnnotatedService {
  ping @0 () -> () $methodNote("ping method");
}
