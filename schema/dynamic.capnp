@0xd4a6b0c0e0000001;

struct Dynamic {
  capability @0 :Capability;
  anyStruct @1 :AnyStruct;
  anyList @2 :AnyList;
  choice :union {
    number @3 :Int32;
    label @4 :Text;
  }
}
