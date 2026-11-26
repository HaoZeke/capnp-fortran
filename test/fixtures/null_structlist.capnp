@0x9a1b2c3d4e5f6071;

struct Elem {
  n @0 :Int64;
  name @1 :Text;
}

struct Wrap {
  union {
    a @0 :Int32;
    g :group {
      union {
        k @1 :Int32;
        items @2 :List(Elem);
        plain @6 :List(Elem) = [];
        obj @3 :Elem;
        nums @5 :List(Int32);
      }
      note @4 :Text;
    }
  }
}
