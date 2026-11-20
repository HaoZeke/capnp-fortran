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

struct Shape {
  union {
    circle :group {
      radius @0 :Float64;
    }
    square :group {
      width @1 :Float64;
    }
  }
}

enum Hue {
  red @0;
  green @1;
  blue @2;
}

struct Scalars {
  v @0 :Void;
  b @1 :Bool;
  i8 @2 :Int8;
  i16 @3 :Int16;
  i32 @4 :Int32;
  i64 @5 :Int64;
  u8 @6 :UInt8;
  u16 @7 :UInt16;
  u32 @8 :UInt32;
  u64 @9 :UInt64;
  f32 @10 :Float32;
  f64 @11 :Float64;
  hue @12 :Hue;
  note @13 :Text;
  blob @14 :Data;
}

# Groups nested past the depth a walker could cap: selecting another arm
# clears all of them.
struct DeepGroups {
  union {
    a @0 :Int32;
    g1 :group {
      v1 @1 :Int32;
      g2 :group {
        v2 @2 :Int32;
        g3 :group {
          v3 @3 :Int32;
          g4 :group {
            v4 @4 :Int32;
            g5 :group {
              v5 @5 :Int32;
              g6 :group {
                v6 @6 :Int32;
                g7 :group {
                  v7 @7 :Int32;
                  g8 :group {
                    v8 @8 :Int32;
                    g9 :group {
                      v9 @9 :Int32;
                      g10 :group {
                        v10 @10 :Int32;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
