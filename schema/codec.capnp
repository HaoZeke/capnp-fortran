@0xa46f9e4625d3a676;

using Json = import "/capnp/compat/json.capnp";

enum Hue {
  red @0;
  blue @1 $Json.name("azure");
}

struct Sample {
  big @0 :Int64;
  ubig @1 :UInt64;
  tiny @2 :Float32;
  wide @3 :Float64;
  title @4 :Text $Json.name("display_name");
  blob @5 :Data;
  b64 @6 :Data $Json.base64;
  hexed @7 :Data $Json.hex;
  hue @8 :Hue;

  job :union {
    none @9 :Void;
    label @10 :Text;
  }

  where :group {
    city @11 :Text;
    zip @12 :UInt16;
  }

  scores @13 :List(Int64);
}
