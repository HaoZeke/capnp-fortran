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
    zip @12 :UInt16 = 411;
  }

  scores @13 :List(Int64);
}

struct Place {
  city @0 :Text = "Reykjavik";
  code @1 :UInt16 = 101;
}

struct MidFlat {
  midId @0 :Int32;
  place @1 :Place $Json.flatten(prefix = "pl_");
}

struct FlatEnvelope {
  id @0 :Int32;
  direct @1 :Place $Json.flatten();
  nested @2 :MidFlat $Json.flatten(prefix = "m_");
  grp :group $Json.flatten(prefix = "g_") {
    flag @3 :Bool;
  }
  renamedGrp :group $Json.name("rgrp") {
    val @4 :Int32;
  }
}

struct TaggedEvent $Json.discriminator(name = "kind") {
  seq @0 :Int32;
  union {
    idle @1 :Void;
    count @2 :Int32;
    moved @3 :Place $Json.flatten();
    note @4 :Text $Json.name("message");
  }
  tail @5 :Int32;
}

struct ValueEvent $Json.discriminator(name = "type", valueName = "payload") {
  seq @0 :Int32;
  union {
    empty @1 :Void;
    num @2 :Int32;
    loc @3 :Place;
    renamed @4 :Text $Json.name("text_tag");
  }
  tail @5 :Int32;
}

struct MultiUnionDoc {
  id @0 :Int32;
  mode :union $Json.flatten() $Json.discriminator() {
    fast @1 :Int32;
    custom @2 :Place $Json.flatten(prefix = "c_");
  }
  extra :union $Json.name("ext") $Json.discriminator(name = "tag", valueName = "body") {
    off @3 :Void;
    on @4 :Text;
  }
}

struct UnionOfMid $Json.discriminator(name = "kind") {
  union {
    mid @0 :MidFlat $Json.flatten();
    other @1 :Int32;
  }
}

struct UnionOfGroupFlat $Json.discriminator(name = "kind") {
  union {
    g :group $Json.flatten() {
      place @0 :Place $Json.flatten(prefix = "pl_");
    }
    other @1 :Int32;
  }
}

struct InnerDisc $Json.discriminator(name = "innerKind", valueName = "innerVal") {
  union {
    a @0 :Int32;
    b @1 :Text;
  }
}

struct NestedFlatDisc $Json.discriminator(name = "outerKind") {
  union {
    g :union $Json.flatten() $Json.discriminator(name = "innerKind", valueName = "innerVal") {
      a @0 :Int32;
      b @1 :Text;
    }
    other @2 :Int32;
  }
}

struct NestedFlatStructDisc $Json.discriminator(name = "outerKind") {
  union {
    g @0 :InnerDisc $Json.flatten();
    other @1 :Int32;
  }
}

struct VoidPayload {
  v @0 :Void;
  vls @1 :List(Void);
  n @2 :Int32;
}

struct UnionDefaults {
  before @0 :Int32;
  union {
    u0ptr @1 :Text;
    u1ptr @2 :Text = "fallback";
    u2sub @3 :Place = (city = "Oslo", code = 500);
    u3lst @4 :List(Int32) = [7, 8];
    u4dat @5 :Data = 0x"cafe";
  }
  after @6 :Int32;
}

struct EvalItem {
  label @0 :Text;
  matrix @1 :List(List(Int32));
}

struct EvalRoot {
  const nestedPlace :Place = (city = "Zurich", code = 8000);
  title @0 :Text = "default-title";
  home @1 :Place = (city = "Pune", code = 411);
  items @2 :List(EvalItem) = [
    (label = "zero", matrix = [[1, 2], [3]]),
    (label = "one", matrix = [[10], [20, 30]])
  ];
  hue @3 :Hue = blue;
}

const evalConst :EvalRoot = (
  title = "configured",
  home = (city = "Tokyo", code = 100),
  items = [
    (label = "alpha", matrix = [[4, 5]]),
    (label = "beta", matrix = [[6, 7], [8, 9]])
  ],
  hue = blue
);

struct GroupArm $Json.discriminator(name = "kind") {
  union {
    box :group {
      x @0 :Int32;
      y @1 :Int32;
    }
    n @2 :Int32;
  }
}

struct GroupUnion $Json.discriminator(name = "kind") {
  union {
    gu :group {
      union {
        p @0 :Int32;
        q @1 :Int32;
      }
      r @2 :Int32;
    }
    n @3 :Int32;
  }
}

struct TextList {
  values @0 :List(Text);
}

struct ListNest {
  sub @0 :List(ListNest);
}
