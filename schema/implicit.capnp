@0xe1d4a7c03b5926f8;

struct Item {
  name @0 :Text;
  n @1 :Int32;
}

struct Box(T) {
  value @0 :T;
}

struct Row(T) {
  cell @0 :T;
}

struct Pick(T) {
  union {
    value @0 :T;
    alt @1 :Int32;
  }
}

struct Plain {
  bare @0 :Box(AnyPointer);
}

interface Factory {
  make @0 [T] (value :T) -> (ok :Bool, out :T);
  pair @1 [A, B] (left :A, right :B, blob :Data) -> (left :A, right :B, items :List(Row(A)));
  boxy @2 [T] (box :Box(T), note :Text) -> (out :T);
  talk @3 [C] (peer :C) -> (peer :C);
  pick @4 [T] (slot :Pick(T)) -> (out :T);
}

interface Holder(T) {
  put @0 [U] (value :T, extra :U) -> (kept :T, out :U);
}

# Same Fortran spelling as holder_put_params_extra_get_text, so the
# duplicate scan renames that procedure.
const holderPutParamsExtraGetText :Int32 = 4;
