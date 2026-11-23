@0xa11a11a11a11a110;
using PairB = import "pair_b.capnp";

struct BoxA(T) {
  value @0 :T;
}

struct UseA {
  a @0 :BoxA(Text);
  b @1 :PairB.BoxB(Data);
}
