@0xb22b22b22b22b220;
using PairA = import "pair_a.capnp";

struct BoxB(T) {
  value @0 :T;
}

struct UseB {
  b @0 :BoxB(Text);
  a @1 :PairA.BoxA(Data);
}
