@0xaaaaaaaaaaaaaaaa;
# Names that exceed Fortran's 63-character identifier limit, a generic
# struct default, and a high-bit annotation id.

struct Seed {
  text @0 :Text = "seed";
}

struct Hold(T) {
  body @0 :Seed = (text = "from-brand");
}

struct HoldUse {
  h @0 :Hold(Data);
}

interface SupercalifragilisticexpialidociousServiceNameThatIsQuiteLong {
  ping @0 () -> ();
}

annotation note @0xfab0b2a29d2f0b19(field) :Text;

struct Outer {
  abcdefghijAbcdefghijAbcdefghijAbcdefghijField @0 :Int32 $note("x");
}

annotation abcdefghijAbcdefghijAbcdefghijAbcdefghijNote @0xe2a9dc02b84a8e5d(struct) :Seed;

struct AbcdefghijAbcdefghijAbcdefghijAbcdefghijHost $abcdefghijAbcdefghijAbcdefghijAbcdefghijNote((text = "ann")) {
  v @0 :Int32;
}
