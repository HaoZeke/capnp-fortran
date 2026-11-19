@0xc33c33c33c33c330;

struct SupercalifragilisticexpialidociousPayload {
  n @0 :Int32;
}

struct SupercalifragilisticexpialidociousGenericHolder(T) {
  value @0 :T;
  notes @1 :List(Text);
}

struct UseStem {
  held @0 :SupercalifragilisticexpialidociousGenericHolder(SupercalifragilisticexpialidociousPayload);
  other @1 :SupercalifragilisticexpialidociousGenericHolder(Text);
}
