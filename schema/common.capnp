@0xc0b1d4f8a2e75301;

struct Vec3 {
  x @0 :Float64;
  y @1 :Float64;
  z @2 :Float64;
}

enum Status {
  idle @0;
  busy @1;
  down @2;
}
