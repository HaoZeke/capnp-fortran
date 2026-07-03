@0xf1e2d3c4b5a69788;
# Generic-type coverage: type parameters bind to pointer types only, so
# generic fields are AnyPointer slots on the wire. capnpc-fortran emits
# AnyPointer accessors for them (the capnp-c behaviour); brands are
# carried by the wire format regardless.

struct Box(T) {
  value @0 :T;
  label @1 :Text;
}

struct BoxUse {
  textBox @0 :Box(Text);
  anyBox @1 :Box;
}
