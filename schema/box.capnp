@0xf1e2d3c4b5a69788;
# Generic-type coverage: type parameters bind to pointer types only, so
# generic fields are AnyPointer slots on the wire. Branded uses resolve
# to typed instantiations (direct, list-element, list-binding, and
# nested-brand positions); unbound uses keep AnyPointer accessors.

struct Box(T) {
  value @0 :T;
  label @1 :Text;
}

struct Nest(T) {
  inner @0 :Box(T);
  boxes @1 :List(Box(T));
}

struct BoxUse {
  textBox @0 :Box(Text);
  anyBox @1 :Box;
  boxes @2 :List(Box(Text));
  listBox @3 :Box(List(Text));
  nest @4 :Nest(Text);
}
