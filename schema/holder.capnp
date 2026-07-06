# Interface-typed struct field: Holder.svc is Echo (capability on the wire).
@0xcd12ab34ef780002;

interface Echo {
  ping @0 () -> ();
}

struct Holder {
  svc @0 :Echo;
  note @1 :Text;
}
