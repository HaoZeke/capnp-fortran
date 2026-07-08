# Regression: nested enum Kind with enumerant `set` collides with field
# kind_set accessor under case-insensitive Fortran (capnp-fortran-fdyj).
@0xfeedface01234567;

struct InputStanza {
  kind @0 :Kind = generic;
  payload @1 :Text;

  enum Kind {
    generic @0;
    set @1;
    scf @2;
  }
}
