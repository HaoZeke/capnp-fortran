# Regression: field path + accessor suffix would exceed Fortran 63-char idents
# without ident_fit (capnp-fortran-3kcj).
@0xdeadbeefcafebabe;

struct SystemSection {
  couplingsFiniteDifferenceDisplacement @0 :Float64;
  anotherQuiteLongFieldNameForGoodMeasure @1 :Int32;
}
