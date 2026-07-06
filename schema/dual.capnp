# Two independent unions on one struct: exercises named union which selection.
@0xab12cd34ef560001;

struct Dual {
  primary :union {
    voidA @0 :Void;
    textA @1 :Text;
  }
  secondary :union {
    voidB @2 :Void;
    intB @3 :Int32;
  }
}
