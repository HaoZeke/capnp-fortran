@0xd1ff00c0ffee0001;
# Interfaces for the RPC differential checker. Each peer hosts Target as
# its bootstrap. The raw driver hosts Callback as its own bootstrap.
# The Fortran peer mirrors the interface ids as TARGET_IFACE and
# CALLBACK_IFACE.

struct N {
  n @0 :Int64;
}

# What the driver sends as params when it passes a capability. Methods
# read it as N.
struct NCap {
  n @0 :Int64;
  cap @1 :Target;
}

struct CapResult {
  cap @0 :Target;
}

struct CallbackResult {
  cap @0 :Callback;
}

struct Many {
  c0 @0 :Target;
  c1 @1 :Target;
  c2 @2 :Target;
  c3 @3 :Target;
  c4 @4 :Target;
  c5 @5 :Target;
  c6 @6 :Target;
  c7 @7 :Target;
  c8 @8 :Target;
  c9 @9 :Target;
  c10 @10 :Target;
  c11 @11 :Target;
  c12 @12 :Target;
  c13 @13 :Target;
  c14 @14 :Target;
  c15 @15 :Target;
  c16 @16 :Target;
  c17 @17 :Target;
  c18 @18 :Target;
  c19 @19 :Target;
}

interface Target @0xd1ff00c0ffee0011 {
  # Results n = params n + base. No capabilities.
  echo @0 N -> N;
  # Raises an application exception.
  fail @1 N -> N;
  # A new Target whose base is params n.
  make @2 N -> CapResult;
  # Calls the driver's Callback.ping with params n and returns its n.
  slow @3 N -> N;
  # The called Target itself.
  self @4 N -> CapResult;
  # The driver's own Callback, which the driver sees as receiverHosted.
  reflect @5 N -> CallbackResult;
  # Fields c0 to c(n-1) hold new Targets with bases 0 to n-1.
  many @6 N -> Many;
  # Like slow, then returns a new Target whose base is ping's n.
  slowMake @7 N -> CapResult;
}

interface Callback @0xd1ff00c0ffee0012 {
  ping @0 N -> N;
}
