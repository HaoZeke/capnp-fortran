#!/usr/bin/env bash
# Generator checks used by the codegen unit suite. Arguments:
#   multifile | suffix | annot | longann | fresh | voidconst
# The plugin and the module directory come from the environment, set by
# the unit test to the binary it is running. Searching the build tree
# would accept a stale profile.
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
bin=${CAPNP_FORTRAN_BIN:-}
moddir=${CAPNP_FORTRAN_MODDIR:-}
if [ -z "$bin" ] || [ -z "$moddir" ]; then
  echo "CAPNP_FORTRAN_BIN and CAPNP_FORTRAN_MODDIR are required" >&2
  exit 2
fi
case "$bin" in
  /*) ;;
  *) bin="$root/$bin" ;;
esac
case "$moddir" in
  /*) ;;
  *) moddir="$root/$moddir" ;;
esac
if [ ! -x "$bin" ]; then
  echo "plugin is not executable: $bin" >&2
  exit 2
fi
if [ ! -d "$moddir" ]; then
  echo "module directory is missing: $moddir" >&2
  exit 2
fi
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
cd "$out"

case "${1:-}" in
multifile)
  (cd "$root" && capnp compile -o- -Ischema \
    schema/pair_a.capnp schema/pair_b.capnp) > "$out/m.cgr"
  "$bin" "$out/m.cgr"
  test -s "$out/schema/pair_a_capnp.f90"
  test -s "$out/schema/pair_b_capnp.f90"
  ;;
suffix)
  (cd "$root" && capnp compile -o- schema/longinst.capnp) > "$out/l.cgr"
  "$bin" "$out/l.cgr"
  python3 - "$out/schema/longinst_capnp.f90" <<'PY'
import re, sys
text = open(sys.argv[1]).read()
ids = re.findall(r"\b[A-Za-z_][A-Za-z0-9_]{63,}\b", text)
if ids:
    sys.stderr.write("\n".join(ids) + "\n")
    sys.exit(1)
types = re.findall(r"^   type :: (\w+)_t$", text, re.M)
held = [t for t in types if "supercali" in t]
if len(set(held)) < 2:
    sys.stderr.write("instantiations collided: %s\n" % held)
    sys.exit(1)
PY
  gfortran -std=f2018 -ffree-line-length-none -fsyntax-only -I"$moddir" \
    "$out/schema/longinst_capnp.f90"
  ;;
annot)
  (cd "$root" && capnp compile -o- schema/idents.capnp) > "$out/i.cgr"
  "$bin" "$out/i.cgr"
  python3 - "$out/schema/idents_capnp.f90" <<'PY'
import re, sys
text = open(sys.argv[1]).read()
kinds = re.findall(r"parameter :: (\w+)_KIND\b", text)
if not kinds:
    sys.exit("no annotation kinds")
for k in kinds:
    value = (":: %s " % k) in text or (":: %s(" % k) in text or (":: %s =" % k) in text
    default = ("%s_DEFAULT" % k) in text
    if not value and not default:
        sys.exit("stem split: %s" % k)
PY
  ;;
longann)
  (cd "$root" && capnp compile -o- schema/longann.capnp) > "$out/t.cgr"
  "$bin" "$out/t.cgr"
  f=$(find "$out" -name longann_capnp.f90 | head -1)
  test -n "$f"
  python3 - "$f" <<'PY'
import sys
text = open(sys.argv[1]).read()
wide = [n for n, l in enumerate(text.splitlines(), 1) if len(l) > 132]
if wide:
    sys.exit("lines over 132 columns: %s" % wide[:10])
PY
  (cd "$out" && gfortran -c -I"$moddir" "$f")
  ;;
fresh)
  "$bin" "$root/test/fixtures/dynamic.cgr.bin"
  diff -q "$out/schema/dynamic_capnp.f90" "$root/test/generated/dynamic_capnp.f90"
  "$bin" "$root/test/fixtures/adder.cgr.bin"
  diff -q "$out/schema/adder_capnp.f90" "$root/test/generated/adder_capnp.f90"
  ;;
voidconst)
  (cd "$root" && capnp compile -o- schema/voidconst.capnp) > "$out/v.cgr"
  "$bin" "$out/v.cgr"
  diff -u "$root/test/generated/voidconst_capnp.f90" "$out/schema/voidconst_capnp.f90"
  cat > "$out/drv.f90" << 'EOF'
program drv
   use voidconst_capnp, only: nothing
   implicit none
   integer, parameter :: seen = nothing
   integer :: x
   x = seen
   if (x /= 0) stop 1
end program
EOF
  gfortran -std=f2018 -fsyntax-only -I"$moddir" -J"$out" \
    "$out/schema/voidconst_capnp.f90" "$out/drv.f90"
  ;;
*)
  echo "usage: codegen_check.sh multifile|suffix|annot|longann|fresh|voidconst" >&2
  exit 2
  ;;
esac
