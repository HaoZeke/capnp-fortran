#!/usr/bin/env bash
# Generator checks used by the codegen unit suite. Arguments:
#   multifile | suffix | annot | fresh
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
bin=$(find "$root/build" -name capnpc-fortran -type f | head -1)
test -n "$bin"
moddir=$(dirname "$(find "$root/build" -name capnp.mod | head -1)")
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
fresh)
  "$bin" "$root/test/fixtures/dynamic.cgr.bin"
  diff -q "$out/dynamic_capnp.f90" "$root/test/generated/dynamic_capnp.f90"
  ;;
*)
  echo "usage: codegen_check.sh multifile|suffix|annot|fresh" >&2
  exit 2
  ;;
esac
