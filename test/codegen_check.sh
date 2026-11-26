#!/usr/bin/env bash
# Generator checks used by the codegen unit suite. Arguments:
#   multifile | suffix | annot | longann | fresh | voidconst |
#   implicit-golden | implicit-roundtrip | box-plain
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

need_capnp() {
  if ! command -v capnp >/dev/null 2>&1; then
    echo "capnp is not on PATH, skipped" >&2
    exit 77
  fi
}

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
implicit-golden)
  need_capnp
  (cd "$root" && capnp compile -o- schema/implicit.capnp) > "$out/i.cgr"
  "$bin" "$out/i.cgr"
  gen=$(find "$out" -name 'implicit_capnp.f90' | head -1)
  test -n "$gen"
  diff -q "$gen" "$root/test/generated/implicit_capnp.f90"
  python3 - "$gen" << 'PY'
import sys
text = open(sys.argv[1]).read()
need = [
    "factory_pair_params_left_get_data",
    "factory_pair_params_left_set_data",
    "factory_pair_params_right_get_list",
    "factory_pair_params_right_set_list",
    "factory_pair_params_right_init_list",
    "factory_talk_params_peer_get_iface",
    "factory_talk_params_peer_set_iface",
    "factory_talk_request",
    "factory_pair_request",
    "factory_boxy_request",
    "holder_put_params_extra_get_data",
    "holder_put_request",
    "factory_boxy_params_note_get",
    "factory_make_results_ok_get",
    "factory_pair_params_blob_get",
    "factory_pair_results_items_get",
    "holder_put_params_extra_get_text_f",
    "box_value_get",
    "box_any_2_value_get_text",
    "plain_bare_get",
    "type(box_any_t)",
    "type(box_any_2_t)",
    "pick_any_value_get_text",
    "factory_pick_request",
    "if (pick_any_which(h) /= 0) then",
]
missing = [n for n in need if n not in text]
if missing:
    sys.exit("missing " + ", ".join(missing))
bad = [
    "holder_put_params_value_get_text",
    "holder_put_params_value_get_data",
    "holder_put_params_value_get_list",
    "holder_put_params_value_get_iface",
    "holder_put_results_kept_get_text",
    "factory_boxy_params_note_get_text",
    "factory_make_results_ok_get_text",
    "factory_pair_params_blob_get_text",
    "box_value_get_text",
    "box_value_get_data",
    "box_value_set_struct",
    "box_value_get_iface",
    "pick_value_get_text",
    "box_any_value_get_text",
    "holder_put_params_extra_get_text(",
]
present = [n for n in bad if n in text]
if present:
    sys.exit("unexpected " + ", ".join(present))

def handle(fn):
    import re
    m = re.search(
        r"function %s\(h, err\) result\(o\)\n(?:.*\n){0,8}?[ \t]+type\((\w+)\) :: o" % fn,
        text)
    if not m:
        sys.exit("no handle for " + fn)
    return m.group(1)

plain = handle("plain_bare_get")
method = handle("factory_boxy_params_box_get")
stem = method[:-2] if method.endswith("_t") else method
if plain != "box_any_t":
    sys.exit("plain-first plain handle " + plain)
if method == plain or (stem + "_value_get_text") not in text:
    sys.exit("plain-first method handle " + method)
PY
  # Method declared before Plain. The plain instantiation still spells
  # box_any, and the method parameter keeps its kind accessors.
  cat > "$out/method_first.capnp" << 'EOS'
@0xe1d4a7c03b5926f9;
struct Box(T) {
  value @0 :T;
}
interface Factory {
  boxy @0 [T] (box :Box(T)) -> (out :T);
}
struct Plain {
  bare @0 :Box(AnyPointer);
}
EOS
  (cd "$out" && capnp compile -o- method_first.capnp) > "$out/mf.cgr"
  "$bin" "$out/mf.cgr"
  mf=$(find "$out" -name 'method_first_capnp.f90' | head -1)
  test -n "$mf"
  python3 - "$mf" << 'PY'
import re, sys
text = open(sys.argv[1]).read()

def handle(fn):
    m = re.search(
        r"function %s\(h, err\) result\(o\)\n(?:.*\n){0,8}?[ \t]+type\((\w+)\) :: o" % fn,
        text)
    if not m:
        sys.exit("no handle for " + fn)
    return m.group(1)

plain = handle("plain_bare_get")
method = handle("factory_boxy_params_box_get")
stem = method[:-2] if method.endswith("_t") else method
if plain != "box_any_t":
    sys.exit("method-first plain handle " + plain)
if method == plain or (stem + "_value_get_text") not in text:
    sys.exit("method-first method handle " + method)
if "box_any_value_get_text" in text:
    sys.exit("method-first plain type gained kind accessors")
print("plain", plain, "method", method)
PY
  ;;
implicit-roundtrip)
  need_capnp
  (cd "$root" && capnp compile -o- schema/implicit.capnp) > "$out/i.cgr"
  "$bin" "$out/i.cgr"
  gen=$(find "$out" -name 'implicit_capnp.f90' | head -1)
  test -n "$gen"
  lib=$(find "$root/build" -name 'libcapnp.a' -printf '%T@ %p\n' | sort -nr | head -1 | cut -d' ' -f2-)
  test -n "$lib"
  san=
  if nm -A "$lib" 2>/dev/null | grep -q ' __asan'; then
    san="-fsanitize=address,undefined"
  fi
  cov=
  if nm -A "$lib" 2>/dev/null | grep -q ' __gcov_init'; then
    cov=--coverage
  fi
  mkdir -p "$out/rt"
  cp "$root/test/fixtures/implicit_driver.f90.in" "$out/rt/implicit_driver.f90"
  gfortran $san $cov -std=f2018 -ffree-line-length-none -J"$out/rt" -I"$out/rt" -I"$moddir" \
    "$gen" "$out/rt/implicit_driver.f90" "$lib" -o "$out/rt/roundtrip"
  "$out/rt/roundtrip"
  ;;
box-plain)
  need_capnp
  (cd "$root" && capnp compile -o- schema/box.capnp) > "$out/b.cgr"
  "$bin" "$out/b.cgr"
  gen="$out/schema/box_capnp.f90"
  test -s "$gen"
  diff -q "$gen" "$root/test/generated/box_capnp.f90"
  python3 - "$gen" << 'PY'
import re, sys
text = open(sys.argv[1]).read()
bad = re.findall(
    r"\bbox_value_(?:get|set|init)_(?:text|data|struct|list|iface|composite)\w*",
    text,
)
if bad:
    sys.exit("box value gained implicit accessors: %s" % bad[:8])
PY
  ;;
*)
  echo "usage: codegen_check.sh multifile|suffix|annot|longann|fresh|voidconst|implicit-golden|implicit-roundtrip|box-plain" >&2
  exit 2
  ;;
esac
