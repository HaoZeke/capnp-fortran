#!/usr/bin/env bash
# Generator checks used by the codegen unit suite. Arguments:
#   multifile | suffix | annot | longann | fresh | voidconst |
#   implicit-golden | implicit-roundtrip | box-plain | box-default |
#   box-const | box-any-tag
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

cat > "$out/handlemod.py" << 'ENDHANDLE'
def handle(text, fn, prefix=""):
    import re
    import sys
    m = re.search(
        r"function %s\(h, err\) result\(o\)\n(?:.*\n){0,8}?[ \t]+type\((\w+)\) :: o" % fn,
        text)
    if not m:
        sys.exit(prefix + "no handle for " + fn)
    return m.group(1)
ENDHANDLE

need_capnp() {
  if ! command -v capnp >/dev/null 2>&1; then
    echo "capnp is not on PATH, skipped" >&2
    exit 77
  fi
}

# Box(AnyPointer), a method Box(T) parameter, and a pointer default on Box,
# compiled and executed so the default value comes back.
box_default_order() {
  local stem="$1"
  local label="$2"
  local gen mod
  (cd "$out" && capnp compile -o- "$stem.capnp") > "$out/$stem.cgr"
  "$bin" "$out/$stem.cgr"
  gen=$(find "$out" -name "${stem}_capnp.f90" | head -1)
  test -n "$gen"
  python3 - "$gen" "$label" << 'PY'
import sys
from handlemod import handle
text = open(sys.argv[1]).read()
label = sys.argv[2]
prefix = label + " "

plain = handle(text, "plain_bare_get", prefix)
method = handle(text, "factory_boxy_params_box_get", prefix)
if plain != "box_any_t":
    sys.exit(label + " plain handle " + plain)
if method != "box_any_2_t":
    sys.exit(label + " method handle " + method)
if "box_any_value_get_text" in text:
    sys.exit(label + " plain type gained kind accessors")
if "box_any_2_value_get_text" not in text:
    sys.exit(label + " method type lost kind accessors")
if "parameter :: BOX_ANY_TAG_DEFAULT(" not in text:
    sys.exit(label + " missing plain default blob")
if "parameter :: BOX_ANY_2_TAG_DEFAULT(" not in text:
    sys.exit(label + " missing method default blob")
if "BOX_ANY_2_2_" in text:
    sys.exit(label + " overlapped blob prefix")
print(label, plain, method)
PY
  mod=$(sed -n 's/^module //p' "$gen" | head -1)
  test -n "$mod"
  mkdir -p "$out/rt-$stem"
  cat > "$out/rt-$stem/drv.f90" << EOF
program drv
   use capnp
   use ${mod}
   implicit none
   type(capnp_message_t), target :: msg
   type(box_any_t) :: plain
   type(box_any_2_t) :: boxed
   type(tag_t) :: tg
   integer(int32) :: n
   integer :: err
   call capnp_message_init_builder(msg, err)
   if (err /= 0) error stop 1
   plain = box_any_new(msg, err)
   if (err /= 0) error stop 2
   tg = box_any_tag_get(plain, err)
   if (err /= 0) error stop 3
   n = tag_n_get(tg)
   if (n /= 7_int32) error stop 4
   boxed = box_any_2_new(msg, err)
   if (err /= 0) error stop 5
   tg = box_any_2_tag_get(boxed, err)
   if (err /= 0) error stop 6
   n = tag_n_get(tg)
   if (n /= 7_int32) error stop 7
   call capnp_message_free(msg)
end program drv
EOF
  gfortran $san $cov -std=f2018 -ffree-line-length-none \
    -J"$out/rt-$stem" -I"$out/rt-$stem" -I"$moddir" \
    "$gen" "$out/rt-$stem/drv.f90" "$lib" -o "$out/rt-$stem/roundtrip"
  "$out/rt-$stem/roundtrip"
}

load_runtime() {
  local syms
  lib=$(find "$root/build" -name 'libcapnp.a' -printf '%T@ %p\n' | sort -nr | head -1 | cut -d' ' -f2-)
  test -n "$lib"
  # Finish nm before searching. A pipe into grep -q closes early, nm dies
  # with SIGPIPE, and pipefail then reports a symbol that is present as absent.
  syms=$(nm -g "$lib" 2>/dev/null || true)
  san=
  if grep -q ' __asan' <<< "$syms"; then
    san="-fsanitize=address,undefined"
  fi
  cov=
  if grep -q ' __gcov_init' <<< "$syms"; then
    cov=--coverage
  fi
}

run_codegen_drv() {
  local stem="$1"
  local gen mod
  (cd "$out" && capnp compile -o- "$stem.capnp") > "$out/$stem.cgr"
  "$bin" "$out/$stem.cgr"
  gen=$(find "$out" -name "${stem}_capnp.f90" | head -1)
  test -n "$gen"
  mod=$(sed -n 's/^module //p' "$gen" | head -1)
  test -n "$mod"
  mkdir -p "$out/rt-$stem"
  gfortran $san $cov -std=f2018 -ffree-line-length-none \
    -J"$out/rt-$stem" -I"$out/rt-$stem" -I"$moddir" \
    "$gen" "$out/rt-$stem/drv.f90" "$lib" -o "$out/rt-$stem/roundtrip"
  "$out/rt-$stem/roundtrip"
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
from handlemod import handle
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

plain = handle(text, "plain_bare_get")
method = handle(text, "factory_boxy_params_box_get")
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
import sys
from handlemod import handle
text = open(sys.argv[1]).read()

plain = handle(text, "plain_bare_get")
method = handle(text, "factory_boxy_params_box_get")
stem = method[:-2] if method.endswith("_t") else method
if plain != "box_any_t":
    sys.exit("method-first plain handle " + plain)
if method != "box_any_2_t":
    sys.exit("method-first method handle " + method)
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
  load_runtime
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
box-default)
  need_capnp
  load_runtime
  cat > "$out/plain_first.capnp" << 'EOS'
@0xb0a1d4e7c3f25801;

struct Tag {
  n @0 :Int32;
}

struct Box(T) {
  value @0 :T;
  tag @1 :Tag = (n = 7);
}

struct Plain {
  bare @0 :Box(AnyPointer);
}

interface Factory {
  boxy @0 [T] (box :Box(T)) -> (out :T);
}
EOS
  cat > "$out/method_first.capnp" << 'EOS'
@0xb0a1d4e7c3f25802;

struct Tag {
  n @0 :Int32;
}

struct Box(T) {
  value @0 :T;
  tag @1 :Tag = (n = 7);
}

interface Factory {
  boxy @0 [T] (box :Box(T)) -> (out :T);
}

struct Plain {
  bare @0 :Box(AnyPointer);
}
EOS
  box_default_order plain_first plain-first
  box_default_order method_first method-first
  ;;
box-const)
  need_capnp
  load_runtime
  cat > "$out/box_const.capnp" << 'EOS'
@0xb0a1d4e7c3f25811;

struct Tag {
  n @0 :Int32;
}

struct Box(T) {
  value @0 :T;
  tag @1 :Tag = (n = 7);
}

interface Factory {
  boxy @0 [T] (box :Box(T)) -> (out :T);
}

struct Plain {
  bare @0 :Box(AnyPointer);
}

const boxAny :Tag = (n = 9);
EOS
  mkdir -p "$out/rt-box_const"
  cat > "$out/rt-box_const/drv.f90" << 'EOF'
program drv
   use capnp
   use box_const_capnp
   implicit none
   type(capnp_message_t), target :: msg
   type(box_any_t) :: plain
   type(box_any_2_t) :: boxed
   type(tag_t) :: tg, cst
   integer(int32) :: n
   integer :: err
   call capnp_message_init_builder(msg, err)
   if (err /= 0) error stop 1
   cst = box_any(err)
   if (err /= 0) error stop 2
   n = tag_n_get(cst)
   if (n /= 9_int32) error stop 3
   plain = box_any_new(msg, err)
   if (err /= 0) error stop 4
   tg = box_any_tag_get(plain, err)
   if (err /= 0) error stop 5
   n = tag_n_get(tg)
   if (n /= 7_int32) error stop 6
   boxed = box_any_2_new(msg, err)
   if (err /= 0) error stop 7
   tg = box_any_2_tag_get(boxed, err)
   if (err /= 0) error stop 8
   n = tag_n_get(tg)
   if (n /= 7_int32) error stop 9
   call capnp_message_free(msg)
end program drv
EOF
  run_codegen_drv box_const
  ;;
box-any-tag)
  need_capnp
  load_runtime
  cat > "$out/box_any_tag.capnp" << 'EOS'
@0xb0a1d4e7c3f25812;

struct Tag {
  n @0 :Int32;
}

struct AnyTag {
}

struct Box(T) {
  value @0 :T;
  tag @1 :Tag = (n = 7);
}

interface Factory {
  boxy @0 [T] (box :Box(T)) -> (out :T);
}

struct Plain {
  bare @0 :Box(AnyPointer);
  held @1 :Box(AnyTag);
}
EOS
  mkdir -p "$out/rt-box_any_tag"
  cat > "$out/rt-box_any_tag/drv.f90" << 'EOF'
program drv
   use capnp
   use box_any_tag_capnp
   implicit none
   type(capnp_message_t), target :: msg
   type(box_any_t) :: plain
   type(box_any_2_t) :: boxed
   type(box_any_tag_t) :: held
   type(tag_t) :: tg
   integer(int32) :: n
   integer :: err
   call capnp_message_init_builder(msg, err)
   if (err /= 0) error stop 1
   plain = box_any_new(msg, err)
   if (err /= 0) error stop 2
   tg = box_any_tag_get(plain, err)
   if (err /= 0) error stop 3
   n = tag_n_get(tg)
   if (n /= 7_int32) error stop 4
   boxed = box_any_2_new(msg, err)
   if (err /= 0) error stop 5
   tg = box_any_2_tag_get(boxed, err)
   if (err /= 0) error stop 6
   n = tag_n_get(tg)
   if (n /= 7_int32) error stop 7
   held = box_any_tag_new(msg, err)
   if (err /= 0) error stop 8
   tg = box_any_tag_tag_get(held, err)
   if (err /= 0) error stop 9
   n = tag_n_get(tg)
   if (n /= 7_int32) error stop 10
   call capnp_message_free(msg)
end program drv
EOF
  run_codegen_drv box_any_tag
  ;;
*)
  echo "usage: codegen_check.sh multifile|suffix|annot|longann|fresh|voidconst|implicit-golden|implicit-roundtrip|box-plain|box-default|box-const|box-any-tag" >&2
  exit 2
  ;;
esac
