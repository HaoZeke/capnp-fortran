#!/usr/bin/env bash
# Regenerate the AddressBook wire goldens with the upstream Cap'n Proto CLI.
#
# test/fixtures/addressbook.bin and its packed and canonical companions are
# what the suite compares this runtime's output against, so they have to come
# from capnp itself. Run with --check to assert the checked-in files still
# match, which is what proves the comparison is against upstream and not
# against our own earlier output.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FIX="$ROOT/test/fixtures"
SCHEMA="$ROOT/schema/addressbook.capnp"

if ! command -v capnp >/dev/null 2>&1; then
  echo "gen-fixtures: no capnp CLI on PATH" >&2
  exit 1
fi

CHECK=0
[[ "${1:-}" == "--check" ]] && CHECK=1

OUT="$FIX"
if [[ $CHECK -eq 1 ]]; then
  OUT=$(mktemp -d)
  trap 'rm -rf "$OUT"' EXIT
fi

capnp encode "$SCHEMA" AddressBook >"$OUT/addressbook.bin" <<'EOF'
( people = [
  ( id = 123,
    name = "Alice",
    email = "alice@example.com",
    phones = [
      (number = "555-1212", type = mobile)
    ],
    employment = (school = "MIT")
  ),
  ( id = 456,
    name = "Bob",
    email = "bob@example.com",
    phones = [
      (number = "555-4567", type = home),
      (number = "555-7654", type = work)
    ],
    employment = (unemployed = void)
  )
] )
EOF

capnp convert binary:packed "$SCHEMA" AddressBook \
  <"$OUT/addressbook.bin" >"$OUT/addressbook.packed.bin"
capnp convert binary:canonical "$SCHEMA" AddressBook \
  <"$OUT/addressbook.bin" >"$OUT/addressbook.canonical.bin"

if [[ $CHECK -eq 1 ]]; then
  rc=0
  for f in addressbook.bin addressbook.packed.bin addressbook.canonical.bin; do
    if cmp -s "$OUT/$f" "$FIX/$f"; then
      echo "ok   $f matches $(capnp --version)"
    else
      echo "FAIL $f differs from $(capnp --version)" >&2
      rc=1
    fi
  done
  exit $rc
fi

echo "wrote goldens under $FIX with $(capnp --version)"
