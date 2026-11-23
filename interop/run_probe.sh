#!/usr/bin/env bash
# Run every probe scenario against one C++ 1.4.0 server.
# Usage: run_probe.sh <server> <client>
set -u
SERVER=$1
CLIENT=$2
PORT=$((RANDOM % 20000 + 30000))
SCENARIOS="bootstrap-ok local-keep local-drop remote-reuse same-remote finish-drop embargo-pipeline local-return-cap tail-ok-finish tail-fail tail-worker"

"$SERVER" "$PORT" &
SPID=$!
trap 'kill $SPID 2>/dev/null || true' EXIT

fail=0
for sc in $SCENARIOS; do
  echo "=== $sc ==="
  "$CLIENT" "$PORT" "$sc"
  rc=$?
  echo "RC $sc $rc"
  if [ "$rc" -ne 0 ]; then
    fail=1
  fi
done
exit $fail
