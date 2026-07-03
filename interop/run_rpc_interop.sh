#!/usr/bin/env bash
# Orchestrates the RPC interop test: start the C++ peer, run the Fortran
# client against it, tear down. Usage: run_rpc_interop.sh <server> <client>
set -u
SERVER=$1
CLIENT=$2
PORT=$((RANDOM % 20000 + 30000))

"$SERVER" "$PORT" &
SPID=$!
trap 'kill $SPID 2>/dev/null' EXIT

"$CLIENT" "$PORT"
RC=$?
exit $RC
