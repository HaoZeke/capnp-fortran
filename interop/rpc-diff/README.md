# RPC differential checker

A developer tool that plays the same raw RPC script against a Cap'n
Proto C++ 1.4.0 `RpcSystem` peer and against a Fortran peer built on
`capnp_rpc`, then compares what the two peers send back. It is not
part of `fpm test` or `meson test`: one run takes a minute or more
and opens loopback TCP ports.

```console
$ pixi run -e interop rpc-diff          # build, then seeds 1-300
$ pixi run -e interop rpc-diff 40 60    # seeds 40-60
$ python3 interop/rpc-diff/run.py --show 42 --out build-rpc-diff/logs
```

The exit status is 0 only when every seed is SAME. `JOBS` sets the
parallel runs (default 4) and `OUT` the log directory (default
`build-rpc-diff/logs`). `SRC=<checkout>` makes `build.sh` build the
Fortran peer from another worktree, to compare an older commit.

## Parts

- `diff.capnp`: `Target`, which both peers serve as their bootstrap,
  and `Callback`, which the driver serves as its own.
- `driver.c++`: writes and reads `rpc.capnp` messages directly on a
  socket, with no `RpcSystem`. A seed fixes every choice.
- `peer_cpp.c++`: `Target` on `capnp::TwoPartyVatNetwork` and
  `makeRpcServer`.
- `peer_f.f90`: the same `Target` on `capnp_rpc`.
- `build.sh`, `check.sh`, `run.py`: build, run and compare.

`Target` methods: `echo` (no capabilities), `fail` (raises), `make` (a
new `Target`), `slow` (calls the driver's `Callback.ping` and returns
its answer), `self`, `reflect` (returns the driver's own `Callback`,
which the driver sees as `receiverHosted`), `many` (up to twenty new
capabilities) and `slowMake` (`slow`, then a new `Target`).

## Script

Each step is one random action, then a sync call (`echo` on the first
bootstrap import) whose messages are not logged, then a 25 ms idle
wait. Actions: calls on imports or on `promisedAnswer` targets with
random transforms (including unknown, capless, failed, pending and
finished answers), with `sendResultsTo.yourself`,
`noPromisePipelining`, `onlyPromisePipeline` and capabilities in the
params; Finish with either `releaseResultCaps`; Release; answers to the
peer's pending calls in random order, sometimes with an exception or a
capability; seventy concurrent calls; Resolve of the driver's promises
(also to `receiverAnswer` and to unknown promise ids); bad Disembargo;
extra Bootstrap. A few ids are deliberately invalid, so some seeds end
in an Abort that both peers must send alike.

Five scenario shapes run as several parts, each its own window, and
are counted per class in `summary.tsv`:

- `FINISH_QUEUED_Q2`: Finish for a call queued on a pending answer.
- `EARLY_FINISH_Q1_WITH_Q2`: Finish for a pending answer that has a
  pipelined call waiting on it.
- `FWD_RA_PARAM`: a call forwarded back to the driver whose params
  name a `receiverAnswer` capability.
- `DISEMBARGO_FWD`: `Disembargo(senderLoopback)` on a forwarded answer.
- `FWD_SH_PARAM`: a forwarded call with a `senderHosted` param, then
  its Return and the Finish, to show when the param is released.

After the last step the driver answers every pending call, finishes
every question and releases every import.

## Log and normalisation

`D>` lines are what the driver sent, `P>` lines what the peer sent,
and `== <step> <action>` starts a window. The driver names ids that
the peer picks by order of first use: `P<n>` for the peer's question
ids, `E<n>` for its export ids (a new name once the driver has released
every reference) and `m<n>` for its embargo ids. Driver ids are
`q<n>` and `D<n>`. A `Return.releaseParamCaps` or a
`Finish.releaseResultCaps` is written as the `REL` line of each release
it implies.

`run.py` then applies these rules to both logs:

1. Lines inside one window are compared as a sorted list.
2. In exception reasons a leading `remote exception: ` is dropped, and
   `SRVEXC`, `PINGEXC` and the Fortran dispatch text
   `method raised error` read `APPEXC`.
3. A leading `expected <condition> [<values>]; ` is dropped, the
   `KJ_REQUIRE` prefix that carries addresses.

The first window that differs is written to `<seed>.diff`, with the
lines found on only one side.
