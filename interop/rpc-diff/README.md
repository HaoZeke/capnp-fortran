# RPC differential checker

A developer tool that plays the same raw RPC script against a Cap'n
Proto C++ 1.4.0 `RpcSystem` peer and against a Fortran peer built on
`capnp_rpc`, then compares what the two peers send back. It is not
part of `fpm test` or `meson test`: one run takes a minute or more
and opens loopback TCP ports.

```console
$ pixi run -e interop rpc-diff          # build, then seeds 1-300 and the probes
$ pixi run -e interop rpc-diff 40 60    # seeds 40-60 and the probes
$ python3 interop/rpc-diff/run.py --probes --bin build-rpc-diff --out /tmp/p
$ python3 interop/rpc-diff/run.py --show 42 --out build-rpc-diff/logs
$ python3 interop/rpc-diff/run.py --force RA_RUNNING --port-base 30000 \
    --bin build-rpc-diff --out /tmp/f 1 300
$ python3 interop/rpc-diff/run.py --show probe:FWD_RA_PLAIN_PENDING --out /tmp/p
```

The exit status is 0 only when every seed and probe is SAME. `JOBS` sets the
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

## Forced classes

`run.py --force <CLASS>` makes about two steps in five of each seed play
one shape the random steps never build, and `run.py --mix` lets any of
them replace a step now and then. Without either flag the seed scripts
are the plain ones. Each forced class is counted in `summary.tsv`:

- `NPP_OPP_FWD`: a forwarded call with `noPromisePipelining` and
  `onlyPromisePipeline`, then a Disembargo, a pipelined call, the
  answer and a Finish in a random order.
- `RA_RUNNING`: a `receiverAnswer` param names an answer that is still
  running.
- `RESOLVE_REFLECTED_RA`: a promise param resolves to `receiverAnswer`
  on an answer that reflects the driver's own cap back.
- `RELEASED_EXPORT_REUSE`: a result cap is released while a call on it
  still runs, and a new export takes its id.
- `RA_REWRITE`: the same `receiverAnswer` param written twice, on an
  answer under `noPromisePipelining` or one that failed.
- `HINTS_FWD_RA`: a forwarded `receiverAnswer` param with random hints
  on both calls.
- `PLAIN_FWD_PROMISE_RESULT`: a plain forward under
  `noPromisePipelining` returns a promise the driver later resolves.
  The answer's Finish and Releases follow in a random order.

Each seed uses two ports from `--port-base` (default 20000) on, so runs
that overlap in time need distinct bases.

## Probes

A probe is a fixed script, run with `driver <port> probe:<NAME>` and by
`run.py --probes`. Each pins one exchange where the Fortran peer once
differed from C++ 1.4.0, and is its own class in `summary.tsv`:

- `PARAM_PROMISE_MERGE_HOSTED`, `PARAM_PROMISE_MERGE_PROMISE`: a param
  promise resolves to an id another call's params name. One import
  counts both, so the last Finish Releases every ref at once.
- `FWD_RA_RELEASED_PARAM`: a forwarded `receiverAnswer` param names a
  result cap the driver has released. C++ exports it again.
- `DISEMBARGO_PLAIN_FWD`, `DISEMBARGO_PLAIN_FWD_CAP`,
  `DISEMBARGO_PLAIN_FWD_FINISH`: `Disembargo(senderLoopback)` on a call
  that went on as a plain call. C++ waits for its Return, then echoes
  on the cap or aborts on a null result.
- `FINISH_PLAIN_FWD_NPP`, `FINISH_PLAIN_FWD_NPP_CAP`,
  `FINISH_PLAIN_FWD_YOURSELF`: Finish for such a call cancels it at
  once.
- `FWD_SP_RESOLVE_TO_RA`: a forwarded param promise resolves to
  `receiverAnswer` on the peer's own export, so a Disembargo goes
  first.
- `FWD_RA_PLAIN_PENDING`, `FWD_RA_PLAIN_PENDING_CALL`,
  `FWD_RA_PLAIN_PENDING_RELEASE`: a forwarded `receiverAnswer` param
  names a plain call still pending. C++ exports a hosted cap that
  forwards to that call's pipeline.
- `YOURSELF_NPP_PLAIN_FWD`, `YOURSELF_NPP_PLAIN_FWD_EXC`,
  `YOURSELF_NPP_PLAIN_FWD_EARLY`: a yourself call with
  `noPromisePipelining` goes on as a plain call. Its question lives
  until Finish, after a result or an exception.
- `PROXY_EARLY_FINISH`: Finish for a plain call whose answer a
  forwarded `receiverAnswer` param still uses. The question lives on.
- `DISEMBARGO_PLAIN_FWD_NPP`: a Disembargo on a plain call that set
  `noPromisePipelining`. C++ aborts.
- `DISEMBARGO_PLAIN_FWD_PROMISE_CAP`: a Disembargo waits on a plain
  call that returns a promise cap, and goes after its Resolve.
- `FWD_SP_RESOLVE_TO_RA_PENDING_NPP`, `FWD_RA_PENDING_NPP`,
  `FWD_RA_RETURNED_NPP`: a forwarded param names an answer under
  `noPromisePipelining`. C++ writes a broken promise and resolves it
  to an exception.
- `PARAM_PROMISE_WRITTEN_MERGE`, `PARAM_PROMISE_RESOLVE_TO_LIVE_IMPORT`:
  a param promise merges with another hold or a live import. The
  Disembargo and the Release counts follow C++.

`run.py --probes` also replays the seeds in `FIXED_SEEDS`, random
scripts that once differed, on a line of their own in the summary.

## Log and normalisation

`D>` lines are what the driver sent, `P>` lines what the peer sent,
and `== <step> <action>` starts a window. The driver names ids that
the peer picks by order of first use: `P<n>` for the peer's question
ids, `E<n>` for its export ids (a new name once the driver has released
every reference) and `m<n>` for its embargo ids. Driver ids are
`q<n>` and `D<n>`. A `Return.releaseParamCaps` or a
`Finish.releaseResultCaps` is written as the `REL` line of each release
it implies. A Return that sets `releaseParamCaps` also shows
`rpc=1`, since a Return with no param caps implies no Release. A Finish
sent before its Return shows `rrc=1` or `rrc=0`.

`run.py` then applies these rules to both logs:

1. Lines inside one window are compared as a sorted list.
2. In exception reasons a leading `remote exception: ` is dropped, and
   `SRVEXC` and the Fortran dispatch text `method raised error` read
   `APPEXC`. The ping exception `PINGEXC` is compared as it is.
3. A leading `expected <condition> [<values>]; ` is dropped, the
   `KJ_REQUIRE` prefix that carries addresses.

The first window that differs is written to `<seed>.diff`, with the
lines found on only one side.
