#!/usr/bin/env python3
"""Run the RPC differential checker over a range of seeds.

For each seed the raw driver plays the same script against the C++
RpcSystem peer and against the Fortran peer. The two logs are
normalised and compared; a seed is SAME when they match.

Usage:
  run.py [--bin DIR] [--out DIR] [--jobs N] [--steps N] FIRST [LAST]
  run.py --show SEED [--out DIR]

Normalisation, applied to both logs before comparing:
  1. Lines inside one window (from a "== " step marker to the next) are
     compared as a sorted list. Both peers answer a step's messages
     before the sync call returns, but they may emit independent
     messages of one step in a different order.
  2. In exception reasons a leading "remote exception: " is dropped,
     and the application text SRVEXC and the Fortran dispatch text
     "method raised error" both read APPEXC. A peer method raising is
     application behaviour. The driver's ping exception PINGEXC, which
     a slow call passes on, is compared as it is.
  3. A leading "expected <condition> [<values>]; " is dropped from
     reasons. KJ_REQUIRE prefixes it to the message, and the values are
     often addresses; the message after it is what is compared.
The driver itself already names peer-chosen ids by order of first use
and writes an implied release (Return.releaseParamCaps,
Finish.releaseResultCaps) as the Release line it stands for.
"""
import argparse
import concurrent.futures as cf
import os
import re
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))

APP_TEXTS = ("SRVEXC", "method raised error")


def norm_line(line):
    def fix(m):
        reason = m.group(2)
        while reason.startswith("remote exception: "):
            reason = reason[len("remote exception: "):]
        reason = re.sub(r"^expected .*?(\[[^\]]*\])?; ", "", reason)
        if any(t in reason for t in APP_TEXTS):
            reason = "APPEXC"
        return "exc(%s: %s)" % (m.group(1), reason)

    return re.sub(r"exc\((\w+): (.*)\)$", fix, line)


def windows(text):
    out, cur = [], None
    for line in text.splitlines():
        if line.startswith("== "):
            cur = (line, [])
            out.append(cur)
        elif cur is None:
            cur = ("== pre", [line])
            out.append(cur)
        else:
            cur[1].append(norm_line(line))
    return [(head, sorted(body)) for head, body in out]


def run_one(binary, driver, port, seed, steps, log_path):
    peer = subprocess.Popen([binary, str(port)], stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, text=True)
    ready = peer.stdout.readline().strip()
    if ready != "READY":
        peer.kill()
        err = peer.stderr.read()
        with open(log_path, "w") as f:
            f.write("PEER not ready: %s %s\n" % (ready, err))
        return
    args = [driver, str(port), str(seed)]
    if steps:
        args.append(str(steps))
    try:
        res = subprocess.run(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                             text=True, timeout=300)
        log = res.stdout
        if res.returncode != 0:
            log += "DRIVER rc=%d %s\n" % (res.returncode, res.stderr.strip())
    except subprocess.TimeoutExpired as e:
        log = (e.stdout or b"").decode() if isinstance(e.stdout, bytes) else (e.stdout or "")
        log += "DRIVER timeout\n"
    try:
        rc = peer.wait(timeout=10)
    except subprocess.TimeoutExpired:
        peer.kill()
        peer.wait()
        rc = "hung"
    err = peer.stderr.read()
    log += "PEER rc=%s\n" % rc
    with open(log_path, "w") as f:
        f.write(log)
    if err.strip():
        with open(log_path + ".stderr", "w") as f:
            f.write(err)


def first_diff(a, b):
    wa, wb = windows(a), windows(b)
    for i in range(max(len(wa), len(wb))):
        x = wa[i] if i < len(wa) else ("<missing>", [])
        y = wb[i] if i < len(wb) else ("<missing>", [])
        if x != y:
            return i, x, y
    return None


def seed_job(opts, seed):
    out = opts.out
    port = 20000 + (seed * 2) % 40000
    c_log = os.path.join(out, "%d.c.log" % seed)
    f_log = os.path.join(out, "%d.f.log" % seed)
    run_one(os.path.join(opts.bin, "peer_cpp"), os.path.join(opts.bin, "driver"),
            port, seed, opts.steps, c_log)
    run_one(os.path.join(opts.bin, "peer_f"), os.path.join(opts.bin, "driver"),
            port + 1, seed, opts.steps, f_log)
    a = open(c_log).read()
    b = open(f_log).read()
    classes = sorted(set(CLASS_RE.findall(a)) | set(CLASS_RE.findall(b)))
    d = first_diff(a, b)
    if d is None:
        return seed, "SAME", "", classes
    i, x, y = d
    with open(os.path.join(out, "%d.diff" % seed), "w") as f:
        f.write(render(seed, i, x, y))
    return seed, "DIFF", "window %d %s" % (i, x[0]), classes


CLASS_RE = re.compile(r"^== \S+ ([A-Z][A-Z0-9_]+)$", re.M)


def render(seed, i, x, y):
    lines = ["seed %d: first differing window %d" % (seed, i),
             "--- C++ peer: %s" % x[0]]
    lines += ["  " + s for s in x[1]]
    lines.append("--- Fortran peer: %s" % y[0])
    lines += ["  " + s for s in y[1]]
    sx, sy = set(x[1]), set(y[1])
    lines.append("only C++: " + " | ".join(s for s in x[1] if s not in sy))
    lines.append("only Fortran: " + " | ".join(s for s in y[1] if s not in sx))
    return "\n".join(lines) + "\n"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("first", type=int, nargs="?")
    ap.add_argument("last", type=int, nargs="?")
    ap.add_argument("--bin", default=os.path.join(ROOT, "build-rpc-diff"))
    ap.add_argument("--out", default=os.path.join(ROOT, "build-rpc-diff", "logs"))
    ap.add_argument("--jobs", type=int, default=4)
    ap.add_argument("--steps", type=int, default=0)
    ap.add_argument("--show", type=int)
    opts = ap.parse_args()
    os.makedirs(opts.out, exist_ok=True)
    if opts.show is not None:
        p = os.path.join(opts.out, "%d.diff" % opts.show)
        print(open(p).read() if os.path.exists(p) else "seed %d: SAME" % opts.show)
        return 0
    if opts.first is None:
        ap.error("give FIRST [LAST] or --show SEED")
    last = opts.last if opts.last is not None else opts.first
    seeds = list(range(opts.first, last + 1))
    t0 = time.time()
    results = []
    with cf.ThreadPoolExecutor(max_workers=opts.jobs) as ex:
        for r in ex.map(lambda s: seed_job(opts, s), seeds):
            results.append(r)
            print("%d %s %s" % r[:3], flush=True)
    same = sum(1 for r in results if r[1] == "SAME")
    lines = []
    for r in results:
        lines.append("%d\t%s\t%s\t%s" % (r[0], r[1], r[2], ",".join(r[3])))
    lines.append("")
    lines.append("seeds %d-%d: SAME %d DIFF %d" %
                 (seeds[0], seeds[-1], same, len(results) - same))
    # Per scenario class: seeds that ran it, and of those how many differ
    # anywhere and how many first differ inside that class's windows.
    names = sorted({c for r in results for c in r[3]})
    for c in names:
        ran = [r for r in results if c in r[3]]
        diff = [r for r in ran if r[1] == "DIFF"]
        blamed = [r for r in diff if r[2].endswith(" " + c)]
        lines.append("class %s: seeds %d SAME %d DIFF %d first-diff-here %d" %
                     (c, len(ran), len(ran) - len(diff), len(diff), len(blamed)))
    with open(os.path.join(opts.out, "summary.tsv"), "w") as f:
        f.write("\n".join(lines) + "\n")
    for line in lines[len(results) + 1:]:
        print(line)
    print("(%.0fs)" % (time.time() - t0))
    return 0 if same == len(results) else 1


if __name__ == "__main__":
    sys.exit(main())
