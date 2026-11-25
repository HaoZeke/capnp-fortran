`test_gate2` prints skipped cases and exits 0, so `fpm test` no longer
fails on a missing tool. A case skips only when a probe finds a tool or
prerequisite missing; any command the case runs that fails exits 1.
