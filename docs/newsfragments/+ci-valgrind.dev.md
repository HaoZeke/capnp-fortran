The `test` CI job installs valgrind and `libc6-dbg`, so the `test_gate2`
leak check runs rather than skipping.
