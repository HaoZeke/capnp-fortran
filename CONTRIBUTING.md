# Contributing

Thanks for interest in **capnp-fortran**. This document is the short path for
local development; the full tutorial lives in the README and Sphinx docs.

## Prerequisites

- Fortran 2018 compiler (`gfortran` 12+ recommended)
- [fpm](https://fpm.fortran-lang.org/) and/or [pixi](https://pixi.sh/)
- Optional: [Cap'n Proto](https://capnproto.org/) `capnp` for schema compile
  and interop tiers; C compiler for the cmocka/c-capnproto interop suite

## Setup

```bash
git clone https://github.com/HaoZeke/capnp-fortran.git
cd capnp-fortran
# pixi (preferred for full matrix):
pixi install
pixi run test
# or fpm only:
fpm test
```

Docs: `pixi run -e docs docs`. Hooks: `prek install` then `prek run -a`
(config in `prek.toml`). Version bumps use `cog bump` when cutting releases.

## Code style

- Free-form Fortran, `implicit none`, modern `iso_fortran_env` /
  `iso_c_binding` kinds
- Prefer small, tested modules; keep generated `*_capnp.f90` regeneration
  deterministic via `capnpc-fortran`
- Conventional Commits for subjects (`feat:`, `fix:`, `docs:`, `test:`, …)
- Do not commit secrets, machine-local paths, or agent/process narration in
  public commits

## Tests

| Command | What it covers |
| ------- | -------------- |
| `fpm test` | Core runtime, codegen fixtures, RPC unit suite |
| Interop tier (see `interop/README.md`) | Golden wire vs c-capnproto; optional C++ RPC peer |
| `scripts/std-check.sh` | `-std=f2018` compile gate |

A change that touches wire layout or codegen must keep `fpm test` green. If
you change the plugin, regenerate checked-in fixtures the same way CI does.

## Pull requests

1. Branch from current `main`.
2. Keep the diff focused; separate mechanical style from behavior when both
   appear.
3. Describe *what* and *why*; link issues when applicable.
4. Ensure `fpm test` (and any relevant interop tier) pass before asking for
   review.

## Security

See [SECURITY.md](SECURITY.md) for private vulnerability reporting. Do not
file public issues for unfixed security bugs.

## License

By contributing, you agree that your contributions are licensed under the
project’s MIT license (see [LICENSE](LICENSE)).
