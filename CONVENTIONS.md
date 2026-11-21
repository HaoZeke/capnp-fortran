# Conventions for capnp-fortran

Versions follow Semantic Versioning, with the pre-1.0 note in the changelog
header. A new public procedure or a new argument on a public reader bumps the
minor version. A fix-only release bumps the patch. A wire or API break is
recorded under Breaking. The version files in this tree are `0.4.0`.

## Changelog

News fragments live in `docs/newsfragments/`. Name them `<issue>.<type>.md`,
or `+slug.<type>.md` when there is no issue. The types are `breaking`,
`security`, `removed`, `deprecated`, `added`, `dev`, `changed`, and `fixed`.
`python3 scripts/changelog_build.py` reads the version from `fpm.toml`, writes
the fragments into `CHANGELOG.md` under the
`<!-- towncrier release notes start -->` marker, deletes the fragments, and
adds a compare link with the other links at the bottom of the file.

A pull request with a user-visible change needs a fragment. A docs-only or
CI-only pull request can carry a `dev` fragment or the `skip-changelog`
label. The towncrier check skips pull requests with that label and pull
requests from `dependabot[bot]`.

## Release

`0.4.0` is already recorded in the version files and in `CHANGELOG.md`.
Merge the pull request onto `main`, then tag. The release workflow stops
with an error when the tagged commit is absent from `main`.
After the towncrier and release-prepare jobs are green on the pull request,
tag that release and push the tag:

```console
git tag -a v0.4.0 -m "v0.4.0"
git push --follow-tags
```

The compare link for `0.4.0` is `v0.3.1...v0.4.0`. It resolves once the
`v0.4.0` tag exists. `python3 scripts/cog_bump.py --auto` on this history
selects a lower version from `v0.3.1`, and the wrapper refuses a version
lower than the changelog before cog starts. `python3 scripts/cog_bump.py
--version 0.4.0` is refused because that section is already present. Cog
does not create a stash for that refusal. The hooks do not rebuild a
changelog section that already exists.

Install towncrier first: `python3 -m pip install "towncrier==26.9.0"`.

Later releases run `python3 scripts/cog_bump.py --auto -A "v{{version}}"`
on `main`, or `python3 scripts/cog_bump.py --version <version> -A
"v{{version}}"` when the commit subjects would miss a public API change.
The pre-bump hooks rewrite the version
files, build the changelog, and run `scripts/check_version.py`. Cocogitto
commits that version. The `-A` message makes the tag annotated, and the
post-bump hook does the same when `-A` is absent. Push with
`git push --follow-tags`. Pushing that tag runs
`.github/workflows/release.yml`.

The tag workflow runs the fpm, Meson, and packaging checks from
`.github/workflows/ci.yml`, then publishes the matching changelog section
and a source tarball with its sha256. `gh` is given
`--repo "$GITHUB_REPOSITORY"`. A second run updates the existing release.
A tag whose version contains a hyphen is marked as a prerelease, and the
release notes use that full version. A prerelease build keeps the news
fragments, so the final release includes them.

`fpm.toml` and the CMake `project(VERSION)` store `major.minor.patch`,
because fpm and CMake reject a prerelease suffix. Meson, the citation, and
Sphinx keep the suffix. The README install pins and the changelog section
use the full tag.

## Issues, pull requests, and labels

Open one issue per gap or bug against Cap'n Proto C++ 1.4.0. Give each a
reproducer or the C++ behaviour to match, and a plain statement of done.
Open one pull request per series, titled in the conventional-commit style of
the repo log. Its body says `Closes #N` for each issue it fixes.

The labels are the GitHub defaults plus `parity`, `wire`, `codegen`, `rpc`,
`dynamic`, `interop`, `security`, `ci`, `dependencies`, and
`skip-changelog`. `.github/labels.yml` defines the set.
`.github/workflows/labels-sync.yml` runs `scripts/sync-labels.sh` on pushes
to `main` that edit `.github/labels.yml` and on `workflow_dispatch`, so
merging updates the live labels automatically. You can also run
`scripts/sync-labels.sh` locally with `gh`. A pull request merges with
green CI and either a fragment, the `skip-changelog` label, or
`dependabot[bot]` as actor. Show the towncrier and release-prepare jobs
green before anyone pushes a tag.
