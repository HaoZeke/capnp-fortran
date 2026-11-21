# Changelog fragments

User-visible changes add one fragment here. The release build reads the
version from `fpm.toml`, folds the fragments into `CHANGELOG.md`, and
removes them.

Use one fragment per user-visible change. Name it `<issue>.<type>.md` when an
issue exists, or `+slug.<type>.md` otherwise. Write for library users.

The available types are `breaking`, `security`, `removed`, `deprecated`,
`added`, `dev`, `changed`, and `fixed`.

Create a fragment with `towncrier create` and preview the section with
`python3 scripts/changelog_build.py --draft`. The preview and the written
section use a compare link, placed with the other links at the bottom of
`CHANGELOG.md`.

A docs-only or CI-only pull request can include a `dev` fragment or the
`skip-changelog` label. The towncrier check skips pull requests with that
label and pull requests from `dependabot[bot]`.
