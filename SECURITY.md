# Security Policy

## Supported versions

Security fixes are released through normal semantic-version tags.

| Version | Supported |
| ------- | --------- |
| 0.1.x   | yes       |
| Latest `main` | yes (fixes land here first) |

## Reporting a vulnerability

Please **do not** open a public GitHub issue for unfixed vulnerabilities.

1. Email the maintainer privately: **rgoswami@ieee.org**, **or**
2. Use GitHub **Security Advisories** / private vulnerability reporting if
   enabled for this repository.

Include: affected commit or tag, platform, compiler, a minimal reproducer
when one is available, and impact assessment.

We aim to acknowledge reports promptly and coordinate disclosure once a fix
is tagged.

## Scope

**In scope:** wire-format decoding of untrusted Cap'n Proto messages, the
`bind(c)` ABI surface, the packed/canonical codecs, and the two-party RPC
transport when built.

**Out of scope (unless trivially fixed):** issues only in unreleased
experimental branches; misconfiguration of peer applications; third-party
schema plugins unrelated to `capnpc-fortran`.

## Security boundary

`capnp-fortran` deserializes Cap'n Proto messages and can run an RPC vat over
sockets. Treat untrusted input as untrusted code for the process: a crafted
message can attempt large allocations, deep pointer graphs, or RPC method
invocations exposed by the application. Callers should set traversal/depth
limits where appropriate and not expose raw socket RPC to untrusted networks
without application-level authentication and authorization.
