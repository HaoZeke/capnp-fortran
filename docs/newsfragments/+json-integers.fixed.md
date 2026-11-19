A JSON integer above 2^53 keeps every digit. capnp 1.4.0 rounds it
through a double and rejects a value that double cannot hold, including
`{"i64":9223372036854775807}` and `{"u64":18446744073709551615}`,
which this decoder accepts.
