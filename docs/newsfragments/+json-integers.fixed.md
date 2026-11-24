An unquoted JSON integer is rounded through a double, matching
capnp 1.4.0. `9007199254740993` becomes `9007199254740992`.
`{"big":9223372036854775807}` and `{"ubig":18446744073709551615}`
are rejected. Quoted forms of those values stay exact.
An out-of-range unquoted token stores the same recovered integer
capnp 1.4.0 prints, and the decode fails. capnp 1.4.0 reports
`Value out-of-range`. This decoder reports `json decode failed`.
