Codec schema checks compare a fresh `capnp compile` with the checked-in
request by decoded content, so capnp 1.0.1 and 1.4.0 both match. A list
assigned to a group is skipped when capnp is older than 1.3.0, because
that compiler aborts.
