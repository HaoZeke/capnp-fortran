Generated getters for a union arm return `CAPNP_ERR_KIND` when `which()`
names another arm. The check runs before the slot is read, for numeric,
enum, and pointer arms. A scalar union getter takes an optional `err`.
A field inside a group arm uses that arm's check. A union nested in the
group keeps its own check as well.
