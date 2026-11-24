A `Resolve` for a promise import adopts a hosted cap, another promise, a
local cap, or an exception. A reflected resolution after a call on the
promise issues an embargo id. An unknown promise id is ignored.
`CapDescriptor.none` and a `Resolve` for a settled import abort.
