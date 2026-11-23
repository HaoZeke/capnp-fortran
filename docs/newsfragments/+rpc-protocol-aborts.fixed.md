A senderLoopback aimed at a capability this vat hosts aborts, because
that object does not point back at the sender. An unknown export id
aborts with the export-id reason. A receiverLoopback
aborts unless its embargo id is one this vat issued. A provide disembargo
is honoured only from the connection that arranged it. A question id
already in use aborts and keeps the earlier export. Question ids past
the 64 slot table are answered, and capabilities they published stay
callable until Finish. A second Return aborts and keeps the first result.
Releasing an unknown export, or more references than it holds, aborts
and leaves the count unchanged.
