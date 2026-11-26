An interface client with several superclasses converts to each further
superclass client through `<iface>_as_<Super>(client)`. The result shares
the capability handle, so that superclass's methods can be called. The
client type still extends the first superclass.
