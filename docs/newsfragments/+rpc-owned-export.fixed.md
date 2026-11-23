`rpc_ctx_export_cap` accepts `owned=.true.` for a server this vat
allocated. The export table frees that server when its refcount
reaches zero, and `rpc_conn_close` drops every export the peer will
not Release. A server is not freed while its `dispatch` is running;
the free runs once after that call returns.
