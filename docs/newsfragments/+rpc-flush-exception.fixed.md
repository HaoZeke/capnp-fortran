A call whose results cap table cannot be flushed is answered with an
exception Return. The flush drops the exports it added and frees the
owned servers those exports introduced, and it frees owned servers
that stayed unexported. An export from before the flush keeps its
refcount.
