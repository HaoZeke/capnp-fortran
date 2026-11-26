Float parsing tests give the strtod buffer dummy the target attribute.
A bind(c) character dummy without that attribute lets points-to analysis
fold two-argument c_associated to false at -O1 and above when the end
pointer addresses the buffer.
