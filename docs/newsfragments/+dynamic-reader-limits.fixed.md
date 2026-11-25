Dynamic readers and generated code reject malformed messages before they
read past a segment. List counts stay within the segment and traversal
limit, matching Cap'n Proto C++ 1.4.0. Unknown list element types, invalid
handles, and integers that do not fit a field report an error. Null struct
pointers read as defaults, integer writes to float fields succeed, and
clearing a group also clears the unions nested inside it.
