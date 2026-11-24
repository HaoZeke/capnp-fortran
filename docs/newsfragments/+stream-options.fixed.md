Stream readers take a reader options value (traversal budget and nesting
limit) and builder messages take a builder options value (first segment
size and allocation strategy). The defaults stay 8 Mi words, nesting 64,
a 1024-word first segment, and doubling of later segments. A first
segment above the segment cap is rejected, later segments stop at that
cap, and packed input is not expanded past the traversal budget.
