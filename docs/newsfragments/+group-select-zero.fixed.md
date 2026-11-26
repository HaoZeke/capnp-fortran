A generated group `_select` zeroes every data and pointer slot of the group,
nested groups and inner union discriminants included, after it sets the
discriminant, as the Cap'n Proto C++ 1.4.0 `init<Group>` accessor does.
Bytes from the previous union member no longer show through the group.
