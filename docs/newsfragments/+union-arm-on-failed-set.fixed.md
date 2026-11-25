A rejected dynamic write to a union member, such as an out-of-range integer
or a value of the wrong type, now makes that member the active arm, as Cap'n
Proto C++ 1.4.0 does. The rejected value is never stored, and the previous
arm's data is left as it was.
