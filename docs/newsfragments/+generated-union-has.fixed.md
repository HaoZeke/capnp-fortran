Generated `has` for a union pointer arm returns false when `which()`
names another arm. A pointer inside a group arm is absent when that arm
is inactive, including a pointer of a union nested in the group.
