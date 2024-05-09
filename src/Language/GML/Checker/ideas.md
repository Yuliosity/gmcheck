# Checker ideas

## WARN
- N arguments in a script call, but it uses only K<N
- Using a deprecated function/variable
- Unspecified arguments evaluation order
- Instance variable is not used outside of the scope
- Script argument N is missing
- Call event_inherited by the object with no parent
- Resource prefix
- Condition is always true/false
- No `ds_type_destroy` after `ds_type_destroy`, possible leak

## ERROR
- N arguments in a script call, but it uses K>N
- Call unimplemented event
- Not all branches return a value
- No `vertex_format_end` after `vertex_format_begin`
