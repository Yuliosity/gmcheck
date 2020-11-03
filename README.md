# GMCheck

Static verification tools for Game Maker Studio 2 projects. Currently provides a mostly complete parser of the pre-2.3 GML; the checker itself is on early stages of the development.

## Plans

- Deriving and tracking variable types to detect type errors.
- Finding usage of possibly uninitialized variables and missing resources.
- Deriving script type signatures just as for builtin functions.
- "Extending" the GML type system to distinguish booleans, integers, colors, resource identifiers and so on, which are just ordinary numbers in GML.
- Detecting unreachable code, premature `break`s and `exit`s, and so on.
- Colorful, friendly and readable reports.
- Maybe pretty-pritting.
- ...
- and many more! Send your ideas and use-cases to the author or add them as proposal issues.
