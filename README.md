# GMCheck

A static verification toolset for finding possible bugs in Game Maker Studio 2 projects. On its early stages of development.

## Usage

GMCheck is best compiled and run using [Haskell Stack](https://haskellstack.org):

`stack build`

`stack exec gmcheck-exe -- -o path-to-report.html path-to-gms-project-directory/`

## Features and Plans

- ✔️ Mostly complete parser of GML, including 2.3 additions.
- ✔️ Deriving and tracking variable types to detect type errors.
- ✔️ "Extending" the GML type system to distinguish floats, integers, colors, resource identifiers and so on, which are just ordinary numbers in GML.
- ✔️ Type-annotated signatures of built-in functions and variables, stored in editable external file.
- ✔️ Parsing [GMEdit](https://github.com/YellowAfterlife/GMEdit)-style type annotations: `var label /*:string*/`.
- ✔️ Finding usage of possibly uninitialized variables and missing resources.
- Deriving script type signatures from their arguments and usage.
- Detecting unreachable code, premature `break`s and `exit`s, and so on.
- Detecting possible memory leaks from undestroyed data structures.
- Colorful, friendly and readable reports.
- Maybe pretty-pritting.
- ...
- and many more! Send your ideas and use-cases to the author or add them as proposal issues.

## Known issues

- Regions are not yet supported.
- Due to the highly dynamic nature of GML, it's impossible to 100% detect and analyze variables without actually running the code. Depending on the project, there may be a high percent of false positives.
