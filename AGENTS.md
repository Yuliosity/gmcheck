# AGENTS.md

## Build & Run

- **Build tool**: Stack (resolver `lts-23.24`). Not cabal-install.
- `stack build` — compile everything (lib + exe + tests)
- `stack test` — run parser tests (hspec + hspec-megaparsec)
- `stack exec ghci` — REPL (custom prompt via `.ghci`)
- `stack exec gmcheck-exe -- -o report.html path/to/gms-project/` — run the checker

## Config files

- `package.yaml` is the **source of truth** for build config. `gmcheck.cabal` is **generated** by hpack and **git-ignored**. Never edit `.cabal` directly; edit `package.yaml` then regenerate with `stack build` (which auto-runs hpack).
- `stack.yaml` pins the LTS snapshot. The lockfile (`stack.yaml.lock`) is also git-ignored and auto-generated.

## Architecture

- **Library** (`src/`): all modules under `Language.GML.*`. Core pipeline: `Project` (load GMS project) → `Checker` (typecheck) → `Checker.Render` (HTML report).
- **Executable** (`app/Main.hs`): CLI using optparse-applicative. Flags: `-o` output report, `-b` builtin path (default `data`), `-e` space-separated error names to disable, positional `PROJECT` directory.
- **Builtin signatures** (`data/*.gmli`): type signatures for GML built-in functions, enums, globals, instances, newtypes. Parsed by `Checker.Builtin` and `Parser.Types`.
- **Tests** (`src/Language/GML/Parser/test/`): parser-only tests (Lexer, AST, Types). There are no checker/integration tests.
- **Test fixtures**: `projects/SmokeTest/` — a sample GMS project.

### Module map

| Module | Role |
|--------|------|
| `Language.GML.AST` | Core GML AST: expressions, statements, variables, operators |
| `Language.GML.Types` | Type system: `Type`, `Signature`, `Enum`, subtyping |
| `Language.GML.Location` | `Pos`, `Source` (SrcScript/SrcObject/SrcRoom), `Located` wrapper |
| `Language.GML.Events` | Object event types + `Read` instance that parses GMS event filenames |
| `Language.GML.Token` | Token data type (unused by main parser — see note below) |
| `Language.GML.Project` | GMS project loader: walks directories, parses .gml/.yy files |
| `Language.GML.Preprocess` | **Stub** — no implementation |
| `Language.GML.Checker` | Core typechecker — pure `RWS` monad (Reader: settings, Writer: Report, State: context) |
| `Language.GML.Checker.Builtin` | Loads `.gmli` files into `Builtin { bFunctions, bGlobalVar, bInstanceVar }` |
| `Language.GML.Checker.Errors` | `Error` sum type (19 constructors), `Report` monoid, numeric error codes |
| `Language.GML.Checker.Render` | HTML report via blaze-html |
| `Language.GML.Parser.AST` | Main GML parser — directly constructs `Language.GML.AST` types |
| `Language.GML.Parser.Lexer` | Scannerless lexer: whitespace/comments, keywords, literals, positions |
| `Language.GML.Parser.Types` | Parser for `.gmli` signature files (types, signatures, enums) |
| `Language.GML.Parser.Token` | Standalone tokenizer — **not used** by the main parser |
| `Language.GML.Parser.Common` | Megaparsec setup: `type Parser = Parsec Void Text` |

## Checker internals

- **Monad**: `type Checker = RWS Settings Report Context`. **Pure** — no IO. All loading happens outside (`loadProject`, `loadBuiltin`), then pure checking over the loaded data.
- **Variable resolution** (4-tier priority): macros → builtins → project resources → local stack → object instance variables.
- **Scoping**: `withFrame` pushes/pops a `Memory` (local var frame) on `cLocal` stack. `withScope` sets the current object for instance-var lookup via `cScope`.
- **Error reporting**: Uses `Writer tell` via `reportPos` which checks `sDisabledErrors` and appends a `Located Error` to the `Report`. `ImplicitParams` (`?pos :: Pos`) avoids plumbing position through every call.
- **Lenses**: TemplateHaskell `makeLenses` generates lenses for `Settings` and `Context` (microlens-platform). `NoFieldSelectors` is a default extension, so all field access is through lenses.

## Key design patterns & gotchas

- **`Located` Eq/Ord ignores position**: `(x :@ Pos 1 5) == (x :@ Pos 10 20)` is `True`. Tests can use `zeroPos` everywhere.
- **GML type model**: `TBool`, `TInt`, `TInstance`, `TAlpha` are all pattern synonyms for `TReal` (GML VM only has doubles). `TNewtype "sprite"` etc. are also real at runtime but distinct in the checker.
- **`TUnknown [Type]` is a union type** (join-semilattice), NOT an inference variable. `TAny = TUnknown []` is the top type.
- **`parseFile` swallows parse errors**: prints to stdout and returns `[]` — partial file failures are silent.
- **Semicolons are optional** after every statement. `{ }` and `begin end` are interchangeable for blocks.
- **Event filenames** like `Create_0.gml` are parsed to `Event` via `Read` in `Project.hs` — changing event constructors breaks project loading.
- **`Preprocess.hs` is a stub** — macro collection currently happens ad-hoc in `Checker.collectMacros`.
- **Lexer test bug**: `numbers` tests are defined but never run (the `test` function only calls `strings`).
- **`Parser/Token.hs` duplicates `Parser/Lexer.hs`** (maintenance hazard — reserved word lists and symbol lists are manually kept in sync).

## Language extensions (library)

`BlockArguments`, `LambdaCase`, `NamedFieldPuns`, `NoFieldSelectors`, `OverloadedRecordDot`, `OverloadedStrings`, `TupleSections` are all default-extensions in `src/`. Tests only have `OverloadedStrings`.

## Style notes

- Module names use the `Language.GML.*` hierarchy.
- GHC flag `-W` (all warnings) is on for the library. Code must compile warning-free.
