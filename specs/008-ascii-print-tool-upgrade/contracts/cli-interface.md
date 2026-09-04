# Contract: `ascii_print` Command-Line Interface

This is the external interface this feature exposes: the `ascii_print`
example binary's command line. Behavioral guarantees are numbered (C1…) so
tasks and tests can reference them directly.

## Usage

```text
ascii_print [OPTIONS] [DIAGRAM] [MOVES]

Arguments:
  [DIAGRAM]  Path to the diagram input, or "-"/omitted for stdin
  [MOVES]    Path to a diagram-manipulations file to apply before rendering

Options:
      --input-format <FORMAT>   How to interpret DIAGRAM [default: encoded] [possible values: encoded, succinct]
      --style <STYLE>           Output rendering style [default: succinct] [possible values: succinct, full-spaced]
      --placement <MODE>        Strand placement behavior [default: precalculated-heights] [possible values: precalculated-heights, index-aligned]
      --grid-borders            Draw grid borders around the diagram
      --echo-diagram            Also print the resulting notation after rendering
      --completions <SHELL>     Print a shell completion script and exit [possible values: bash, zsh, fish, powershell, elvish]
  -h, --help                    Print help
  -V, --version                 Print version
```

(`-h`/`-V`/`--help`/`--version` are clap's own built-ins, not
project-defined options, so FR-011's "no short options" applies to every
option this feature defines — it does not ask us to disable clap's standard
help/version handling.)

## Behavioral Guarantees

- **C1**: `ascii_print diagram.txt` (no other options) prints the succinct
  style, placed with `precalculated-heights`, exactly as if
  `--input-format encoded --style succinct --placement
  precalculated-heights` had been passed. (Story 1, SC-001)
- **C2**: `ascii_print diagram.txt --style full-spaced` prints the same
  diagram fully spaced, with no other behavior change. (Story 2, SC-002)
- **C3**: `ascii_print diagram.txt moves.txt` applies `moves.txt` to the
  parsed diagram before C1/C2 apply. (FR-001)
- **C4**: `ascii_print succinct.txt --input-format succinct --style
  full-spaced` reconstructs notation from `succinct.txt` (previously produced
  succinct output) and prints it fully spaced, representing the same
  topology and, absent an explicit `--placement` override, the same rendered
  placement as the input. (Story 3, SC-003)
- **C5**: `ascii_print succinct.txt --input-format succinct moves.txt` exits
  non-zero with an error naming the conflict, printing nothing. (R4
  validation rule)
- **C6**: Malformed input in any format (unparseable diagram, unparseable
  moves, unparseable/unrecognized succinct text) exits non-zero with a
  descriptive error on stderr; stdout is empty. (FR-009, SC-006)
- **C7**: `ascii_print --completions zsh` (with or without other arguments)
  prints a valid zsh completion script for this exact CLI to stdout and
  exits zero, performing no diagram processing. (FR-012, SC-005)
- **C8**: `ascii_print --help` lists every option in Options above, each in
  long form only, with its default and, for enum-valued options, every legal
  value. (FR-011, SC-004)
- **C9**: `--grid-borders` and `--echo-diagram` behave exactly as
  `KNOTTY_GRID=true` / `KNOTTY_PRINT_ABBREV=true` do today, with no other
  change to their effect — this feature relocates them from environment
  variables to flags (FR-014), it does not change what they draw or print.
- **C10**: None of the environment variables `KNOTTY_PRECALC`, `KNOTTY_GRID`,
  `KNOTTY_COMPACT`, `KNOTTY_PRINT_ABBREV` are read anymore; behavior is
  controlled entirely by the flags above. Setting them has no effect (this is
  an intentional breaking change from today's tool — see plan.md Risks).

## Exit Codes

- `0` — success (including `--completions`).
- non-zero — any parse failure (C6), any validation rule from
  `data-model.md` (e.g. C5), or a clap usage error (unknown flag, invalid
  enum value, etc., which clap itself reports and exits non-zero for).
