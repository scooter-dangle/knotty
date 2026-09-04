# Phase 1 Data Model: ASCII Print Tool Upgrade

This feature adds no new persistent data or library-level entities beyond one
small, pure addition to `AbbreviatedDiagram`'s API (`try_from_succinct_text`,
R5/R6). Everything else below is the CLI's own argument model
(`examples/ascii_print.rs`) plus the succinct text format's (unchanged
visible, newly-defined trailing metadata) shape.

## CLI Argument Model (`examples/ascii_print.rs`)

A single `clap::Parser`-derived struct, replacing today's positional
`std::env::args()` reads and `std::env::var` checks.

| Field | Shape | Default | Notes |
|---|---|---|---|
| `diagram` | positional, `Option<PathBuf>` (`-` or absent = stdin) | — (required conceptually; `-`/stdin accepted, matching today) | Primary input; interpreted per `--input-format`. |
| `moves` | positional, `Option<PathBuf>` | `None` | Diagram manipulations file. Rejected (clear error) when `--input-format succinct` is set (R4). |
| `--input-format` | enum: `encoded` \| `succinct` | `encoded` | Selects how `diagram` is parsed. |
| `--style` | enum: `succinct` \| `full-spaced` | `succinct` | Selects the output rendering (FR-003/FR-004/FR-005/FR-006). |
| `--placement` | enum: `precalculated-heights` \| `index-aligned` | `precalculated-heights`, unless `--input-format succinct` and the input carries a placement metadata line (R6), in which case that value is the default | Overrides the placement mode used to render (FR-002/FR-003). |
| `--grid-borders` | flag (bool) | `false` | Replaces `KNOTTY_GRID` (FR-014). |
| `--echo-diagram` | flag (bool) | `false` | Replaces `KNOTTY_PRINT_ABBREV`; prints the resulting notation after rendering. Rejected together with `--input-format succinct`, since there is no notation to echo distinct from what was just reconstructed — see Validation Rules. |
| `--completions` | enum: `bash` \| `zsh` \| `fish` \| `powershell` \| `elvish` | — | When present, emits a completion script for the named shell to stdout and exits; no diagram processing occurs (FR-012). |

All options are long-form only; no `short` is declared on any `clap::Arg`
(FR-011). `--input-format`, `--style`, and `--placement` are declared as
derive `ValueEnum`s so clap validates and lists legal values automatically in
`--help`.

### Validation Rules

- `--input-format succinct` + a `moves` positional → error: "diagram
  manipulations require an encoded diagram; succinct input has already been
  rendered."
- `--input-format succinct` + `--placement` **not** explicitly passed and no
  placement metadata line found in the input → falls back to the tool's
  normal default (`precalculated-heights`), per R6.
- Any input (either format) that fails to parse → the underlying parser's
  `Result::Err` message is printed to stderr and the process exits non-zero;
  no partial/garbled diagram is ever printed (FR-009).
- `--completions <shell>` is mutually exclusive with every other option
  (clap `ArgGroup` or manual check) — it only emits the completion script.

## Succinct Text Format (visible shape, unchanged; new trailing line)

The succinct format's diagram art is unchanged in shape from today's
`ascii_print_compact` output — same glyph vocabulary, same rows — except
that a maximal run of columns blank across every row now collapses to two
placeholder columns instead of zero (R7). One optional trailing line is newly
defined:

```text
<diagram art, one row per line, as today>
# placement: precalculated-heights
```

- The metadata line, if present, is always the *last* line and always begins
  with `#`.
- Its value is one of the same two tokens used by `--placement`
  (`precalculated-heights` | `index-aligned`).
- It is written by the tool whenever `--style succinct` is used; it is
  optional on *input* — hand-edited or externally produced succinct text
  without it is still accepted (R6's fallback).
- It does not appear inside the diagram art itself, so it never affects a
  human glancing at the printed rows (Story 1).

## Library Addition

```rust
impl AbbreviatedDiagram {
    /// Reconstructs notation from a previously printed succinct (or
    /// fully-spaced) diagram, by re-running the same glyph-scanning approach
    /// diagram rotation already uses (`scan_row`), row by row, bottom to
    /// top. Returns an error if a row cannot be interpreted.
    pub fn try_from_succinct_text(text: &str) -> Result<Self, String>;
}
```

- Input: the diagram-art portion of succinct text (metadata line, if any,
  stripped by the CLI before calling this).
- Output: an `AbbreviatedDiagram` with `mode: PlacementMode::default()`
  (`IndexAligned`) — matching every other notation-construction path
  (`from_str`, `new_from_tuples`); the CLI is responsible for calling
  `.set_mode(...)` afterward with whatever mode R6 resolved to, exactly as it
  already does for encoded-diagram input.
- No new public enum or struct: this is one pure function alongside
  `try_rotate_90_ccw`, sharing its scan loop shape.

## State / Lifecycle

None — this is a single-shot CLI tool; each invocation is stateless from run
to run. No persistence, no long-lived state transitions.
