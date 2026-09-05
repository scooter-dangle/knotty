# Phase 1 Data Model: ASCII Print Tool Upgrade

**Revised during implementation** — see `research.md` R5/R10/R11 for why.
This feature adds **no new library code at all**. `VerboseDiagram` already
has a complete, lossless, already-tested text round-trip (`to_text()` /
`impl FromStr for VerboseDiagram`, `src/render.rs:238-305`) and is already
`pub` and re-exported from `src/lib.rs`, alongside the already-`pub`
`AbbreviatedDiagram::from_abbreviated`. Everything below is the CLI's own
argument model (`examples/ascii_print.rs`) and the succinct text format's
shape (visible art unchanged; one new hidden trailer).

## CLI Argument Model (`examples/ascii_print.rs`)

A single `clap::Parser`-derived struct, replacing today's positional
`std::env::args()` reads and `std::env::var` checks.

| Field | Shape | Default | Notes |
|---|---|---|---|
| `diagram` | positional, `Option<PathBuf>` (`-` or absent = stdin) | — (required conceptually; `-`/stdin accepted, matching today) | Primary input; interpreted per `--input-format`. |
| `moves` | positional, `Option<PathBuf>` | `None` | Diagram manipulations file. Rejected (clear error) when `--input-format succinct` is set (R4) — there is no notation to apply them to. |
| `--input-format` | enum: `encoded` \| `succinct` | `encoded` | Selects how `diagram` is parsed. |
| `--style` | enum: `succinct` \| `full-spaced` | `succinct` | Selects the output rendering (FR-003/FR-004/FR-005/FR-006). |
| `--placement` | enum: `precalculated-heights` \| `index-aligned` | `precalculated-heights` | Overrides the placement mode used when rendering from **encoded** input (FR-002/FR-003). Rejected (clear error) when `--input-format succinct` is set (R11) — succinct input is already a placed grid, so there is nothing left to place. |
| `--grid-borders` | flag (bool) | `false` | Replaces `KNOTTY_GRID` (FR-014). Applies to both input formats — it only affects how the grid (freshly built or recovered) is drawn. |
| `--echo-diagram` | flag (bool) | `false` | Replaces `KNOTTY_PRINT_ABBREV`; prints the resulting notation after rendering **encoded** input. Rejected (clear error) when `--input-format succinct` is set (R11) — succinct input never produces notation, only a recovered grid. |
| `--completions` | `clap_complete::Shell` (bash/zsh/fish/powershell/elvish — its variants match the contract exactly) | — | When present, emits a completion script for the named shell to stdout and exits; no diagram processing occurs (FR-012). Mutually exclusive with every other option. |

All options are long-form only; no `.short()` is declared on any `clap::Arg`
(FR-011). `--input-format`, `--style`, and `--placement` are declared as
derive `ValueEnum`s so clap validates and lists legal values automatically in
`--help`.

### Validation Rules

- `--input-format succinct` + a `moves` positional → error naming the
  conflict (contract C5).
- `--input-format succinct` + an explicit `--placement` → error: succinct
  input is already a placed grid; there is no notation left to place.
- `--input-format succinct` + `--echo-diagram` → error: succinct input never
  produces notation, only a recovered grid, so there is nothing to echo.
- Succinct input with no `# ascii_print-grid: ` lines found (see below) →
  error: not recognizable as this tool's succinct output (FR-009).
- Any input that fails to parse in any format → the underlying parser's
  `Result::Err` message is printed to stderr and the process exits non-zero;
  no partial/garbled diagram is ever printed (FR-009).
- `--completions <shell>` combined with any other option → error naming the
  conflict (contract C7).

## Succinct Text Format (visible shape unchanged; one new hidden trailer)

The succinct format's visible diagram art is **byte-for-byte identical** to
today's `ascii_print_compact` output — no library change touches it (R10).
One trailer is newly defined, appended after the visible art:

```text
<ascii_print_compact::<GRID_BORDERS> output, exactly as today, one row per line>
# ascii_print-grid: <VerboseDiagram::to_text() row 1>
# ascii_print-grid: <VerboseDiagram::to_text() row 2>
...
# ascii_print-grid: <VerboseDiagram::to_text() row N>
```

- Every trailer line begins with the literal marker `# ascii_print-grid: `,
  chosen to be distinct from any comment a human might type by hand, while
  still looking like an ordinary `#`-prefixed comment to a casual reader —
  consistent with this codebase's existing comment convention
  (`CommentLines::comment_start`, R3).
- Each trailer line's content, after stripping the marker, is exactly one
  line of `VerboseDiagram::to_text()`'s existing output (one byte per grid
  cell — `.` empty, `_` line, `x`/`y` crossings, `(`/`)` open/close, `/`/`\`
  transfers) — a format that already exists, is already `pub`, and already
  round-trips losslessly (`src/render.rs`).
- The trailer is **not** optional on output: it is written every time
  `--style succinct` is used, since it is the only thing that makes
  `--input-format succinct` possible at all.
- On input, the tool collects every `# ascii_print-grid: `-prefixed line (in
  order), strips the marker, rejoins with `\n`, and parses the result with
  `VerboseDiagram`'s existing `FromStr`. Every other line (the visible art,
  or any unrelated `#` comment) is ignored when reading succinct input — the
  visible art is for human eyes only; the trailer is authoritative.
- No placement-mode marker is needed (R11): the recovered `VerboseDiagram`
  *is* the exact placed grid, not notation to be re-placed.

## No Library Addition

Superseding the original plan (R5, since falsified — see research.md): there
is no new function, enum, or struct in `src/`. The CLI calls only
already-`pub` library API:

- `AbbreviatedDiagram::from_abbreviated` — unchanged, already used today.
- `VerboseDiagram::to_text()` — to build the trailer on output.
- `str::parse::<VerboseDiagram>()` (`FromStr`) — to recover the grid from a
  succinct input's trailer.
- `VerboseDiagram::display::<GRID_BORDERS>()` — to render a recovered grid
  fully spaced.
- `AbbreviatedDiagram::ascii_print_compact::<GRID_BORDERS>()` /
  `::ascii_print::<GRID_BORDERS>()` — unchanged, for encoded-input rendering
  exactly as today.

The one small piece of genuinely new logic — replicating
`ascii_print_compact`'s existing blank-column-stripping algorithm so
`--style succinct` can also be requested when `--input-format succinct` (no
`AbbreviatedDiagram` is available in that path, only a bare
`VerboseDiagram`) — is a small, self-contained helper in
`examples/ascii_print.rs` operating on `Vec<String>` lines, not a library
change; see `contracts/succinct-round-trip.md`.

## State / Lifecycle

None — this is a single-shot CLI tool; each invocation is stateless from run
to run. No persistence, no long-lived state transitions.
