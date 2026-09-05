# Phase 0 Research: ASCII Print Tool Upgrade

All unknowns below were resolved by reading the current implementation
directly (`examples/ascii_print.rs`, `src/diagram.rs`, `src/raw_lines.rs`,
`src/rotate.rs`) rather than by guessing. Line references are to the state of
`main` at plan time.

## R1 — Where does the CLI rewrite live, and does it touch the library's own defaults?

**Decision**: The entire CLI rewrite (clap, flags, defaults) lives in
`examples/ascii_print.rs`. It does **not** change `AbbreviatedDiagram`'s own
default `PlacementMode` (`IndexAligned`, established by spec
007-strand-height-precalc and already shipped in `src/diagram.rs` /
`src/raw_lines.rs`).

**Rationale**: `examples/ascii_print.rs` today reads
`AbbreviatedDiagram::from_str` (which always sets
`mode: PlacementMode::default()`), then conditionally calls
`knot.set_mode(PlacementMode::PrecalculatedHeights)` only when
`KNOTTY_PRECALC=true` (`examples/ascii_print.rs:27-29`). Flipping the *tool's*
default (FR-002) is exactly this same call made unconditionally unless the
user opts out — a CLI-layer change only. Changing the library's own default
would silently change behavior for every other library consumer (the
`knot-so-good` GUI, any future consumer) and would re-litigate a decision
007 already made and shipped; nothing in this feature's spec asks for that.

**Alternatives considered**: Changing `PlacementMode::default()` itself —
rejected: out of scope, and breaks 007's explicit contract that
`IndexAligned` is the library-wide default.

## R2 — CLI argument-parsing library

**Decision**: `clap` v4 (latest stable resolves to **4.6.6** as of this plan;
re-check for a newer 4.x patch immediately before implementation) with the
`derive` feature, plus `clap_complete` (**4.6.9**, matching major version) for
shell completion generation. Both added as `[dev-dependencies]` in the root
`Cargo.toml`, scoped to `examples/ascii_print.rs` only.

**Rationale**: `derive`, long-only options, and shell completions are
explicit, named requirements in the feature input (FR-010, FR-011, FR-012,
FR-013). `clap_complete` is the official companion crate for generating
completion scripts from a `clap::Command` and is the standard way to satisfy
FR-012. Neither the root library (`src/`) nor `wasm32-unknown-unknown` is
affected: `cargo build --target wasm32-unknown-unknown` does not build
examples unless `--examples` is passed, and neither dependency is reachable
from `src/`.

**Alternatives considered**: `argh` / `pico-args` (too minimal — no built-in
completions, derive support weaker) — rejected, doesn't meet FR-012.
Builder-style `clap` API — rejected by explicit requirement (FR-010 wants
derive).

**Constitution note (V. Minimal Dependencies)**: two new entries in
`Cargo.toml`, justified above and by the explicit feature request; both are
`dev-dependencies` used by exactly one example binary, not by `src/`.

## R3 — What "encoded diagram" and "diagram manipulations" already mean

**Decision**: No new notation. "Encoded diagram" = the existing abbreviated
notation parsed by `AbbreviatedDiagram::from_str` (`src/diagram.rs:778`).
"Diagram manipulations" = the existing `DiagramMoves`/`DiagramMove` sequence
parsed by its own `FromStr` (`src/moves.rs`) and applied via
`knot.try_apply_all(moves)`. Both already support `#`-prefixed comments and
whitespace-delimited tokens via the shared `CommentLines` helper
(`src/moves.rs:5-53`).

**Rationale**: Confirms FR-001 requires no new parsing work — only wiring the
two existing positional inputs through clap instead of bare
`std::env::args()`.

## R4 — Fixed default input/output semantics

**Decision**: Positional argument 1 is the primary diagram input (as today);
positional argument 2 remains the optional manipulations file (as today). A
new `--input-format <encoded|succinct>` option (default `encoded`) tells the
tool how to interpret positional argument 1. A new `--style
<succinct|full-spaced>` option (default `succinct`) selects the output style.
These two options are orthogonal (FR-004/FR-005/FR-006/FR-007).

**Rationale**: Keeps the default invocation shape identical to today's
(`ascii_print <diagram-file> [moves-file]`), satisfying Story 1 with zero new
required flags, while making the succinct-input path (Story 3) opt-in and
explicit rather than auto-detected — auto-detection between two ASCII-ish
text formats is unreliable and would produce confusing silent
misinterpretation on malformed input (violates FR-009).

**Validation rule**: `--input-format succinct` combined with a manipulations
file (positional 2) is rejected with a clear error — manipulations are
notation-level transforms (R3) and succinct text has already been rendered;
there is no notation left to apply them to.

## R5 — SUPERSEDED: reusing the rotation feature's `scan_row` for succinct input

> **This decision was empirically falsified during implementation and is kept
> here, struck through in effect, only so the record is honest. See R10 for
> what replaced it.**

The original plan was to add `AbbreviatedDiagram::try_from_succinct_text`,
mirroring `try_rotate_90_ccw`'s scan loop (`scan_row(cur, prev)`,
bottom-to-top) but without rotation's left-right character reversal, on the
theory that `scan_row` recovers placement-independent notation from rendered
ASCII text.

**What actually happened**: before implementing R7 below, a direct
experiment was run — construct the simplest possible diagram (`unknot`,
notation `(0 )0`), render it with the existing, unchanged
`ascii_print::<false>()`, then feed its lines through `scan_row` bottom-to-top
with **no** character reversal, exactly as R5 proposed. The result was
`[(')', 1), ('(', 0)]` — wrong order *and* a wrong index — against the
original `[('(', 0), (')', 0)]`. `scan_row`'s per-row `closes.into_iter().rev()`
plus the character-reversal `try_rotate_90_ccw` always feeds it are not
incidental preprocessing; the reversal *is* the 90° rotation. Called on
natural (unreversed) text, `scan_row` computes the notation of a **different,
rotated** diagram, not the original one. There is no cheap adjustment (row
order, character order, or output reordering) that turns this into an
identity operation — rotating is not its own inverse in one step, and the
codebase has never had a reason to compute one.

Both **R6** (a placement-mode marker) and **R7** (collapsing dead column runs
to keep `scan_row`'s spacing assumptions intact) were designed to shore up
this now-abandoned approach and are superseded along with it — R10 needs
neither.

**Lesson kept for future research sections**: a component "already exercised
by 35 unit tests" (as R5 argued) is only evidence for the transform it's
actually tested against (rotation), not for a *different* transform that
happens to reuse the same function signature. The fix here was to verify the
actual hypothesis (does this call sequence recover the *original* notation?)
directly, before building anything on top of it — see R10.

## R10 — What actually solves succinct round-tripping: `VerboseDiagram`'s existing lossless grid text

**Key finding, verified by reading `src/render.rs:238-305`**: the library
already has a complete, independent, lossless, already-tested text
round-trip for the *rendered grid* — not the notation — that R5's
investigation missed entirely:

- `VerboseDiagram::to_text(&self) -> String` (`src/render.rs:239-258`) prints
  one byte per grid cell (`Horiz::as_byte`: `.` empty, `_` line, `x`/`y` the
  two crossing types, `(`/`)` open/close, `/`/`\` transfers), one line per
  grid row.
- `impl FromStr for VerboseDiagram` (`src/render.rs:261-305`) parses that
  exact format back via `Horiz::from_byte`, byte-for-byte, already covered by
  existing round-trip tests (`src/render.rs:374-380`, `text_settles_in_one_pass`).
- `VerboseDiagram`, `Horiz`, `VerboseLine`, and
  `AbbreviatedDiagram::from_abbreviated` are all already `pub` and already
  re-exported from `src/lib.rs`.

This means the entire hard problem R5 was trying to solve — recover enough
structure from previously-printed text to re-render fully spaced — is
**already solved in the library, for the grid representation**, and requires
**zero new library code**. The only genuinely new problem is much smaller:
where does this lossless text live relative to the human-facing succinct
diagram, given the two must coexist in one file?

**Decision**: `--style succinct` output is the existing, **entirely
unchanged** `ascii_print_compact` art, followed by one hidden trailer block:
every line of `VerboseDiagram::to_text()` (built from the same
already-placed diagram, via the already-`pub`
`VerboseDiagram::from_abbreviated`), each prefixed with a distinct marker,
`# ascii_print-grid: `. A human sees only the familiar compact picture; the
tool, reading its own succinct output back in (`--input-format succinct`),
collects every `# ascii_print-grid: `-prefixed line, strips the prefix,
rejoins with `\n`, and parses it straight back to a `VerboseDiagram` via the
existing `FromStr`. For `--style full-spaced`, that grid is rendered directly
via the existing `VerboseDiagram::display::<GRID_BORDERS>()` — no notation,
no placement mode, no re-derivation of anything; the exact original grid is
recovered and rendered.

This is strictly *more* faithful than R5's plan promised (byte-for-byte grid
recovery, not "same notation + same mode re-derives the same picture"), and
it requires no change to `ascii_print_compact`, `scan_row`, or any snapshot —
R7 and R9 as originally written are withdrawn along with R5; see below.

**Rationale**:
- Reuses fully-tested, already-`pub` library code instead of adding any.
- `#` as a comment marker is still this codebase's established convention
  (R3's `CommentLines::comment_start`), so the trailer still looks like nothing
  more than trailing comments to a human, and to any other tool that already
  knows to ignore `#`-prefixed lines in this project's text formats.
- Keeps the visible succinct art pixel-identical to today's
  `ascii_print_compact` — Story 1/2 need **no** library change at all, and
  zero snapshot churn (R9's blast-radius concern evaporates).

**Alternatives considered**: R5's `scan_row`-reuse plan — falsified above.
Redefining "succinct" to *be* `to_text()`'s one-byte-per-cell format directly
(dropping the padded ASCII-art look entirely) — rejected: it's more
information-dense but reads as a data dump, not the "easy-to-scan text-mode
diagram" Story 1 asks for, and it would be a visible behavior change for the
`knot-so-good` GUI's existing "compact" toggle and any current users of
`ascii_print_compact`'s output shape. Reworking `ascii_print_compact` to
compact at whole-grid-cell granularity so a from-scratch cell-pattern parser
could read it back — rejected: a real rewrite of tested rendering code for a
benefit (slightly more compact succinct output) the hidden-trailer approach
gets for free, with far more moving parts.

## R11 — Placement mode and moves no longer need special handling for succinct input

Because R10 recovers the exact grid (not notation), several problems R6
existed to solve disappear on their own:

- **No placement-mode marker needed.** The grid trailer already *is* the
  placed picture; there is nothing to re-derive, so nothing to record
  provenance for. R6 is fully withdrawn, not just its mechanism.
- **`--placement` is meaningless for succinct input** — there is no notation
  left to place. Passing it explicitly alongside `--input-format succinct` is
  a validation error (data-model.md), the same way a `moves` file is (R4).
- **`--echo-diagram` is meaningless for succinct input** for the same
  reason — there is no notation to echo, only a grid. Also a validation
  error, correcting data-model.md's earlier (equally mistaken) claim that it
  would "double as a way to convert a succinct file back into an
  encoded-diagram file" — that claim depended on notation reconstruction
  that, per R5, does not exist.

## R12 — Downstream consumers and snapshot blast radius, corrected

Since `ascii_print_compact` and `ascii_print` are now **unchanged** by this
feature (R10 supersedes the R7 change that would have touched them):

- `examples/knot-so-good` needs no changes and sees no behavior difference of
  any kind (R8's original concern no longer applies — there is no change to
  observe).
- **Zero** `insta` snapshots are affected (R9's original 15-file blast radius
  no longer applies). The baseline captured in tasks.md T002 (110 tests
  passing, those 15 snapshot files) should still read 110 passing and those
  15 files byte-for-byte unchanged at the end of implementation — that
  equality is now itself a useful regression check for "the succinct/full-spaced
  styles really are untouched," worth confirming explicitly rather than
  assuming.
