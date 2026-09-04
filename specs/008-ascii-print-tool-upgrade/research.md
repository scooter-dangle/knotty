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

## R5 — Reusing the rotation feature's text→notation scanner for succinct input

This is the substantial technical question behind FR-007/FR-008: turning a
previously-printed succinct diagram back into a fully-spaced rendering.

**Key finding**: The codebase already solves an equivalent problem. Diagram
rotation (`AbbreviatedDiagram::try_rotate_90_ccw`, `src/diagram.rs:974-994`)
already reconstructs notation from *rendered ASCII text* — it renders the
diagram fully-spaced (`full_render_lines`), then feeds each row through
`scan_row` (`src/rotate.rs:13-108`), which recovers `(element, index)` pairs
by matching **local glyph shapes** (e.g. `/_*\`, ` _+ `) against the current
and previous row — not by trusting any global row/column position. Its own
doc comment and 007's plan (`specs/007-strand-height-precalc/plan.md`, Risks
table) both confirm this was designed to be placement-mode-independent by
construction.

**Decision**: Add `AbbreviatedDiagram::try_from_succinct_text(text: &str) ->
Result<Self, String>`, mirroring `try_rotate_90_ccw`'s scan loop exactly
(bottom-to-top, `scan_row(cur, prev)`, `Self::new_from_tuples(out)`) but
**without** rotation's left-right character reversal — this function
reconstructs notation in place, it doesn't rotate anything.

Because `scan_row` recovers pure notation (a strand's index among
currently-live strands), never a rendered row position, the placement used to
originally draw the succinct input is irrelevant to what notation comes back
out. **Strand placement fidelity (FR-008) is achieved by re-deriving it, not
by recovering it**: re-rendering the recovered notation under the same
`PlacementMode` that produced the input reproduces the identical rendering,
because placement is a pure function of (notation, mode).

**Open sub-problem — which mode to re-render with**: see R6.

**Alternatives considered**: Inventing a wholly new succinct-text parser from
scratch — rejected, `scan_row` is already exercised by 35 unit tests
(`src/rotate.rs`) across two real fixtures and is the proven tool for exactly
this "ASCII text → notation" direction; reusing it is both less code and
lower risk than a parallel implementation.

## R6 — Recording which placement mode a succinct diagram was drawn with

**Problem**: `scan_row` intentionally discards row/placement information, so
nothing in the succinct ASCII art itself says whether it was drawn
`IndexAligned` or `PrecalculatedHeights`. Without that, expanding succinct
text with the *wrong* mode reproduces the right topology but the *wrong*
rendered placement, violating the spec's "not... a different strategy"
assumption whenever the input wasn't produced under the tool's current
default.

**Decision**: When printing succinct output, the tool appends one trailing
`#`-prefixed metadata line recording the placement mode used, e.g.
`# placement: precalculated-heights`. When reading succinct input, the tool
looks for that trailing line and uses it as the default for `--placement`
(still overridable by explicitly passing `--placement` on the command line).
Absence of the line (e.g. hand-edited or externally produced input) falls
back to the tool's normal default (R1).

**Rationale**: `#` as a comment/metadata marker is already this codebase's
established convention (`CommentLines::comment_start`, used by both the
notation and moves formats — R3), so this doesn't introduce a new idea, just
extends the existing one to the succinct format. It sits on its own trailing
line, so it never touches the visible diagram art a human reads (Story 1's
"easy-to-scan" requirement is unaffected).

**Alternatives considered**: Always re-render with the tool's current
default regardless of provenance — rejected, silently produces the wrong
placement for any succinct file not produced under today's default (e.g. one
generated with `--placement index-aligned`), which is exactly the failure
mode FR-008 exists to prevent. Requiring the user to always pass
`--placement` explicitly when expanding succinct input — rejected, needlessly
burdens the common case where the mode matches the tool's own default anyway
(Story 3's "single command" success criterion, SC-003).

## R7 — Making the succinct format safely re-parseable (`scan_row` spacing assumptions)

**Problem**: Today's `try_ascii_print_compact` (`src/diagram.rs:1597-1627`)
deletes every column that is blank (`" "` or `"_"`) across *every* row,
collapsing a run of N such columns to zero. `scan_row` matches some patterns
with variable-width regexes (`/_*\`, ` _+ `) that tolerate this fine, but one
branch checks a literal two-space run (`cur_tail.starts_with("  ")`,
`src/rotate.rs:49`) to look up context on the row below — a run collapsed to
zero (or one) column would no longer contain a literal two-character match at
the expected offset.

**Invariant confirmed**: `Grid::column` (`src/raw_lines.rs:126-139`) only
changes a row's character in response to an event at that column (an entry in
`glyphs`); with no event, a row emits `Line` (`_`) if live or `Empty` (` `)
if not, unchanged from the previous column. So within one maximal run of
"blank-in-every-row" columns, **each individual row's character is constant**
for the whole run (it is always `_` for that run, or always ` `) — nothing
about the run's *length* carries information, only its *presence* and, per
row, which of the two characters it is.

**Decision**: Change `try_ascii_print_compact` to collapse each maximal
all-blank run to **exactly two** placeholder columns (each row keeping its
own constant character from that run), instead of deleting it outright. Reuse
`scan_row` completely unmodified against succinct text.

**Rationale**: Per-row content is provably unchanged by collapsing (see
invariant above) — only the exact original width is lost, which the spec's
own assumption already permits (topology and placement must round-trip
exactly; pixel-for-pixel width does not). Collapsing to exactly two columns
(rather than one) preserves every literal fixed-width match `scan_row`
depends on today, including the two-space case, with zero changes to
`src/rotate.rs` — the lowest-risk option that still keeps the vast majority
of the compaction's visual benefit (a run of tens of columns still collapses
to two).

**Alternatives considered**: Collapsing to exactly one column — rejected,
breaks the exact `"  "` check in `scan_row` for diagrams whose blank run was
otherwise exactly the width that check depends on, and finding/fixing every
place that assumption is load-bearing is unnecessary risk for one column of
extra compactness. Encoding exact run-lengths as inline digits — rejected,
turns the visible diagram art into something a human can no longer scan at a
glance, defeating Story 1's purpose. A separate side-channel run-length table
— rejected as unneeded complexity: the spec does not require byte-identical
width recovery, only faithful topology and placement.

**Residual risk (carried into implementation)**: `src/rotate.rs` currently
has 35 unit tests built against fully-spaced fixtures. Before relying on
`scan_row` unmodified for succinct input, add unit tests that feed it
collapsed-to-two-column input directly (not just full-width input) to confirm
no pattern was implicitly assuming *more* than two columns of padding
anywhere. This is a testing task, not an open design question.

## R8 — Downstream consumers of `ascii_print_compact`'s exact output

**Finding**: `examples/knot-so-good` (a separate crate, spec
006-gui-makeover) calls `try_ascii_print_compact::<false>()` purely to render
a `<pre>` block in its Yew UI (`examples/knot-so-good/src/main.rs:421-422`).
Its only related test asserts `full != compact`
(`examples/knot-so-good/src/tests.rs:209-219`), which remains true under R7's
change.

**Decision**: No changes needed in `knot-so-good`. Its rendered compact view
will show two-column gaps instead of none — a cosmetic difference, not a
functional regression — and is out of scope for this feature.

## R9 — Snapshot blast radius

**Finding**: `ascii_print_compact` output is asserted in `insta` snapshots at
`src/diagram/snapshots/knotty__diagram__tests__snapshot_ascii_print*.snap`
(8 files) and `..._precalculated_heights*.snap`/`..._with_crossings*.snap` (7
files) — 15 snapshots total, driven from `src/diagram/tests.rs:175-177,
307-309, 361-363`.

**Decision**: R7's change to `try_ascii_print_compact` will change all 15
snapshots that contain at least one collapsible blank run (most will). This
is expected, mechanical fallout from a deliberate, spec-mandated behavior
change, not a regression — per the constitution's Test-First workflow, these
are regenerated with `cargo insta review` and the diffs eyeballed as part of
implementation, same as any other intentional rendering change.
