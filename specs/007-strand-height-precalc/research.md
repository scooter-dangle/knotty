# Phase 0 Research: Height-Precalculated Strand Placement

> **⚠️ R3 and R5 are invalid against current main.** They are written around
> `raw_lines::{append, expand_above, contract_above}`, which PR #42 deleted when
> it retired the split-cell rendering. R1, R2, R4, and R6 survive in substance
> but need their integration points restated against `OpeningCentered`. See the
> **Rebase impact** section at the top of [plan.md](./plan.md).

This document resolves the open design questions for the height-precalculated
rendering mode. The spec's clarifications already settled the product-level
decisions (unconditional max-row placement; rotation-feature semantics; mode as
an operating context defaulting to legacy). The questions below are the
remaining technical unknowns.

## R1. How is the rendering mode represented and threaded?

**Decision**: Introduce `pub enum RenderMode { Legacy, PrecalculatedHeights }`
(`#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]`, `#[default] Legacy`),
and store it as a field on `AbbreviatedDiagram`. Convert the tuple struct
`AbbreviatedDiagram(pub(crate) Vec<AbbreviatedItem>)` to a named struct
`AbbreviatedDiagram { items: Vec<AbbreviatedItem>, mode: RenderMode }`. Add
`mode(&self) -> RenderMode`, `set_mode(&mut self, RenderMode)`, and a
`with_mode(self, RenderMode) -> Self` builder. Re-export `RenderMode` from
`lib.rs`.

**Rationale**:
- Directly models FR-012 ("a single operating context a user selects to work
  in; all operations run under the active mode") and FR-013 (default = legacy).
- Because rendering and rotation are `&self`/`&mut self` methods, reading
  `self.mode` means **no existing public signature changes** — `ascii_print`,
  `try_ascii_print*`, `try_rotate_90_ccw`, `try_apply`, and `try_apply_all` keep
  their current shapes and simply behave per the active mode. With the default
  `Legacy`, existing callers (and the example apps) compile and behave
  identically, satisfying FR-005/SC-004 by construction.
- Rotation is dispatched at runtime through `DiagramMove::Rotate90CounterClockwise`
  inside `try_apply_all`; a field lets that path honor the mode without adding a
  parameter to the move API.

**Alternatives considered**:
- *Const generic `<const PRECALC: bool>`* (mirroring `GRID_BORDERS`): rejected —
  const generics cannot be selected by a runtime `DiagramMove`, so the rotation
  path could not honor a user-chosen mode; also multiplies the already-large set
  of `ascii_print` signatures.
- *Threaded runtime parameter* on each render/rotate/apply call: rejected —
  forces signature changes that ripple into `examples/ascii_print.rs` and
  `examples/knot-so-good`, and makes "work in one mode" the caller's burden on
  every call (easy to get inconsistent), contradicting the operating-context
  model.

**Cost / risk**: The tuple→named-struct change requires a mechanical
`self.0 → self.items` / `knot.0 → knot.items` rename (~40 sites in `diagram.rs`)
and updating constructors (`new_from_tuples`, the `FromStr` parser, rotation's
`Self::new_from_tuples`). Risk is low (compiler-checked). Any
`assert_debug_snapshot!` on an `AbbreviatedDiagram` may shift due to the new
field — audit and re-accept those specific snapshots (the default value keeps
equality between parsed and constructed diagrams).

## R2. How are per-strand maximum rows precalculated?

**Decision**: Add a single linear pass over the abbreviated sequence that
simulates the vertical stack and records, for each opened pair, the maximum
bottom-row it occupies between its `(` and its matching `)`.

Model: maintain an ordered stack of currently-open pairs. Each `(N` inserts a
new pair at logical index `N` (pairs at index ≥ N shift up by 2); each `)N`
removes the pair occupying index `N` (pairs above shift down by 2); crossings
do not change vertical indices. Track each live pair's current bottom index and
fold its running maximum. The result is a map *opening-event → peak row*, which
is the row at which that opening is placed in `PrecalculatedHeights` mode. Total
diagram height is unchanged (`AbbreviatedDiagram::height()` already computes the
max simultaneous depth × 2).

**Rationale**: Matches the unconditional max-row decision (Clarifications
2026-06-18); O(features) and allocation-light (WASM-friendly); reuses the
existing notion of logical index used throughout `raw_lines::append`.

**Alternatives considered**: Cost-aware placement weighing crossing-alignment
against displacement — explicitly out of scope per clarification.

**⚠️ Under review — granularity**: this decision is written *per pair* (one peak
row per opening event). The feature owner describes the calculation as producing
"the starting heights for each strand in each strand pair" — i.e. *per strand*,
two values per opening. These coincide only if a pair's two strands are always
placed adjacent and never diverge before closing, which the notation does not
obviously guarantee. The distinction is material to the shape of the height map,
so it is tracked as the open Shape question in
[contracts/strand-heights.md](./contracts/strand-heights.md) and resolved from
the part-1 golden fixtures (tasks.md T010). Amend this section once fixed.

## R3. How is the grid built so placed strands run flat?

**Decision**: Add a max-height placement path alongside the existing
`raw_lines::{append, expand_above, contract_above}`. The legacy path inserts a
new pair at logical index `idx` and bumps everything above up via `TransferUp`
(and pulls down via `TransferDown` on close). The new path instead:

1. Opens each pair directly at its precalculated peak row (from R2).
2. Keeps a placed strand flat (horizontal `Line`) for its whole lifetime where
   its rendered row already equals its logical requirement — eliminating the
   avoidable up-then-down (reversed-direction) transfers (FR-003, FR-004).
3. Still emits the **boundary** diagonals intrinsic to a pair entering at its
   opening index and leaving at its closing index (FR-009).

Reuse the existing `Horiz` transfer glyphs (`TransferUp*`, `TransferDown*`,
`Opened*`, `Closed*`) — no new glyphs are expected. `render.rs` likely needs no
change; this will be confirmed when snapshots are generated.

**Rationale**: Confines the new logic to the placement layer (`raw_lines.rs` +
the `from_abbreviated` driver in `diagram.rs`), leaving display and scanning
untouched. Exact glyph-by-glyph output is validated via `insta` snapshots per
the Test-First constitution principle rather than hand-specified here.

## R4. How are crossings whose partners are no longer adjacent handled?

**Decision**: At each crossing `\N`/`/N`, the two participating strands must be
on adjacent rendered rows. Under max-height placement they may not be. Detect
the gap and emit a localized **crossing-alignment transfer**: bring the two
crossing strands adjacent immediately before the crossing column and restore
their placement immediately after (FR-007, FR-011). A crossing is never drawn
between non-adjacent rows.

**Rationale**: Preserves the legacy invariant only *locally* (at the crossing),
which is the minimum needed for a valid planar rendering, instead of globally
(which is what forces the avoidable transfers in the first place). These
transfers are scanned during rotation but do **not** increase the scanned
feature count (Clarifications 2026-06-18), so they are compatible with SC-006.

**Open validation point**: The precise alignment construction (how many rows to
move, and the exact glyph sequence) is the highest-uncertainty implementation
area and will be developed test-first with small, targeted snapshot fixtures
(e.g. `basket`, `ugly_trefoil`, and minimal hand-built crossing cases) before
the general path is generalized.

## R5. How is default-mode parity guaranteed?

**Decision**: `RenderMode::Legacy` routes through the existing
`append/expand_above/contract_above` code path unchanged. The CI/test gate is
that **every existing snapshot remains byte-for-byte identical** (SC-004); new
snapshots are added only for `PrecalculatedHeights`. A round-trip/equivalence
check (rotate or re-render) confirms both modes represent the same knot
(SC-003, FR-006).

**Rationale**: The cheapest, strongest guarantee against regression is the
existing snapshot suite plus a parity assertion that legacy output is untouched.

## R6. Rotation stability measurement

**Decision**: Express SC-006 as a test that counts scanned features
(`AbbreviatedDiagram` length, i.e. number of `AbbreviatedItem`s) before and
after rotation in `PrecalculatedHeights` mode, and across a full four-rotation
cycle. Assert: no increase versus the original, and strictly fewer than the
legacy-mode rotation for diagrams whose legacy rendering contains
reversed-direction transfers (e.g. `terrace`). Knot equivalence is asserted via
the existing rotation round-trip behavior.

**Rationale**: Feature count is directly observable from the abbreviated form,
making SC-006 measurable without inspecting glyphs.

## Resolved unknowns

All Technical Context items are resolved; no `NEEDS CLARIFICATION` remains.
