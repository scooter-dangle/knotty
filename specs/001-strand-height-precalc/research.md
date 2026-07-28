# Phase 0 Research: Height-Precalculated Strand Placement

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

**IMPLEMENTED VARIANT (deviation)**: the mode was added as a *second tuple
field* — `AbbreviatedDiagram(Vec<AbbreviatedItem>, RenderMode)` — rather than
converting to a named struct. Sizing the change showed 38 `self.0` access sites
but only one construction site, so the named-struct form would have meant a
38-site mechanical rename for no behavioral gain, against the project rule of
minimal, noise-free commits. `.0` is already the established idiom throughout
`diagram.rs`. Everything else in R1 (default `Legacy`, accessors, unchanged
public signatures) is as designed.

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

**IMPLEMENTED VARIANT (deviation)**: crossing-alignment transfers were *not*
built. Instead, the placement pass validates that every event's two lines land
on adjacent rows and that strand order is preserved; when they do not, the
whole diagram falls back to the legacy placement. This still satisfies FR-011's
hard guarantee — a crossing is never drawn between non-adjacent rows — and
FR-006/FR-007, and crossing-bearing diagrams whose partners *are* adjacent
under max-height placement (e.g. `ugly_trefoil`) render flat today. What is
lost is the flat rendering for diagrams that would need alignment transfers.
See R7 for the boundary this exposed.

## R7. Which diagrams can be drawn flat (discovered during implementation)

**Finding**: a constant per-line row assignment is only faithful when no pair's
two lines are ever separated. Nesting separates them: in `(0 (1 )1 )0` the outer
pair opens at rows 0/1, but the inner pair opening between them pushes the outer
top line to row 3, so the outer pair is no longer adjacent when it must close.
Naive max placement also inverts strand order in cases like
`(0 (2 )2 (0 (0 (0 (0 …`, where a lower strand keeps rising after the strand
above it has closed.

**Decision**: detect both conditions and fall back to legacy for that diagram.

**Consequence**: *stacked* diagrams — exactly the ones with avoidable
up-then-down displacement — render flat (`terrace` drops from 18 transfer
segments to 0). *Nested* diagrams (`donut`, `c_thingy`, `trefoil`, `basket`)
fall back and are byte-identical to legacy. This is the right dividing line for
correctness, but it is coarser than ideal: a mixed diagram loses the benefit on
its stacked parts too.

**Remaining work**: to benefit mixed/nested diagrams, a pair's top line needs to
rise once after opening and descend once before closing (its own inherent
boundary movement, permitted by FR-009) while every *other* strand stays flat.
That requires a per-line transfer mechanism; today's `expand_above`/
`contract_above` move every line at or above an index together.

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
