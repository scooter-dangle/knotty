# Tasks: Height-Precalculated Strand Placement (Placement Mode)

**Input**: Design documents from `/specs/007-strand-height-precalc/`

**Prerequisites**: [plan.md](./plan.md), [spec.md](./spec.md), [research.md](./research.md), [data-model.md](./data-model.md), [contracts/public-api.md](./contracts/public-api.md), [contracts/strand-heights.md](./contracts/strand-heights.md), [quickstart.md](./quickstart.md), [fixtures/](./fixtures/)

**Regenerated** 2026-09-03 against `origin/main` @ `37b7c09`, replacing the
pre-rebase task list.

**Tests**: Test tasks ARE included. Constitution Principle III (Test-First) is
binding: new behavior in `src/` requires `#[test]` coverage, and a new diagram
operation requires `insta` snapshot tests specifically.

**Two kinds of test, deliberately**: correctness is asserted against **golden
fixtures** — five owner-supplied input/output pairs covering 63 features, in
[fixtures/](./fixtures/) — because an `insta` snapshot only records what the code
produced. Snapshots sit on top for regression coverage. Where a task says
*fixture*, the expected value comes from the supplied samples, never from running
the implementation.

> ⚠️ **The five fixtures are necessary but not sufficient for Component A.** The
> natural-looking rule "a pair's gap equals the count of strands opened between
> it" matches all 23 pairs in all five and is **wrong**. T014 carries the three
> cases that catch it. See research R2.

## Organization

Organized around the **two independently-implementable components**, which is
the feature's real seam, rather than by user story:

```text
Phase 2  Foundational ── PlacementMode plumbing + grid/emitter extraction
              │
              ├──▶ Phase 3  Component A: encoding → per-strand heights
              │                                     │
              ├──▶ Phase 4  Component B: encoding + heights → grid
              │                                     │
              └─────────────▶ Phase 5  Integration: wire A → B ◀──┘
                                  │
                    Phases 6–9   US1 / US2 / US3 / US4 acceptance
                                  │
                    Phase 10      Polish
```

**Phases 3 and 4 are fully parallel.** Component B is built and tested against
fixture-supplied heights, never against Component A's output, so neither half
blocks the other and a defect in one cannot mask a defect in the other. They meet
at [contracts/strand-heights.md](./contracts/strand-heights.md); T032 proves it.

Both components are required for any user-visible behavior, so the user stories
are **acceptance phases**, not implementation phases. There is no shippable
increment before Phase 5.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files or test functions, no dependencies)
- **[Story]**: Which user story this task serves (US1–US4); component and
  foundational tasks carry none
- Include exact file paths in descriptions

## Path Conventions

Single Rust library crate. Feature code in `src/`; tests in the inline
`#[cfg(test)] mod tests` of `src/raw_lines.rs` and in `src/diagram/{test,tests}.rs`;
`insta` snapshots in `src/snapshots/` and `src/diagram/snapshots/`.

**Constitution gate on every implementation task**: `cargo check --target
wasm32-unknown-unknown` must pass before the task is done (Principle II,
NON-NEGOTIABLE); no `Cargo.toml` entry may be added (Principle V).

---

## Phase 1: Setup

**Purpose**: Establish the pre-change baseline that SC-004 is measured against.

- [ ] T001 Confirm toolchain and both targets build from repo root: `cargo build` and `cargo check --target wasm32-unknown-unknown`, matching the channel in `rust-toolchain.toml`
- [ ] T002 Capture the green baseline: run `cargo test`, and record the pass count and the inventory of the **24** existing snapshot files under `src/snapshots/` and `src/diagram/snapshots/` in the PR description

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Introduce `PlacementMode` as an operating context, and separate
glyph emission from placement so both modes share one emitter.

**⚠️ CRITICAL**: no component work begins until T011 passes.

- [ ] T003 Define `pub enum PlacementMode { #[default] IndexAligned, PrecalculatedHeights }` with `#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]` in `src/diagram.rs`, per research R1 and data-model.md
- [ ] T004 Convert `pub struct AbbreviatedDiagram(pub(crate) Vec<AbbreviatedItem>)` at `src/diagram.rs:115` to the named struct `AbbreviatedDiagram { items: Vec<AbbreviatedItem>, mode: PlacementMode }`, performing the mechanical `self.0` → `self.items` rename across all 37 sites in `src/diagram.rs` (depends on T003)
- [ ] T005 Update `knot.0.iter()` in `VerboseDiagram::from_abbreviated` at `src/diagram.rs:123` to `knot.items.iter()` (depends on T004)
- [ ] T006 Ensure every existing constructor yields `IndexAligned`: `new_from_tuples` (`src/diagram.rs:1464`), the `FromStr` parser, and the `Self::new_from_tuples` call on the rotation output path (depends on T004)
- [ ] T007 Add `mode(&self)`, `set_mode(&mut self, PlacementMode)` and `with_mode(self, PlacementMode) -> Self` to the `AbbreviatedDiagram` impl in `src/diagram.rs`, per contracts/public-api.md (depends on T004)
- [ ] T008 Re-export `PlacementMode` from `src/lib.rs` alongside `AbbreviatedDiagram` and `AbbreviatedItem` (depends on T003)
- [ ] T009 Audit `insta::assert_debug_snapshot!` call sites for any serializing an `AbbreviatedDiagram`, which would gain the new `mode` field; re-accept only those, in `src/diagram/snapshots/` (depends on T004)
- [ ] T010 Extract the grid state (`lines`, `live`) and `column()` out of `OpeningCentered` (`src/raw_lines.rs:8`) into a shared inner struct, leaving `OpeningCentered` to drive it via `raise_once`/`lower_once`/`append`. Behavior-neutral refactor — no glyph output may change. This is what makes FR-014 true by construction and unblocks Component B (research R3)
- [ ] T011 Gate the phase: `cargo test` with all 24 pre-existing snapshots byte-for-byte unchanged versus the T002 baseline, then `cargo check --target wasm32-unknown-unknown` (depends on T004, T005, T006, T007, T008, T009, T010)

**Checkpoint**: `PlacementMode` exists and defaults to `IndexAligned`, no public
signature changed, glyph emission is shared, behavior identical to pre-feature.

---

## Phase 3: Component A — Strand Height Calculation

**Goal**: Given an encoding, compute every strand's height — one more than the
tallest thing ever beneath it (FR-001, research R2).

**Independent Test**: for all five fixtures the computed heights equal the
supplied maxima exactly, and the three regression cases in T014 pass. Needs no
rendering and no Component B.

**Runs fully parallel with Phase 4.**

### Fixtures & Tests for Component A

> Write these FIRST; confirm they fail before T017.

- [ ] T012 Land the five fixtures' per-strand maxima as golden fixtures in a `mod height_fixtures` inside the `#[cfg(test)] mod tests` of `src/raw_lines.rs`, transcribed verbatim from [fixtures/](./fixtures/) with no value derived from running code
- [ ] T013 Add a fixture-driven test asserting computed heights equal the expected maxima for every fixture in T012, in `src/raw_lines.rs` (depends on T012)
- [ ] T014 [P] Add the three discriminating regression cases from [quickstart.md](./quickstart.md) in `src/raw_lines.rs` — `(0 (1 )1 (1 )1 )0` → `(0,3),(1,2),(1,2)` (sequential siblings reuse rows); `(0 (1 (2 )2 (3 )3 )1 )0` → `(0,7),(1,4),(2,3),(5,6)` (sibling stacked above a divergent pair); `(0 (0 )0 (2 )2 )0` → `(2,3),(0,1),(4,5)` (transitive push). **The first is what distinguishes the correct rule from the plausible wrong one**
- [ ] T015 [P] Add edge-case tests in `src/raw_lines.rs` for the empty diagram, a single opening/closing pair, and a deeply nested sequence — each yielding well-formed heights without error (FR-010)
- [ ] T016 [P] Add a determinism test in `src/raw_lines.rs` asserting the same encoding twice yields identical heights (FR-008)

### Implementation for Component A

- [ ] T017 Implement the stack simulation in `src/raw_lines.rs`: walk the sequence maintaining the ordered list of live strands — `(N` inserts a pair at logical index `N`, `)N` removes two, **crossings do not reorder levels** — recording the immediately-below relation among adjacent neighbours after each mutation (research R2) (depends on T010, T013)
- [ ] T018 Implement the height assignment in `src/raw_lines.rs` as a memoized longest path over that relation: `height(s) = 0` when nothing is ever below `s`, else `1 + max(height(t))` over everything below it. Adjacent edges suffice — do not collect the full below-relation (depends on T017)
- [ ] T019 Close the component: `cargo test` with every T012 fixture and T014 regression case passing, then `cargo check --target wasm32-unknown-unknown` (depends on T018)

**Checkpoint**: Component A is correct against the fixtures *and* against the
three cases the fixtures cannot distinguish.

---

## Phase 4: Component B — Render From Precalculated Heights

**Goal**: Given an encoding and per-strand heights, render the grid — flat
strands, caps/cups/crossings at the floored midpoint, boundary transfers, and
crossing convergence with returns (FR-002, FR-003, FR-007, FR-009, FR-011,
FR-015, FR-016).

**Independent Test**: for all five fixtures, rendering the encoding against the
fixture's **supplied** heights reproduces the expected grid exactly.

**Runs fully parallel with Phase 3.**

> **Critical**: every task here takes heights from fixtures, never from Component
> A. That independence is the point of the seam.

### Fixtures & Tests for Component B

> Write these FIRST; confirm they fail before T025.

- [ ] T020 Land the five fixtures' expected grids as golden fixtures in a `mod render_fixtures` inside the `#[cfg(test)] mod tests` of `src/raw_lines.rs`, transcribed verbatim from [fixtures/](./fixtures/)
- [ ] T021 Add a fixture-driven test asserting the render of each fixture's encoding against its supplied heights equals the expected grid exactly (depends on T020)
- [ ] T022 [P] Add an invariant test in `src/raw_lines.rs` asserting no crossing glyph is ever emitted between non-adjacent rows, over every fixture (FR-011, C6)
- [ ] T023 [P] Add a transfer-counting helper in `src/raw_lines.rs` that counts **per glyph** and classifies each transfer as open/close displacement, boundary, or crossing-alignment, so SC-002's three categories are measurable
- [ ] T024 [P] Add a test in `src/raw_lines.rs` asserting the rendered grid height equals `max(all heights) + 1` for every fixture — **not** `AbbreviatedDiagram::height()`, which under-counts whenever a pair diverges (FR-017, research R7)

### Implementation for Component B

- [ ] T025 Implement the placement builder in `src/raw_lines.rs` alongside `OpeningCentered`, driving the shared emitter from T010: size the grid as `max(heights) + 1`, open each pair's cap at `floor((lower + upper) / 2)`, and keep placed strands flat (FR-002, FR-003) (depends on T010, T021)
- [ ] T026 Implement the logical-level → rendered-row mapping in `src/raw_lines.rs`: a notation index names a level among currently-live strands, not a grid row, and the two diverge under this mode (see [fixtures/rotated-5_1.md](./fixtures/rotated-5_1.md), where `\2` renders at row 6) (depends on T025)
- [ ] T027 Implement boundary transfers in `src/raw_lines.rs` — each strand moves between a cap or cup and its own height, split as evenly as the separation allows with an odd separation giving the lower strand the extra step. These are the boundary diagonals FR-009 requires be retained (FR-002, FR-009, FR-015, FR-016) (depends on T026)
- [ ] T028 Implement crossing convergence and return in `src/raw_lines.rs`: bring partners together at the floored midpoint, cross, then return **both** to their heights, since a crossing is not a boundary (FR-011) (depends on T027)
- [ ] T029 Confirm `src/render.rs` needs no new `Horiz` glyphs — the fixtures use only the existing eight variants; if a gap is found, add the minimum and record the deviation in `specs/007-strand-height-precalc/research.md` (depends on T028)
- [ ] T030 Close the component: `cargo test` with every T020 fixture passing, then `cargo check --target wasm32-unknown-unknown` (depends on T028, T029)

**Checkpoint**: Component B reproduces all five expected grids from supplied heights.

---

## Phase 5: Integration (A → B)

**Purpose**: Wire the two verified halves together behind the mode dispatch. The
first point at which any user-visible behavior exists.

- [ ] T031 Make `VerboseDiagram::from_abbreviated` at `src/diagram.rs:118` dispatch on `knot.mode()` — `IndexAligned` through the untouched `OpeningCentered` path, `PrecalculatedHeights` computing heights via A and rendering via B — leaving the signature unchanged (depends on T019, T030)
- [ ] T032 Assert the seam in `src/raw_lines.rs`: Component A's computed heights equal the heights each Component B fixture supplies, for every fixture. This is what proves the two independently-built halves meet (depends on T019, T030)
- [ ] T033 Add end-to-end `insta::assert_snapshot!` coverage rendering all five fixture encodings through the full `PrecalculatedHeights` path in `src/diagram/tests.rs`, mirroring the existing `snapshot_ascii_print` style at `src/diagram/tests.rs:117` (depends on T031)
- [ ] T034 Run `cargo insta review` and accept ONLY the new `PrecalculatedHeights` snapshots; any diff on a pre-existing snapshot is an SC-004 regression to fix, not to accept (depends on T033)
- [ ] T035 Gate: `cargo test` and `cargo check --target wasm32-unknown-unknown` (depends on T034)

**Checkpoint**: the feature works end to end.

---

## Phase 6: User Story 1 — Reduced up-and-down strand movement (Priority: P1) 🎯

**Goal**: passing strands run flat (FR-003, FR-004, C2–C4, SC-001).

**Independent Test**: `terrace` in `PrecalculatedHeights` shows no intermediate
vertical movement on strands that previously climbed and descended, with strictly
fewer displacement transfers than its `IndexAligned` render.

- [ ] T036 [P] [US1] Add a test in `src/diagram/tests.rs` asserting the `PrecalculatedHeights` render of `terrace` has strictly fewer **open/close displacement** segments than its `IndexAligned` render, using the T023 classifier (FR-004, SC-001, C4)
- [ ] T037 [P] [US1] Add a test in `src/diagram/tests.rs` asserting every strand whose height is constant between its cap and cup renders with zero intermediate transfer segments (FR-003, C3)
- [ ] T038 [US1] Gate: `cargo test` and `cargo check --target wasm32-unknown-unknown` (depends on T036, T037)

---

## Phase 7: User Story 2 — Stable complexity under repeated rotation (Priority: P2)

**Goal**: rotation scans the mode-aware grid, so reversed-direction transfers no
longer re-encode as extra features (FR-012, C10, SC-006).

**Independent Test**: rotating a diagram whose `IndexAligned` render contains
reversed-direction transfers never raises the feature count above the original,
is strictly lower than the `IndexAligned` rotation, and a four-rotation cycle
preserves the knot.

- [ ] T039 [US2] Make `AbbreviatedDiagram::full_render_lines` at `src/diagram.rs:895` render under `self.mode`, so `try_rotate_90_ccw` (`src/diagram.rs:912`) scans the mode-aware grid, both signatures unchanged (depends on T031)
- [ ] T040 [US2] Carry the active mode onto the diagram rotation constructs in `src/diagram.rs`, so the operating context survives `try_rotate_90_ccw` — reconciling with T006's default-`IndexAligned` construction (depends on T039)
- [ ] T041 [P] [US2] Add a test in `src/diagram/test.rs` near `test_try_rotate_90_ccw_features` at `:224` asserting the scanned feature count after one rotation in `PrecalculatedHeights` is ≤ the original and strictly < the `IndexAligned` rotation, for `terrace`
- [ ] T042 [P] [US2] Add a four-rotation full-cycle test in `src/diagram/test.rs` asserting the count never grows across the cycle and the final diagram represents the same knot
- [ ] T043 [US2] Confirm `scan_row` at `src/rotate.rs:13` needs no change — its regexes match local glyph shapes and its indices come from counters along a scan line, neither row-dependent (research R6). If a change *is* needed, add a regression test in `mod test_scan_row` at `src/rotate.rs:111` per Principle III (depends on T039)
- [ ] T044 [US2] Confirm `DiagramMove::Rotate90CounterClockwise` dispatch in `src/moves.rs` reaches the mode-aware path through `try_apply`/`try_apply_all` without a move-API change (depends on T039)
- [ ] T045 [US2] Gate: `cargo test`, accept new snapshots, `cargo check --target wasm32-unknown-unknown`. **Rotation results differing from `IndexAligned` is expected, not a regression** — only default-mode output is frozen (depends on T039, T040, T043, T044)

---

## Phase 8: User Story 3 — Opt in without changing existing output (Priority: P3)

**Goal**: `IndexAligned` is the default and byte-for-byte unchanged; notation-only
moves are mode-independent; placement is orthogonal to grid mapping (FR-005,
FR-012, FR-013, FR-014, C1, C9, C11, SC-004).

**Independent Test**: a default-constructed diagram reports `IndexAligned` and
renders identically to today.

> **Scheduling**: this phase depends only on Phase 2 and can run at any point
> alongside Phases 3–7. Placed here to keep stories in priority order.

- [ ] T046 [P] [US3] Add a test in `src/diagram/test.rs` asserting `AbbreviatedDiagram::default()`, `new_from_tuples` and `FromStr` all report `mode() == PlacementMode::IndexAligned` (FR-013)
- [ ] T047 [P] [US3] Add a test in `src/diagram/test.rs` asserting a notation-only move — `Swap`, `WrapAround`, `ChangeCrossing`, a Reidemeister move, `Bulge`/`Collapse` — yields identical `to_tuples()` under both modes (C9)
- [ ] T048 [P] [US3] Add a test in `src/diagram/test.rs` asserting `set_mode`/`with_mode` round-trip and that `with_mode(IndexAligned)` renders identically to an untouched diagram (C1)
- [ ] T049 [US3] Confirm the notation-only move implementations in `src/diagram.rs` and `src/moves.rs` read only `items` and never branch on `mode`, correcting any that do (FR-012) (depends on T007)
- [ ] T050 [US3] Verify the free `ascii_print` / `try_ascii_print` / `*_compact` helpers at `src/diagram.rs:1563`–`:1579` still build `IndexAligned` diagrams (depends on T006)
- [ ] T051 [US3] Run the FR-005 / SC-004 gate: `cargo test` with all 24 pre-existing snapshots byte-for-byte identical to the T002 baseline, proving the default placement's behavior is unchanged, plus `cargo check --target wasm32-unknown-unknown` (FR-005, SC-004, C1) (depends on T049, T050)

---

## Phase 9: User Story 4 — Fidelity across all element types (Priority: P4)

**Goal**: crossings connect correct partners end to end on real knots (FR-007,
FR-011, C6).

**Independent Test**: `basket` and `ugly_trefoil` render in
`PrecalculatedHeights` with every crossing connecting the same two strands as in
the `IndexAligned` render.

- [ ] T052 [P] [US4] Add `insta::assert_snapshot!` coverage rendering `basket` and `ugly_trefoil` in `PrecalculatedHeights` in `src/diagram/tests.rs`, alongside the existing fixtures at `:137` and `:152`
- [ ] T053 [P] [US4] Add a test in `src/diagram/tests.rs` asserting each crossing connects the same strand pair as the `IndexAligned` render for both diagrams (FR-007, C6)
- [ ] T054 [P] [US4] Add a test in `src/diagram/tests.rs` asserting no two strands ever occupy the same row under `PrecalculatedHeights`, for every fixture and both named diagrams (US4 acceptance scenario 2)
- [ ] T055 [US4] Gate: `cargo test`, accept new snapshots, `cargo check --target wasm32-unknown-unknown` (depends on T052, T053, T054)

---

## Phase 10: Polish & Cross-Cutting Concerns

- [ ] T056 [P] Add edge-case coverage in `src/diagram/tests.rs` for the empty diagram, a strand whose height equals its cap row, and closings at the bottom row — each rendering without error and equivalent to `IndexAligned` where no divergence exists (FR-010, C8)
- [ ] T057 [P] Add an end-to-end determinism test in `src/diagram/tests.rs` rendering the same diagram twice in `PrecalculatedHeights` (FR-008, C7, SC-005)
- [ ] T058 [P] Add a knot-equivalence check across both modes for every fixture in `src/diagram/tests.rs` (FR-006, C5, SC-003)
- [ ] T059 Record the SC-002 and SC-007 measurements in `specs/007-strand-height-precalc/quickstart.md`: per-example displacement, boundary and crossing-alignment transfer counts **and rendered height against the default's**, for `terrace`, `basket`, `ugly_trefoil` and the five fixtures, making both tradeoffs explicit (SC-002, SC-007)
- [ ] T060 Audit callers that assume `AbbreviatedDiagram::height()` bounds the rendered row count — correct only under `IndexAligned` (FR-017, research R7) — checking `examples/ascii_print.rs` and `examples/knot-so-good/src/main.rs`
- [ ] T061 [P] Optionally expose a mode flag in `examples/ascii_print.rs`, keeping the library the sole owner of the behavior (Principle I)
- [ ] T062 [P] Optionally expose a mode toggle in `examples/knot-so-good/src/main.rs`, with any GUI-only dependency confined to `examples/knot-so-good/Cargo.toml` (Principle V)
- [ ] T063 Walk the seven scenarios in `specs/007-strand-height-precalc/quickstart.md` end to end and confirm each pass condition holds
- [ ] T064 Final constitution gate: `cargo build`, `cargo test`, `cargo check --target wasm32-unknown-unknown`, and confirm `Cargo.toml` gained no dependency (Principles II and V)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (1)**: no dependencies
- **Foundational (2)**: depends on Setup — BLOCKS everything; T010 and T011 are the gates
- **Component A (3)**: depends on T010/T011. Independent of Component B
- **Component B (4)**: depends on T010/T011. Independent of Component A
- **Integration (5)**: depends on BOTH components (T019, T030)
- **US1 (6)**, **US2 (7)**, **US4 (9)**: depend on Integration
- **US3 (8)**: depends on Phase 2 only — can run any time
- **Polish (10)**: depends on the desired acceptance phases

### Critical Path

```text
T001─T002─┬─T003…T009─T010─T011─┬─Phase 3 (A)─┬─Phase 5─┬─Phase 6 (US1)
          │                     └─Phase 4 (B)─┘         ├─Phase 7 (US2)
          │                                             └─Phase 9 (US4)
          └─────────────────────────Phase 8 (US3, unblocked early)
```

Phase 4 is the longer component track — 6 implementation tasks against
Component A's 2 — so it governs the critical path. Start it first if one person
works both.

### Within Each Component

- Fixtures (T012 / T020) before the tests that consume them
- Tests before implementation; confirm they fail first
- A: stack simulation (T017) before longest path (T018)
- B: placement (T025) → level mapping (T026) → boundary transfers (T027) → crossing convergence (T028)

### Parallel Opportunities

- Phase 2 is largely serial: T004 is one mechanical rename across all of `src/diagram.rs`, so T005–T007 and T009 must follow it rather than race it. T010 touches only `src/raw_lines.rs` and can proceed alongside T004
- **Phases 3 and 4 run fully parallel** — the headline opportunity
- **Phase 8 (US3) runs parallel with everything after Phase 2**
- Within phases, all [P] tasks touch distinct test functions

---

## Parallel Example: The Two Components

```bash
# After T011, both tracks start at once:

# Track A:
Task: "T012 Land the five fixtures' maxima in src/raw_lines.rs"
Task: "T014 Add the three discriminating regression cases in src/raw_lines.rs"
Task: "T017/T018 Stack simulation and longest-path heights in src/raw_lines.rs"

# Track B — heights come from fixtures, never from A:
Task: "T020 Land the five fixtures' expected grids in src/raw_lines.rs"
Task: "T025 Placement builder in src/raw_lines.rs"
Task: "T028 Crossing convergence and return in src/raw_lines.rs"
```

Both tracks touch `src/raw_lines.rs`; keep them in separate modules within the
file to avoid merge conflicts.

---

## Implementation Strategy

### There Is No Partial MVP

Both components are required for any user-visible behavior. The first
demonstrable increment is **Phase 5**. Before that, progress is measured by
fixture pass rate per component.

### Recommended Sequence (one person)

1. Phases 1–2 → foundation, plumbing, shared emitter
2. Phase 4 (Component B) → the longer, more intricate track first
3. Phase 3 (Component A) → the shorter track
4. Phase 5 → integrate; T032 proves the halves meet
5. Phases 6–9 → acceptance, US3 whenever convenient
6. Phase 10 → polish

### Recommended Sequence (two people)

1. Both complete Phases 1–2 together, agreeing the T010 extraction
2. Split: one takes Phase 3, the other Phase 4 — neither waits
3. Rejoin at Phase 5; T032 is the integration proof
4. Split the acceptance phases arbitrarily; they are independent

### Risk Notes

**Component A looks easier than it is.** The rule is three lines, but two
plausible formulations were tried and rejected, one of which agreed with all 23
pairs in all five fixtures. T014 is not optional coverage — it is the only thing
in the suite that distinguishes the correct rule from that wrong one.

**Component B's cost is in the transfers, not the features.** In
[non-adjacent-crossing](./fixtures/non-adjacent-crossing.md), 9 of 19 columns are
transfers. Build T027 and T028 against that fixture before generalizing.

**Height can grow.** `height()` under-counts whenever a pair diverges; T024 and
T060 guard the two places that matters.

---

## Notes

- [P] = different files or test functions, no dependencies
- [Story] labels mark acceptance tasks; component and foundational tasks carry none
- **Fixture expected-values must never be generated by running the implementation** — that turns a correctness test back into a snapshot
- Verify tests fail before implementing
- Commit per task or logical group, conventional-commit prefixes (`feat:`, `fix:`, `refactor:`, `test:`, `doc:`)
- Never accept a diff on a pre-existing snapshot — that is an SC-004 regression
- Run `cargo check --target wasm32-unknown-unknown` before marking any implementation task done (Principle II, NON-NEGOTIABLE)
