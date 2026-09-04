# Tasks: Height-Precalculated Strand Placement (Rendering Mode)

**Input**: Design documents from `/specs/007-strand-height-precalc/`

**Prerequisites**: [plan.md](./plan.md), [spec.md](./spec.md), [research.md](./research.md), [data-model.md](./data-model.md), [contracts/public-api.md](./contracts/public-api.md), [contracts/strand-heights.md](./contracts/strand-heights.md), [quickstart.md](./quickstart.md)

> **⚠️ SUPERSEDED — regenerate with `/speckit-tasks` before implementing.**
>
> [plan.md](./plan.md), [research.md](./research.md),
> [data-model.md](./data-model.md) and [contracts/](./contracts/) were replanned
> on 2026-09-03 against current `main`. The tasks below still predate that and
> are wrong in specifics: they name `append`/`expand_above`/`contract_above`
> (deleted by #42), stale `src/raw_lines.rs:NNN` line numbers,
> `RenderMode::Legacy` (now `PlacementMode::IndexAligned`), and a 16-snapshot
> baseline (now 24). They also predate the midpoint rules, the two-pass height
> calculation, the height-growth finding, and the five golden fixtures.
>
> The **two-component structure survives** — Phases 3 and 4 still map to
> Components A and B, and the fixtures now make T012/T018 mechanical rather than
> blocked. T010's `(depends on T012)` is void: the per-strand/per-pair question
> it was waiting on was settled by clarification, not by fixtures.

**Tests**: Test tasks ARE included. Constitution Principle III (Test-First) is
binding: new behavior in `src/` requires `#[test]` coverage, and a new diagram
operation requires `insta` snapshot tests specifically.

**Two kinds of test, deliberately**: correctness is asserted against
**golden fixtures** — known-correct inputs and outputs supplied by the feature
owner — because an `insta` snapshot only records what the code produced and asks
a human to bless it. Snapshots are retained on top of the fixtures for
regression coverage, satisfying Principle III. Where a task says *fixture*, the
expected value must come from the supplied samples, never from running the
implementation.

## Organization

Tasks are organized around the **two independently-implementable components**,
which is the feature's real implementation seam, rather than by user story:

```text
Phase 2  Foundational ── RenderMode plumbing + the seam type
              │
              ├──▶ Phase 3  Component A: encoding → strand heights
              │                                      │
              ├──▶ Phase 4  Component B: encoding + heights → rendered grid
              │                                      │
              └──▶ Phase 7  US3 (opt-in / no regression) — independent
                                                     │
                    Phase 5  Integration: wire A → B ┘
                                  │
                    Phases 6,8,9  US1 / US2 / US4 acceptance validation
                                  │
                    Phase 10      Polish
```

**Phases 3 and 4 are fully parallel.** Component B is built and tested against
fixture-supplied heights, never against Component A's output, so neither half
blocks the other and a defect in one cannot mask a defect in the other. They
meet at [contracts/strand-heights.md](./contracts/strand-heights.md), and T028
proves they actually meet.

Because both components are necessary for any user-visible behavior, the user
stories are **validation phases** here, not implementation phases — their
acceptance criteria are asserted once A and B are integrated. There is no
shippable increment before Phase 5.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task validates (US1, US2, US3, US4)
- Component phases carry no story label — like Foundational, they serve all stories
- Include exact file paths in descriptions

## Path Conventions

Single Rust library crate (per plan.md "Structure Decision"). Feature code lands
in `src/`; tests live in the inline `#[cfg(test)] mod tests` of `src/raw_lines.rs`
and in `src/diagram/{test,tests}.rs`; `insta` snapshots land in `src/snapshots/`
and `src/diagram/snapshots/`.

**Constitution gate on every implementation task**: `cargo check --target
wasm32-unknown-unknown` must pass before the task is done (Principle II,
NON-NEGOTIABLE), and no `Cargo.toml` entry may be added (Principle V).

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Establish the pre-change baseline that SC-004 ("byte-for-byte
identical default output") is measured against.

- [ ] T001 Confirm the pinned toolchain and both targets build from repo root: run `cargo build` and `cargo check --target wasm32-unknown-unknown`, matching the channel in `rust-toolchain.toml`
- [ ] T002 Capture the green baseline: run `cargo test` and record the current pass count and the inventory of the 16 existing snapshot files under `src/snapshots/` and `src/diagram/snapshots/` in the PR description, so any later drift is detectable

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Introduce `RenderMode`, thread it onto `AbbreviatedDiagram` as an
operating context, and define the seam type both components code against.

**⚠️ CRITICAL**: No component work can begin until T010 fixes the seam type.

- [ ] T003 Define `pub enum RenderMode { #[default] Legacy, PrecalculatedHeights }` with `#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]` in `src/diagram.rs`, per research R1 and data-model.md
- [ ] T004 Convert `pub struct AbbreviatedDiagram(pub(crate) Vec<AbbreviatedItem>)` at `src/diagram.rs:115` into the named struct `AbbreviatedDiagram { items: Vec<AbbreviatedItem>, mode: RenderMode }`, performing the mechanical `self.0` → `self.items` rename across all ~37 sites in `src/diagram.rs` (depends on T003)
- [ ] T005 Update the remaining tuple-field readers outside the struct's own impl — `knot.0.iter()` in `VerboseDiagram::from_abbreviated` at `src/diagram.rs:123` — to `knot.items.iter()` (depends on T004)
- [ ] T006 Ensure every existing constructor yields `mode = RenderMode::Legacy`: `new_from_tuples` (`src/diagram.rs:1464`), the `FromStr` parser, and the `Self::new_from_tuples` call on the rotation output path in `src/diagram.rs` (depends on T004)
- [ ] T007 Add the accessors `mode(&self) -> RenderMode`, `set_mode(&mut self, RenderMode)`, and the `with_mode(self, RenderMode) -> Self` builder to the `AbbreviatedDiagram` impl in `src/diagram.rs`, per contracts/public-api.md (depends on T004)
- [ ] T008 Re-export `RenderMode` from `src/lib.rs` alongside `AbbreviatedDiagram` and `AbbreviatedItem` (depends on T003)
- [ ] T009 Audit `insta::assert_debug_snapshot!` call sites for any that serialize an `AbbreviatedDiagram` and would gain the new `mode` field (research R1 cost note); re-accept only those, in `src/diagram/snapshots/` (depends on T004)
- [ ] T010 **Resolve the open Shape question and define the seam type.** Read the Component A fixtures to determine whether heights are per-pair or per-strand, then define the internal height-map type in `src/raw_lines.rs`, record the resolution in `specs/007-strand-height-precalc/contracts/strand-heights.md`, and amend research R2 in `specs/007-strand-height-precalc/research.md` to match. **This unblocks Phases 3 and 4 simultaneously** (depends on T012)
- [ ] T011 Gate the phase: run `cargo test` and confirm every pre-existing snapshot is byte-for-byte unchanged versus the T002 baseline, then run `cargo check --target wasm32-unknown-unknown` (depends on T004, T005, T006, T007, T008, T009, T010)

**Checkpoint**: `RenderMode` exists and defaults to `Legacy`, no public signature
changed, behavior identical to pre-feature, and the seam type is fixed. Both
component tracks can now start in parallel.

---

## Phase 3: Component A — Strand Height Calculation

**Goal**: Given a diagram encoding, calculate the starting height of every
strand (FR-001, FR-008, FR-010; contracts/strand-heights.md "Component A").

**Independent Test**: For every supplied part-1 sample, the calculated heights
equal the sample's expected heights exactly. Requires no rendering and no
Component B.

**Runs fully parallel with Phase 4.**

### Fixtures & Tests for Component A

> **Write these FIRST and confirm they fail before T015.**

- [ ] T012 Land the supplied part-1 samples as golden fixtures — encoding → expected heights — in a `mod height_fixtures` inside the `#[cfg(test)] mod tests` of `src/raw_lines.rs`, transcribed verbatim from the feature owner's samples with no value derived from running code
- [ ] T013 Add a fixture-driven test asserting the calculation's output equals the expected heights for every fixture in T012, in the `#[cfg(test)] mod tests` of `src/raw_lines.rs` (depends on T012)
- [ ] T014 [P] Add edge-case tests for the empty diagram, a single opening/closing pair with nothing opening beneath it, and a deeply nested opening sequence — each producing a well-formed height map without error (FR-010) in the `#[cfg(test)] mod tests` of `src/raw_lines.rs`
- [ ] T015 [P] Add a determinism test asserting the calculation over the same encoding twice yields identical output (FR-008) in the `#[cfg(test)] mod tests` of `src/raw_lines.rs`

### Implementation for Component A

- [ ] T016 Implement the height calculation in `src/raw_lines.rs` as a single linear walk of the abbreviated sequence maintaining an ordered stack of open pairs (each `(N` inserts at logical index `N` shifting entries ≥ N up by 2; each `)N` removes the entry at `N` shifting above down by 2; crossings do not shift), folding each live strand's running maximum into the seam type from T010 (research R2) (depends on T010, T013)
- [ ] T017 Close the component: run `cargo test` confirming every T012 fixture passes, and run `cargo check --target wasm32-unknown-unknown` (depends on T016)

**Checkpoint**: Component A is correct against the supplied samples, standalone.

---

## Phase 4: Component B — Render From Precalculated Heights

**Goal**: Given an encoding and a height map, render the full grid — flat
strands, boundary diagonals, and the crossing-alignment transfers needed once
crossing partners are no longer adjacent by default (FR-002, FR-003, FR-007,
FR-009, FR-011; contracts/strand-heights.md "Component B").

**Independent Test**: For every supplied part-2 sample, rendering the encoding
against the sample's **supplied** heights produces the expected ASCII exactly.
Requires no Component A.

**Runs fully parallel with Phase 3.**

> **Critical**: every task in this phase takes heights from fixtures, never from
> Component A. That independence is the whole point of the seam — honoring it is
> what keeps a bug in A from masking or manufacturing a bug in B.

### Fixtures & Tests for Component B

> **Write these FIRST and confirm they fail before T022.**

- [ ] T018 Land the supplied part-2 samples as golden fixtures — encoding + supplied heights → expected rendered ASCII — in a `mod render_fixtures` inside the `#[cfg(test)] mod tests` of `src/raw_lines.rs`, including at least one case where a crossing's partners are non-adjacent under the supplied heights (research R4)
- [ ] T019 Add a fixture-driven test asserting the render of each T018 fixture's encoding against its supplied heights equals the expected ASCII exactly (depends on T018)
- [ ] T020 [P] Add an invariant test asserting no crossing glyph pair is ever emitted between non-adjacent rows, over every T018 fixture (FR-011, C6) in the `#[cfg(test)] mod tests` of `src/raw_lines.rs`
- [ ] T021 [P] Add a transfer-counting helper that classifies each emitted transfer as open/close displacement versus crossing-alignment, in the `#[cfg(test)] mod tests` of `src/raw_lines.rs`, so FR-004 and SC-002 are measurable with the two counts tracked separately

### Implementation for Component B

- [ ] T022 Implement placement from supplied heights in `src/raw_lines.rs` alongside the existing `append`/`expand_above`/`contract_above` (`src/raw_lines.rs:135`, `:21`, `:74`): open each strand at its supplied height, keep placed strands flat for their lifetime, and emit the boundary diagonals intrinsic to entering at the opening index and leaving at the closing index (FR-002, FR-003, FR-009, research R3) (depends on T010, T019)
- [ ] T023 Implement crossing-partner gap detection in `src/raw_lines.rs`: at each `\N`/`/N`, determine whether the two participating strands sit on adjacent rendered rows under the supplied heights (depends on T022)
- [ ] T024 Implement the localized crossing-alignment transfer in `src/raw_lines.rs` — bring the two partners adjacent immediately before the crossing column and restore their placement immediately after, so a crossing is never drawn between non-adjacent rows (FR-007, FR-011, research R4) (depends on T023)
- [ ] T025 Confirm `src/render.rs` needs no new `Horiz` glyphs (research R3 predicts the existing `TransferUp*`/`TransferDown*`/`Opened*`/`Closed*` set suffices); if a gap is found, add the minimum glyph and record the deviation in `specs/007-strand-height-precalc/research.md` (depends on T024)
- [ ] T026 Close the component: run `cargo test` confirming every T018 fixture passes, and run `cargo check --target wasm32-unknown-unknown` (depends on T024, T025)

**Checkpoint**: Component B is correct against the supplied samples, standalone.

---

## Phase 5: Integration (A → B)

**Purpose**: Wire the two verified components together behind the mode dispatch.
This is the first point at which any user-visible behavior exists.

- [ ] T027 Make `VerboseDiagram::from_abbreviated` at `src/diagram.rs:118` dispatch on `knot.mode()` — `Legacy` routes through the untouched `append` path, `PrecalculatedHeights` computes heights via Component A and renders via Component B — leaving the function signature unchanged (depends on T017, T026)
- [ ] T028 Assert the seam: for every encoding appearing in both fixture sets, Component A's calculated heights equal the height map the Component B fixture supplies, in the `#[cfg(test)] mod tests` of `src/raw_lines.rs` — this is what proves the two independently-built halves actually meet (depends on T017, T026)
- [ ] T029 Add end-to-end `insta::assert_snapshot!` coverage rendering `terrace` (`(0 (2 (4 (6 )5 )3 )1 (1 (3 (5 )6 )4 )2 )0`) through the full `PrecalculatedHeights` path in `src/diagram/tests.rs`, mirroring the existing `snapshot_ascii_print` style at `src/diagram/tests.rs:117` (depends on T027)
- [ ] T030 Run `cargo insta review` and accept ONLY new `PrecalculatedHeights` snapshots; any diff on a pre-existing snapshot is an SC-004 regression to fix, not to accept (depends on T029)
- [ ] T031 Run `cargo test` and `cargo check --target wasm32-unknown-unknown` (depends on T030)

**Checkpoint**: The feature works end to end. User story validation can begin.

---

## Phase 6: User Story 1 Validation - Reduced up-and-down strand movement (Priority: P1)

**Goal**: Confirm the integrated path delivers the core value — passing strands
run flat (FR-003, FR-004, C2–C4, SC-001).

**Independent Test**: `terrace` rendered in `PrecalculatedHeights` shows no
intermediate vertical movement on strands that previously climbed and descended,
with strictly fewer transfer diagonals than its `Legacy` render.

- [ ] T032 [P] [US1] Add a test asserting the `PrecalculatedHeights` render of `terrace` has strictly fewer transfer segments than its `Legacy` render, using the T021 classifier (FR-004, SC-001, C4) in `src/diagram/tests.rs`
- [ ] T033 [P] [US1] Add a test asserting that every strand whose supplied height is constant between its opening and closing renders with zero intermediate transfer segments (FR-003, SC-001) in `src/diagram/tests.rs`
- [ ] T034 [US1] Run `cargo test` and `cargo check --target wasm32-unknown-unknown` (depends on T032, T033)

**Checkpoint**: US1 acceptance scenarios pass.

---

## Phase 7: User Story 3 Validation - Opt in without changing existing output (Priority: P3)

**Goal**: Prove `Legacy` is the default and is byte-for-byte unchanged, and that
notation-only moves are mode-independent (FR-005, FR-012, FR-013, C1, C9, SC-004).

**Independent Test**: A default-constructed diagram reports `mode() ==
RenderMode::Legacy` and renders identically to today.

> **Scheduling note**: this phase depends only on Phase 2 and can run at any
> point alongside Phases 3–6. It is placed here to keep stories in priority
> order, not because it is blocked.

- [ ] T035 [P] [US3] Add a test asserting `AbbreviatedDiagram::default()`, `new_from_tuples`, and `FromStr` all report `mode() == RenderMode::Legacy` (FR-013) in `src/diagram/test.rs`
- [ ] T036 [P] [US3] Add a test asserting a notation-only move — `Swap`, `WrapAround`, `ChangeCrossing`, a Reidemeister move, and `Bulge`/`Collapse` — yields identical `to_tuples()` under both modes (C9, US3 acceptance scenario 3) in `src/diagram/test.rs`
- [ ] T037 [P] [US3] Add a test asserting `set_mode` / `with_mode` round-trip and that `with_mode(Legacy)` renders identically to an untouched diagram (C1) in `src/diagram/test.rs`
- [ ] T038 [US3] Confirm the notation-only move implementations in `src/diagram.rs` and `src/moves.rs` read only `items` and never branch on `mode`, correcting any that do (FR-012) (depends on T007)
- [ ] T039 [US3] Verify the free `ascii_print` / `try_ascii_print` / `*_compact` helpers at `src/diagram.rs:1563`–`:1579` still build `Legacy`-mode diagrams, matching the note in contracts/public-api.md (depends on T006)
- [ ] T040 [US3] Run the SC-004 gate: `cargo test` with every pre-existing snapshot byte-for-byte identical to the T002 baseline, plus `cargo check --target wasm32-unknown-unknown` (depends on T038, T039)

**Checkpoint**: Existing consumers and snapshots are provably unaffected.

---

## Phase 8: User Story 2 Validation - Stable complexity under repeated rotation (Priority: P2)

**Goal**: Rotation scans the mode-aware render, so reversed-direction transfers
no longer re-encode as extra features (FR-012, C10, SC-006).

**Independent Test**: Rotating `terrace` with `PrecalculatedHeights` active
never raises the feature count above the original, is strictly lower than the
`Legacy`-mode rotation, and a full four-rotation cycle preserves the knot.

- [ ] T041 [US2] Make `AbbreviatedDiagram::full_render_lines` at `src/diagram.rs:895` render under `self.mode` so `try_rotate_90_ccw` (`src/diagram.rs:912`) scans the mode-aware grid, with both signatures unchanged (depends on T027)
- [ ] T042 [US2] Carry the active mode onto the diagram rotation constructs, so the operating context survives `try_rotate_90_ccw` and repeated rotation stays in one mode (FR-012) — reconciling with T006's default-`Legacy` construction in `src/diagram.rs` (depends on T041)
- [ ] T043 [P] [US2] Add a test asserting the scanned feature count after one `try_rotate_90_ccw` in `PrecalculatedHeights` is ≤ the original and strictly < the `Legacy`-mode rotation for `terrace`, in `src/diagram/test.rs` near the existing `test_try_rotate_90_ccw_features` at `src/diagram/test.rs:224` (research R6)
- [ ] T044 [P] [US2] Add a four-rotation full-cycle test asserting the feature count never grows across the cycle and the final diagram represents the same knot as the original, in `src/diagram/test.rs`
- [ ] T045 [US2] Confirm `scan_row` at `src/rotate.rs:13` needs no change — the crossing-alignment and boundary transfers must scan to no extra features (spec Clarifications 2026-06-18); if it does need one, add a regression test in the `mod test_scan_row` at `src/rotate.rs:111` per Principle III (depends on T041)
- [ ] T046 [US2] Confirm `DiagramMove::Rotate90CounterClockwise` dispatch in `src/moves.rs` reaches the mode-aware path through `try_apply`/`try_apply_all` without a move-API change (research R1) (depends on T041)
- [ ] T047 [US2] Run `cargo test`, accept new snapshots via `cargo insta review`, and run `cargo check --target wasm32-unknown-unknown` (depends on T041, T042, T045, T046)

**Checkpoint**: Repeated rotation no longer inflates the feature count — the motivating use case works.

---

## Phase 9: User Story 4 Validation - Fidelity across all element types (Priority: P4)

**Goal**: Confirm crossings connect correct partners end to end on real knots
(FR-007, FR-011, C6).

**Independent Test**: `basket` and `ugly_trefoil` render in
`PrecalculatedHeights` with every crossing connecting the same two strands as in
the `Legacy` render.

- [ ] T048 [P] [US4] Add `insta::assert_snapshot!` coverage rendering `basket` and `ugly_trefoil` in `PrecalculatedHeights` in `src/diagram/tests.rs`, alongside the existing fixtures at `src/diagram/tests.rs:137` and `:152`
- [ ] T049 [P] [US4] Add a test asserting each crossing connects the same strand pair as the `Legacy` render for both diagrams (FR-007, C6) in `src/diagram/tests.rs`
- [ ] T050 [P] [US4] Add a test asserting deeply nested openings never place two strands on the same row in the new mode (US4 acceptance scenario 2) in `src/diagram/tests.rs`
- [ ] T051 [US4] Run `cargo test`, accept new snapshots via `cargo insta review`, and run `cargo check --target wasm32-unknown-unknown` (depends on T048, T049, T050)

**Checkpoint**: The mode is usable for real knots.

---

## Phase 10: Polish & Cross-Cutting Concerns

- [ ] T052 [P] Add edge-case coverage for the empty diagram, a strand whose opening row already equals its maximum row, and closings at the bottom row — each rendering without error and equivalent to `Legacy` where no avoidable movement exists (FR-010, C8) in `src/diagram/tests.rs`
- [ ] T053 [P] Add an end-to-end determinism test rendering the same diagram twice in `PrecalculatedHeights` (FR-008, C7, SC-005) in `src/diagram/tests.rs`
- [ ] T054 [P] Add a knot-equivalence check across both modes for every fixture in `src/diagram/tests.rs`, confirming the two renders decode to the same knot (FR-006, C5, SC-003)
- [ ] T055 Report the SC-002 measurement: record per-example open/close-displacement and crossing-alignment transfer counts for `terrace`, `basket`, and `ugly_trefoil` in `specs/007-strand-height-precalc/quickstart.md`, making the crossing-heavy tradeoff explicit
- [ ] T056 [P] Optionally expose a mode flag in `examples/ascii_print.rs` for manual inspection, keeping the library the sole owner of the behavior (Principle I)
- [ ] T057 [P] Optionally expose a mode toggle in the mini app at `examples/knot-so-good/src/main.rs`, with any GUI-only dependency confined to `examples/knot-so-good/Cargo.toml` (Principle V)
- [ ] T058 Walk the four scenarios in `specs/007-strand-height-precalc/quickstart.md` end to end and confirm each pass condition holds
- [ ] T059 Final constitution gate: `cargo build`, `cargo test`, `cargo check --target wasm32-unknown-unknown`, and confirm `Cargo.toml` gained no dependency (Principles II and V)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies
- **Foundational (Phase 2)**: Depends on Setup — BLOCKS everything. T010 is the gate that unblocks both components
- **Component A (Phase 3)**: Depends on T010. Independent of Component B
- **Component B (Phase 4)**: Depends on T010. Independent of Component A
- **Integration (Phase 5)**: Depends on BOTH components (T017, T026)
- **US1 (Phase 6)**, **US2 (Phase 8)**, **US4 (Phase 9)**: Depend on Integration
- **US3 (Phase 7)**: Depends on Phase 2 only — can run any time
- **Polish (Phase 10)**: Depends on all desired validation phases

### The Critical Path

```text
T001─T002─┬─T003─T004─T005─T006─T007─T008─T009─┬─T010─┬─Phase 3 (A)─┬─Phase 5─┬─Phase 6
          │                                    │      └─Phase 4 (B)─┘         ├─Phase 8
          │                                    │                              └─Phase 9
          └────────────────────────────────────┴─Phase 7 (US3, unblocked early)
```

Phase 4 is the longer of the two component tracks (5 implementation tasks versus
2), and contains the highest-uncertainty work, so it governs the critical path.
Start it first if the two tracks are worked sequentially by one person.

### Within Each Component

- Fixtures (T012 / T018) before the tests that consume them
- Tests before implementation — confirm they fail first
- Component B: placement (T022) before gap detection (T023) before alignment (T024)
- Component A: T010's Shape resolution before T016

### Parallel Opportunities

- Phase 2 is largely serial: T004 is a single mechanical rename touching all of `src/diagram.rs`, so T005–T007 and T009 must follow it rather than race it
- **Phases 3 and 4 run fully parallel** — the headline opportunity. Different developers, different sessions, no shared state beyond the T010 seam type
- **Phase 7 (US3) runs parallel with everything after Phase 2**
- Within phases, all [P] tasks touch distinct test functions

---

## Parallel Example: The Two Components

```bash
# After T010 fixes the seam type, both tracks start at once:

# Track A (developer 1 / session 1):
Task: "T012 Land part-1 golden fixtures in src/raw_lines.rs"
Task: "T016 Implement height calculation in src/raw_lines.rs"

# Track B (developer 2 / session 2) — heights come from fixtures, not from A:
Task: "T018 Land part-2 golden fixtures in src/raw_lines.rs"
Task: "T022 Implement placement from supplied heights in src/raw_lines.rs"
Task: "T024 Implement crossing-alignment transfers in src/raw_lines.rs"
```

Both tracks touch `src/raw_lines.rs`, so if worked concurrently, keep them in
separate modules within the file (or separate files) to avoid merge conflicts.

---

## Implementation Strategy

### There Is No Partial MVP

Both components are necessary for any user-visible behavior, and per the feature
owner both must be correct in their own right for the overall feature to be
correct. The first demonstrable increment is **Phase 5**. Before that, value is
measured by fixture pass rate per component, not by rendered output.

### Recommended Sequence (one person)

1. Phases 1–2 → foundation and seam type fixed
2. Phase 4 (Component B) → the longer, riskier track first
3. Phase 3 (Component A) → the shorter track
4. Phase 5 → integrate; T028 proves the halves meet
5. Phases 6–9 → story validation, US3 whenever convenient
6. Phase 10 → polish

### Recommended Sequence (two people)

1. Both complete Phases 1–2 together, agreeing the T010 seam type
2. Split: one takes Phase 3, the other Phase 4 — neither waits on the other
3. Rejoin at Phase 5; T028 is the integration proof
4. Split the validation phases arbitrarily; they are independent

### Risk Note

Per research R4, the crossing-alignment construction (T023–T024) is the
highest-uncertainty area: the exact number of rows to move and the exact glyph
sequence are not pinned down in the design docs. The part-2 fixtures are what
make this tractable — build from the non-adjacent-crossing cases in T018 before
generalizing, and expect more iteration here than anywhere else.

The second risk is the unresolved Shape question in
[contracts/strand-heights.md](./contracts/strand-heights.md): research R2
describes heights per *pair*, the feature request describes them per *strand*.
T010 resolves it from the fixtures. Implementing Component A before that
resolution risks building the wrong computation.

---

## Notes

- [P] tasks = different files or different test functions, no dependencies
- [Story] label marks which user story a validation task serves; component and foundational tasks carry none
- **Fixture expected-values must never be generated by running the implementation** — that would turn a correctness test back into a snapshot
- Verify tests fail before implementing
- Commit after each task or logical group, using conventional-commit prefixes (`feat:`, `fix:`, `refactor:`, `test:`, `doc:`)
- Never accept a diff on a pre-existing snapshot — that is an SC-004 regression, not a snapshot update
- Run `cargo check --target wasm32-unknown-unknown` before marking any implementation task done (Principle II, NON-NEGOTIABLE)
