# Tasks: Height-Precalculated Strand Placement (Rendering Mode)

**Input**: Design documents from `/specs/001-strand-height-precalc/`

**Prerequisites**: [plan.md](./plan.md), [spec.md](./spec.md), [research.md](./research.md), [data-model.md](./data-model.md), [contracts/public-api.md](./contracts/public-api.md), [quickstart.md](./quickstart.md)

**Tests**: Test tasks ARE included. Constitution Principle III (Test-First) is
binding for this repo: new behavior in `src/` requires `#[test]` coverage, and a
new diagram operation requires `insta` snapshot tests specifically. The spec's
Assumptions section also commits to capturing expected outputs as `insta`
snapshots.

**Organization**: Tasks are grouped by user story so each story can be
implemented and validated as an increment.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

Single Rust library crate (per plan.md "Structure Decision"). All feature code
lands in `src/`; tests live in the existing inline `#[cfg(test)] mod tests` of
`src/raw_lines.rs` and in the `src/diagram/{test,tests}.rs` modules; `insta`
snapshots land in `src/snapshots/` and `src/diagram/snapshots/`.

**Constitution gate applied to every implementation task**: `cargo check --target
wasm32-unknown-unknown` must pass before a task is marked done (Principle II,
NON-NEGOTIABLE), and no `Cargo.toml` entry may be added (Principle V).

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Establish the pre-change baseline that SC-004 ("byte-for-byte
identical default output") is measured against.

- [ ] T001 Confirm the pinned toolchain and both targets build from repo root: run `cargo build` and `cargo check --target wasm32-unknown-unknown`, matching the channel in `rust-toolchain.toml`
- [ ] T002 Capture the green baseline: run `cargo test` and record the current pass count and the inventory of the 16 existing snapshot files under `src/snapshots/` and `src/diagram/snapshots/` in the PR description, so any later drift is detectable

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Introduce `RenderMode` and thread it onto `AbbreviatedDiagram` as
an operating context. This is the plumbing every user story reads, and it must
be behavior-neutral on its own.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [ ] T003 Define `pub enum RenderMode { #[default] Legacy, PrecalculatedHeights }` with `#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]` in `src/diagram.rs`, per research R1 and data-model.md
- [ ] T004 Convert `pub struct AbbreviatedDiagram(pub(crate) Vec<AbbreviatedItem>)` at `src/diagram.rs:115` into the named struct `AbbreviatedDiagram { items: Vec<AbbreviatedItem>, mode: RenderMode }`, performing the mechanical `self.0` → `self.items` rename across all ~37 sites in `src/diagram.rs` (depends on T003)
- [ ] T005 Update the remaining tuple-field readers outside the struct's own impl — `knot.0.iter()` in `VerboseDiagram::from_abbreviated` at `src/diagram.rs:123` — to `knot.items.iter()` (depends on T004)
- [ ] T006 Ensure every existing constructor yields `mode = RenderMode::Legacy`: `new_from_tuples` (`src/diagram.rs:1464`), the `FromStr` parser, and the `Self::new_from_tuples` call on the rotation output path in `src/diagram.rs` (depends on T004)
- [ ] T007 Add the accessors `mode(&self) -> RenderMode`, `set_mode(&mut self, RenderMode)`, and the `with_mode(self, RenderMode) -> Self` builder to the `AbbreviatedDiagram` impl in `src/diagram.rs`, per contracts/public-api.md (depends on T004)
- [ ] T008 Re-export `RenderMode` from `src/lib.rs` alongside `AbbreviatedDiagram` and `AbbreviatedItem` (depends on T003)
- [ ] T009 Audit `insta::assert_debug_snapshot!` call sites for any that serialize an `AbbreviatedDiagram` and would gain the new `mode` field (research R1 cost note); re-accept only those, in `src/diagram/snapshots/` (depends on T004)
- [ ] T010 Gate the phase: run `cargo test` and confirm every pre-existing snapshot is byte-for-byte unchanged versus the T002 baseline, then run `cargo check --target wasm32-unknown-unknown` (depends on T004, T005, T006, T007, T008, T009)

**Checkpoint**: `RenderMode` exists, defaults to `Legacy`, no public signature
changed, and observable behavior is identical to pre-feature. User story work
can begin.

---

## Phase 3: User Story 1 - Render with reduced up-and-down strand movement (Priority: P1) 🎯 MVP

**Goal**: Precalculate each strand's peak row and open it there, so passing
strands render flat instead of climbing and descending (FR-001–FR-004, FR-009,
C2–C4, SC-001).

**Independent Test**: Render `terrace` (`(0 (2 (4 (6 )5 )3 )1 (1 (3 (5 )6 )4 )2 )0`)
in `PrecalculatedHeights` and confirm strands that previously climbed and
descended now show no intermediate vertical movement, with strictly fewer
transfer diagonals than the `Legacy` render of the same diagram.

### Tests for User Story 1

> **NOTE: Write these FIRST and confirm they fail before implementing T015–T018.**

- [ ] T011 [P] [US1] Add a unit test for the peak-row precalculation pass — an ordered-stack walk asserting the peak row of each opened pair for hand-checked sequences including `terrace` — in the `#[cfg(test)] mod tests` of `src/raw_lines.rs`
- [ ] T012 [P] [US1] Add a test-only transfer-counting helper over a rendered `VerboseDiagram` that counts `TransferUp*` and `TransferDown*` glyphs, in the `#[cfg(test)] mod tests` of `src/raw_lines.rs`, so FR-004/SC-002 are measurable
- [ ] T013 [P] [US1] Add `insta::assert_snapshot!` coverage rendering `terrace` via `with_mode(RenderMode::PrecalculatedHeights)` in `src/diagram/tests.rs`, mirroring the existing `snapshot_ascii_print` style at `src/diagram/tests.rs:117`
- [ ] T014 [P] [US1] Add a test asserting the `PrecalculatedHeights` render of `terrace` has strictly fewer transfer segments than its `Legacy` render (FR-004, SC-001, C4) in `src/diagram/tests.rs`

### Implementation for User Story 1

- [ ] T015 [US1] Implement the peak-row precalculation pass in `src/raw_lines.rs`: one linear walk of the abbreviated sequence maintaining an ordered stack of open pairs (each `(N` inserts at logical index `N` shifting pairs ≥ N up by 2; each `)N` removes the pair at `N` shifting above down by 2; crossings do not shift), folding each live pair's running maximum bottom index into an opening-event → peak-row map (research R2)
- [ ] T016 [US1] Implement the max-height placement path in `src/raw_lines.rs` alongside the existing `append`/`expand_above`/`contract_above` (`src/raw_lines.rs:135`, `:21`, `:74`): open each pair directly at its precalculated peak row, keep placed strands flat for their lifetime, and still emit the boundary diagonals intrinsic to entering at the opening index and leaving at the closing index (FR-003, FR-009, research R3) (depends on T015)
- [ ] T017 [US1] Make `VerboseDiagram::from_abbreviated` at `src/diagram.rs:118` dispatch on `knot.mode()` — `Legacy` routes through the untouched `append` path, `PrecalculatedHeights` through the new path — leaving the function signature unchanged (depends on T016)
- [ ] T018 [US1] Confirm `src/render.rs` needs no new `Horiz` glyphs for the new path (research R3 predicts the existing `TransferUp*`/`TransferDown*`/`Opened*`/`Closed*` set suffices); if a gap is found, add the minimum glyph and record the deviation in `specs/001-strand-height-precalc/research.md` (depends on T017)
- [ ] T019 [US1] Run `cargo insta review` and accept ONLY the new `PrecalculatedHeights` snapshots in `src/diagram/snapshots/` and `src/snapshots/`; any diff on a pre-existing snapshot is a regression to fix, not to accept (SC-004) (depends on T017)
- [ ] T020 [US1] Run `cargo test` and `cargo check --target wasm32-unknown-unknown` to close the story (depends on T019)

**Checkpoint**: Crossing-free diagrams render flat in the new mode with strictly
fewer transfers; default output untouched. This is the MVP.

---

## Phase 4: User Story 2 - Keep diagram complexity stable under repeated rotation (Priority: P2)

**Goal**: Rotation scans the mode-aware render, so reversed-direction transfers
no longer re-encode as extra features and the scanned feature count stays stable
across rotations (FR-012, C10, SC-006).

**Independent Test**: Rotate a diagram whose `Legacy` render contains
reversed-direction transfers (e.g. `terrace`) with `PrecalculatedHeights`
active; the feature count (`items` length) never exceeds the original, is
strictly lower than the `Legacy`-mode rotation, and a full four-rotation cycle
returns a diagram representing the same knot.

### Tests for User Story 2

- [ ] T021 [P] [US2] Add a test asserting the scanned feature count after one `try_rotate_90_ccw` in `PrecalculatedHeights` mode is ≤ the original count and strictly < the `Legacy`-mode rotation count for `terrace`, in `src/diagram/test.rs` near the existing `test_try_rotate_90_ccw_features` at `src/diagram/test.rs:224` (research R6)
- [ ] T022 [P] [US2] Add a four-rotation full-cycle test asserting the feature count never grows across the cycle and the final diagram represents the same knot as the original, in `src/diagram/test.rs`
- [ ] T023 [P] [US2] Add an `insta` snapshot of the rotated-in-`PrecalculatedHeights` `terrace` notation in `src/diagram/tests.rs`, so the mode-dependent rotation output is pinned

### Implementation for User Story 2

- [ ] T024 [US2] Make `AbbreviatedDiagram::full_render_lines` at `src/diagram.rs:895` render under `self.mode` so `try_rotate_90_ccw` (`src/diagram.rs:912`) scans the mode-aware grid, with both signatures unchanged (depends on T017)
- [ ] T025 [US2] Carry the active mode onto the diagram that rotation constructs, so the operating context survives `try_rotate_90_ccw` and repeated rotation stays in one mode (FR-012) — reconciling with T006's default-`Legacy` construction in `src/diagram.rs` (depends on T024)
- [ ] T026 [US2] Confirm `scan_row` at `src/rotate.rs:13` needs no change — the crossing-alignment and boundary transfers the new mode emits must scan to no extra features (spec Clarifications 2026-06-18); if `scan_row` does need a change, add a regression test in the `mod test_scan_row` at `src/rotate.rs:111` per Constitution Principle III (depends on T024)
- [ ] T027 [US2] Confirm `DiagramMove::Rotate90CounterClockwise` dispatch in `src/moves.rs` reaches the mode-aware path through `try_apply`/`try_apply_all` without a move-API change (research R1) (depends on T024)
- [ ] T028 [US2] Run `cargo test`, accept new snapshots via `cargo insta review`, and run `cargo check --target wasm32-unknown-unknown` (depends on T024, T025, T026, T027)

**Checkpoint**: Repeated rotation in the new mode no longer inflates the feature
count — the motivating use case works.

---

## Phase 5: User Story 3 - Opt in without changing existing output (Priority: P3)

**Goal**: Prove the mode is a genuine opt-in operating context: `Legacy` is the
default and is byte-for-byte unchanged, and notation-only moves are
mode-independent (FR-005, FR-012, FR-013, C1, C9, SC-004).

**Independent Test**: A default-constructed diagram reports
`mode() == RenderMode::Legacy` and renders identically to today; the same
diagram under either mode yields identical `items` after a notation-only move.

### Tests for User Story 3

- [ ] T029 [P] [US3] Add a test asserting `AbbreviatedDiagram::default()`, `new_from_tuples`, and `FromStr` all report `mode() == RenderMode::Legacy` (FR-013) in `src/diagram/test.rs`
- [ ] T030 [P] [US3] Add a test asserting that a notation-only move — `Swap`, `WrapAround`, `ChangeCrossing`, a Reidemeister move, and `Bulge`/`Collapse` — yields identical `to_tuples()` under both modes (C9, US3 acceptance scenario 3) in `src/diagram/test.rs`
- [ ] T031 [P] [US3] Add a test asserting `set_mode` / `with_mode` round-trip and that `with_mode(Legacy)` renders identically to an untouched diagram (C1) in `src/diagram/test.rs`

### Implementation for User Story 3

- [ ] T032 [US3] Confirm the notation-only move implementations in `src/diagram.rs` and `src/moves.rs` read only `items` and never branch on `mode`, correcting any that do (FR-012) (depends on T007)
- [ ] T033 [US3] Verify the free `ascii_print` / `try_ascii_print` / `*_compact` helpers at `src/diagram.rs:1563`–`:1579` still build `Legacy`-mode diagrams, matching the note in contracts/public-api.md (depends on T006)
- [ ] T034 [US3] Run the SC-004 gate: `cargo test` with every pre-existing snapshot byte-for-byte identical to the T002 baseline, plus `cargo check --target wasm32-unknown-unknown` (depends on T032, T033)

**Checkpoint**: Existing consumers and snapshots are provably unaffected.

---

## Phase 6: User Story 4 - Fidelity preserved across all element types (Priority: P4)

**Goal**: Crossings still connect the correct partners under max-height
placement, via localized crossing-alignment transfers, and are never drawn
between non-adjacent rows (FR-007, FR-011, C6).

**Independent Test**: Render `basket` and `ugly_trefoil` in
`PrecalculatedHeights`; every crossing connects the same two strands as in the
`Legacy` render, and no crossing spans non-adjacent rows.

### Tests for User Story 4

- [ ] T035 [P] [US4] Add minimal hand-built crossing fixtures whose partners are separated under max-height placement — the highest-uncertainty area per research R4's open validation point — as `insta` snapshots in the `#[cfg(test)] mod tests` of `src/raw_lines.rs`
- [ ] T036 [P] [US4] Add `insta::assert_snapshot!` coverage rendering `basket` and `ugly_trefoil` in `PrecalculatedHeights` in `src/diagram/tests.rs`, alongside the existing fixtures at `src/diagram/tests.rs:137` and `:152`
- [ ] T037 [P] [US4] Add an invariant test asserting no crossing glyph pair is ever emitted between non-adjacent rows in the new mode, and that each crossing connects the same strand pair as the `Legacy` render (FR-011, C6) in `src/diagram/tests.rs`
- [ ] T038 [P] [US4] Add a test asserting deeply nested openings never place two strands on the same row in the new mode (US4 acceptance scenario 2) in `src/diagram/tests.rs`

### Implementation for User Story 4

- [ ] T039 [US4] Implement crossing-partner gap detection in the max-height placement path in `src/raw_lines.rs`: at each `\N`/`/N`, determine whether the two participating strands sit on adjacent rendered rows (depends on T016)
- [ ] T040 [US4] Emit the localized crossing-alignment transfer in `src/raw_lines.rs` — bring the two partners adjacent immediately before the crossing column and restore their placement immediately after — so a crossing is never drawn between non-adjacent rows (FR-011, research R4) (depends on T039)
- [ ] T041 [US4] Extend the T012 transfer-counting helper in `src/raw_lines.rs` to classify each transfer as open/close displacement versus crossing-alignment, so SC-002 tracks the two counts separately rather than hiding the tradeoff (depends on T012, T040)
- [ ] T042 [US4] Run `cargo test`, accept the new crossing snapshots via `cargo insta review`, and run `cargo check --target wasm32-unknown-unknown` (depends on T040, T041)

**Checkpoint**: The mode is usable for real knots — crossings render faithfully.

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Close the remaining success criteria and edge cases that span
stories.

- [ ] T043 [P] Add edge-case tests for the empty diagram, a lone opening/closing pair with nothing opening beneath it, a strand whose opening row already equals its maximum row, and closings at the bottom row — each rendering without error and equivalent to `Legacy` where no avoidable movement exists (FR-010, C8, spec Edge Cases) in `src/diagram/tests.rs`
- [ ] T044 [P] Add a determinism test rendering the same diagram twice in `PrecalculatedHeights` and asserting identical output (FR-008, C7, SC-005) in `src/diagram/tests.rs`
- [ ] T045 [P] Add a knot-equivalence check across both modes for every fixture in `src/diagram/tests.rs`, confirming the two renders decode to the same knot (FR-006, C5, SC-003)
- [ ] T046 Report the SC-002 measurement: record per-example open/close-displacement and crossing-alignment transfer counts for `terrace`, `basket`, and `ugly_trefoil` in `specs/001-strand-height-precalc/quickstart.md`, making the crossing-heavy tradeoff explicit
- [ ] T047 [P] Optionally expose a mode flag in `examples/ascii_print.rs` for manual inspection, keeping the library the sole owner of the behavior (Constitution Principle I)
- [ ] T048 [P] Optionally expose a mode toggle in the mini app at `examples/knot-so-good/src/main.rs`, with any GUI-only dependency confined to `examples/knot-so-good/Cargo.toml` (Constitution Principle V)
- [ ] T049 Walk the four scenarios in `specs/001-strand-height-precalc/quickstart.md` end to end and confirm each pass condition holds
- [ ] T050 Final constitution gate: `cargo build`, `cargo test`, `cargo check --target wasm32-unknown-unknown`, and confirm `Cargo.toml` gained no dependency (Principles II and V)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies — start immediately
- **Foundational (Phase 2)**: Depends on Setup — BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational. No dependency on other stories
- **User Story 2 (Phase 4)**: Depends on Foundational + US1 (T017 supplies the mode-aware render that rotation scans)
- **User Story 3 (Phase 5)**: Depends on Foundational only — can run fully parallel with US1/US2/US4
- **User Story 4 (Phase 6)**: Depends on Foundational + US1 (T016 supplies the placement path the crossing-alignment logic extends)
- **Polish (Phase 7)**: Depends on all desired stories being complete

### User Story Dependencies

Unlike a typical feature, the stories here are layers on one rendering path
rather than independent slices. The real graph is:

```text
Foundational (T003–T010)
   ├── US3 (opt-in / no regression)        ← fully independent
   └── US1 (max-height placement)          ← MVP
         ├── US2 (rotation stability)
         └── US4 (crossing fidelity)
```

- **US1 (P1)**: Independently testable on crossing-free diagrams (`terrace`)
- **US2 (P2)**: Independently testable once US1 lands, using the crossing-free `terrace` — full validation on crossing-bearing diagrams wants US4 first
- **US3 (P3)**: Independently testable immediately after Foundational; it asserts the *absence* of change
- **US4 (P4)**: Extends US1's placement path; not meaningful before it

### Within Each User Story

- Tests are written first and must FAIL before the implementation tasks in that phase
- Precalculation (T015) before placement (T016) before dispatch (T017)
- Placement (T016) before crossing alignment (T039, T040)
- `cargo insta review` acceptance only after the render path is settled, and only for NEW snapshots

### Parallel Opportunities

- T001 and T002 are sequential (baseline needs a green build)
- Phase 2 is largely serial: T004 is a single mechanical rename touching all of `src/diagram.rs`, so T005–T007 and T009 must follow it rather than race it. T003 and T008 bracket it
- All test tasks marked [P] within a story touch distinct test functions and can be authored in parallel
- **US3 (T029–T034) can run fully parallel with US1, US2, and US4** — it only reads the Foundational plumbing
- T043, T044, T045 are independent test additions; T047 and T048 touch separate example crates

---

## Parallel Example: User Story 1

```bash
# Author all four US1 tests together (they must fail before T015–T018):
Task: "Peak-row precalculation unit test in src/raw_lines.rs mod tests"
Task: "Transfer-counting test helper in src/raw_lines.rs mod tests"
Task: "terrace PrecalculatedHeights snapshot in src/diagram/tests.rs"
Task: "Fewer-transfers-than-Legacy assertion in src/diagram/tests.rs"
```

## Parallel Example: Cross-Story

```bash
# Once Phase 2 closes, US3 needs nothing from US1 and can run alongside it:
Task: "US1 — peak-row precalculation pass in src/raw_lines.rs"
Task: "US3 — default-mode-is-Legacy test in src/diagram/test.rs"
Task: "US3 — notation-only moves are mode-independent test in src/diagram/test.rs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup — establish the byte-for-byte baseline
2. Complete Phase 2: Foundational — `RenderMode` plumbing, behavior-neutral
3. Complete Phase 3: User Story 1 — max-height placement
4. **STOP and VALIDATE**: `terrace` renders flat with strictly fewer transfers; every pre-existing snapshot unchanged
5. This is a shippable, opt-in rendering path on its own

### Incremental Delivery

1. Setup + Foundational → mode exists, nothing behaves differently
2. Add US1 → crossing-free diagrams render flat (MVP)
3. Add US3 → opt-in and no-regression provably locked down (can land in parallel with US1)
4. Add US2 → rotation stability, the motivating payoff
5. Add US4 → crossings render faithfully; the mode becomes usable for real knots
6. Polish → edge cases, determinism, SC-002 reporting, optional example surfaces

### Risk Note

Per research R4, the crossing-alignment construction (T039–T040) is the
highest-uncertainty area: the exact number of rows to move and the exact glyph
sequence are not pinned down in the design docs. Build it test-first from the
minimal hand-built fixtures in T035 before generalizing, and expect to iterate
on that pair of tasks more than any other.

---

## Notes

- [P] tasks = different files or different test functions, no dependencies
- [Story] label maps each task to a user story for traceability
- Verify tests fail before implementing
- Commit after each task or logical group, using conventional-commit prefixes (Constitution: `feat:`, `fix:`, `refactor:`, `test:`, `doc:`)
- Never accept a diff on a pre-existing snapshot — that is an SC-004 regression, not a snapshot update
- Run `cargo check --target wasm32-unknown-unknown` before marking any implementation task done (Principle II, NON-NEGOTIABLE)
