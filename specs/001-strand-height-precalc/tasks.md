---

description: "Task list for height-precalculated strand placement rendering mode"
---

# Tasks: Height-Precalculated Strand Placement (Rendering Mode)

**Input**: Design documents from `/specs/001-strand-height-precalc/`

**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/public-api.md, quickstart.md

**Tests**: Test tasks ARE included and are MANDATORY — Constitution Principle III (Test-First) requires `insta` snapshot coverage for new diagram operations and regression tests for changes in `diagram.rs`/`rotate.rs`.

**Organization**: Tasks are grouped by user story (US1–US4 from spec.md) so each story is independently implementable and testable.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

Single Rust library crate: sources in `src/`, tests inline (`#[cfg(test)] mod tests`) plus `insta` snapshots in `src/snapshots/` and `src/diagram/snapshots/`. Example binaries in `examples/`.

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Establish the pre-change baseline that the parity gate (SC-004) is measured against

- [ ] T001 Record baseline by running `cargo test` and confirming all existing snapshots in `src/snapshots/` and `src/diagram/snapshots/` pass unmodified
- [ ] T002 [P] Confirm WASM gate passes pre-change with `cargo check --target wasm32-unknown-unknown` (Constitution Principle II)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Introduce `RenderMode` and the mode-carrying diagram. Required by ALL user stories.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T003 Add `pub enum RenderMode { Legacy (default), PrecalculatedHeights }` deriving `Clone, Copy, PartialEq, Eq, Debug, Default` in `src/render.rs` per data-model.md
- [ ] T004 Convert `AbbreviatedDiagram` from tuple struct to named struct `{ items: Vec<AbbreviatedItem>, mode: RenderMode }` in `src/diagram.rs`, performing the mechanical `self.0` → `self.items` rename across all ~40 access sites (depends on T003)
- [ ] T005 Update all `AbbreviatedDiagram` constructors in `src/diagram.rs` (`FromStr`, `new_from_tuples`, `to_tuples`) so they default `mode` to `RenderMode::Legacy` (depends on T004)
- [ ] T006 Add `mode()`, `set_mode()`, and `with_mode()` accessors to `impl AbbreviatedDiagram` in `src/diagram.rs` (depends on T004)
- [ ] T007 Re-export `RenderMode` from `src/lib.rs` alongside the existing `render` re-exports (depends on T003)
- [ ] T008 Update `VerboseDiagram::from_abbreviated` in `src/render.rs` to read the diagram's mode and dispatch to the legacy placement path (behavior unchanged at this point) (depends on T004, T003)
- [ ] T009 Audit and re-accept any `assert_debug_snapshot!` snapshots that shifted due to the struct change, running `cargo insta review` for `src/diagram/snapshots/` (depends on T005)
- [ ] T010 **Parity gate**: verify `cargo test` passes with every pre-existing ASCII snapshot byte-for-byte unchanged and re-run `cargo check --target wasm32-unknown-unknown` (depends on T009)

**Checkpoint**: `RenderMode` exists, defaults to `Legacy`, and all existing behavior is provably unchanged — user stories can now begin

---

## Phase 3: User Story 1 - Render with reduced up-and-down strand movement (Priority: P1) 🎯 MVP

**Goal**: In `PrecalculatedHeights` mode, place each opening at its precalculated maximum row so passing strands render flat instead of zig-zagging up and back down (FR-001, FR-002, FR-003, FR-004, FR-009).

**Independent Test**: Render `terrace` (`(0 (2 (4 (6 )5 )3 )1 (1 (3 (5 )6 )4 )2 )0`) in the new mode; strands that previously climbed and descended render with no intermediate vertical movement, transfer count drops, and the figure decodes to the same knot.

### Tests for User Story 1 ⚠️ Write FIRST, ensure they FAIL

- [ ] T011 [P] [US1] Add unit test for the peak-row precalculation pass (open/close stack simulation, expected peak row per opening) in the `#[cfg(test)] mod tests` of `src/raw_lines.rs`
- [ ] T012 [P] [US1] Add `insta` snapshot test rendering `terrace` in `RenderMode::PrecalculatedHeights` in `src/diagram/tests.rs`
- [ ] T013 [P] [US1] Add test asserting the `terrace` precalc render contains strictly fewer transfer glyphs (`TransferUp*`/`TransferDown*`) than the legacy render, and that a strand whose row is unchanged between open and close renders flat, in `src/diagram/tests.rs`
- [ ] T014 [P] [US1] Add edge-case tests for empty and single-pair diagrams producing legacy-equivalent output in the new mode (FR-010) in `src/diagram/tests.rs`

### Implementation for User Story 1

- [ ] T015 [US1] Implement the peak-row precalculation pass (linear walk of the abbreviated sequence maintaining the open-pair stack, folding each pair's maximum bottom row) in `src/raw_lines.rs` per research.md R2 (depends on T011)
- [ ] T016 [US1] Implement the max-height placement path — opening each pair directly at its peak row and keeping placed strands flat — as new `append`/`expand`/`contract` variants in `src/raw_lines.rs` per research.md R3 (depends on T015)
- [ ] T017 [US1] Wire `VerboseDiagram::from_abbreviated` in `src/render.rs` to dispatch to the max-height path when the mode is `PrecalculatedHeights` (depends on T016, T008)
- [ ] T018 [US1] Preserve the boundary diagonals intrinsic to a pair entering at its opening index and leaving at its closing index in `src/raw_lines.rs` (FR-009) (depends on T016)
- [ ] T019 [US1] Add a temporary legacy fallback in `src/render.rs` so diagrams containing crossings still render via the legacy path in `PrecalculatedHeights` mode, keeping knot fidelity (FR-006) correct at this checkpoint; removed in US4 (depends on T017)
- [ ] T020 [US1] Run `cargo insta review` to accept the new `PrecalculatedHeights` snapshots and confirm no legacy snapshot changed, then `cargo check --target wasm32-unknown-unknown` (depends on T017, T018, T019)

**Checkpoint**: Crossing-free diagrams render flat in the new mode; legacy output untouched; MVP demonstrable

---

## Phase 4: User Story 2 - Keep diagram complexity stable under repeated rotation (Priority: P2)

**Goal**: Rotation performed under the active `PrecalculatedHeights` mode does not accumulate features from reversed-direction transfers (SC-006).

**Independent Test**: Rotate a diagram whose legacy render has reversed-direction transfers; the scanned feature count never exceeds the original and is strictly lower than the legacy-mode rotation. A full four-rotation cycle returns an equivalent knot without growth.

### Tests for User Story 2 ⚠️ Write FIRST, ensure they FAIL

- [ ] T021 [P] [US2] Add test asserting the scanned feature count (`items` length) after one `try_rotate_90_ccw` in `PrecalculatedHeights` is ≤ the original and < the legacy-mode rotation, in `src/diagram/tests.rs`
- [ ] T022 [P] [US2] Add test rotating through a full four-rotation cycle in `PrecalculatedHeights` asserting no monotonic feature growth and knot equivalence with the original, in `src/diagram/tests.rs`
- [ ] T023 [P] [US2] Add test asserting the active mode survives rotation (rotated diagram's `mode()` equals the pre-rotation mode) in `src/diagram/tests.rs`

### Implementation for User Story 2

- [ ] T024 [US2] Update `try_rotate_90_ccw` in `src/diagram.rs` so the diagram rebuilt from `Self::new_from_tuples(out)` carries forward `self.mode` instead of defaulting to `Legacy` (depends on T023, T006)
- [ ] T025 [US2] Verify `full_render_lines` in `src/diagram.rs` feeds the mode-aware render into `scan_row` so rotation scans the max-height grid (depends on T017, T024)
- [ ] T026 [US2] Add `insta` snapshot(s) for a rotated diagram in `PrecalculatedHeights` mode in `src/diagram/tests.rs` and accept via `cargo insta review` (depends on T024, T025)

**Checkpoint**: Rotation is mode-aware and stable across repeated application; US1 still passes

---

## Phase 5: User Story 3 - Opt in without changing existing output (Priority: P3)

**Goal**: The rendering mode is a single operating context; legacy remains the default and notation-only moves are mode-independent (FR-005, FR-012, FR-013, SC-004).

**Independent Test**: A default-constructed diagram reports `RenderMode::Legacy` and renders byte-identically to today; a notation-only move yields identical `items` under either mode.

### Tests for User Story 3 ⚠️ Write FIRST, ensure they FAIL

- [ ] T027 [P] [US3] Add test asserting a parsed/default-constructed diagram has `mode() == RenderMode::Legacy` (FR-013) in `src/diagram/tests.rs`
- [ ] T028 [P] [US3] Add test asserting notation-only moves (`Swap`, `WrapAround`, `ChangeCrossing`, a Reidemeister move) produce identical `items` under both modes (FR-012, US3 scenario 3) in `src/diagram/tests.rs`
- [ ] T029 [P] [US3] Add test asserting `with_mode`/`set_mode` change only rendering and rotation, leaving `items` untouched, in `src/diagram/tests.rs`

### Implementation for User Story 3

- [ ] T030 [US3] Confirm the mode is threaded only through render and rotation paths in `src/diagram.rs`, ensuring notation-only move implementations never read `self.mode` (depends on T028)
- [ ] T031 [US3] Document the operating-context semantics and the legacy default on the `RenderMode` enum and the accessors in `src/render.rs` and `src/diagram.rs`, matching contracts/public-api.md (depends on T030)

**Checkpoint**: Opt-in semantics verified; all prior stories still pass

---

## Phase 6: User Story 4 - Fidelity preserved across all element types (Priority: P4)

**Goal**: Crossings render correctly under max-height placement via crossing-alignment transfers, and the temporary fallback is removed (FR-007, FR-011).

**Independent Test**: Render `basket` and `ugly_trefoil` in the new mode; every crossing connects the same two strands as the legacy render and no crossing is drawn between non-adjacent rows.

### Tests for User Story 4 ⚠️ Write FIRST, ensure they FAIL

- [ ] T032 [P] [US4] Add minimal hand-built crossing fixtures whose partners are separated under max-height placement, with `insta` snapshots, in `src/raw_lines.rs` tests
- [ ] T033 [P] [US4] Add `insta` snapshot tests rendering `basket` and `ugly_trefoil` in `RenderMode::PrecalculatedHeights` in `src/diagram/tests.rs`
- [ ] T034 [P] [US4] Add test asserting no crossing is drawn between non-adjacent rows and each crossing connects the same strand pair as the legacy render (FR-011) in `src/diagram/tests.rs`

### Implementation for User Story 4

- [ ] T035 [US4] Implement crossing-partner gap detection at each crossing column in `src/raw_lines.rs` per research.md R4 (depends on T032, T016)
- [ ] T036 [US4] Implement crossing-alignment transfer insertion — bring the two crossing strands adjacent immediately before the crossing and restore placement after — in `src/raw_lines.rs` (depends on T035)
- [ ] T037 [US4] Remove the temporary crossing fallback added in T019 from `src/render.rs` so crossing diagrams use the max-height path (depends on T036)
- [ ] T038 [US4] Re-run the US2 rotation assertions to confirm crossing-alignment transfers do not increase the scanned feature count (SC-006) in `src/diagram/tests.rs` (depends on T037, T021)
- [ ] T039 [US4] Accept new snapshots via `cargo insta review` and re-run `cargo check --target wasm32-unknown-unknown` (depends on T037)

**Checkpoint**: All four user stories independently functional; full element coverage

---

## Phase 7: Polish & Cross-Cutting Concerns

- [ ] T040 [P] Add a mode-selection flag to `examples/ascii_print.rs` for manual inspection of the new mode
- [ ] T041 [P] Add a mode toggle to the mini app in `examples/knot-so-good/src/main.rs`
- [ ] T042 [P] Document `RenderMode` and the rotation interaction in `README.md`
- [ ] T043 Run every scenario in `specs/001-strand-height-precalc/quickstart.md` and confirm the documented pass criteria
- [ ] T044 Final gate: `cargo test`, `cargo check --target wasm32-unknown-unknown`, and confirm no pre-existing snapshot changed (SC-004)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies
- **Foundational (Phase 2)**: Depends on Setup — **BLOCKS all user stories**
- **US1 (Phase 3)**: Depends on Foundational
- **US2 (Phase 4)**: Depends on Foundational + US1 (rotation scans the max-height render)
- **US3 (Phase 5)**: Depends on Foundational only — can run in parallel with US1/US2
- **US4 (Phase 6)**: Depends on US1 (extends the max-height placement path)
- **Polish (Phase 7)**: Depends on all desired stories

### User Story Dependencies

- **US1 (P1)**: Independent once Foundational completes — the MVP
- **US2 (P2)**: Requires US1's renderer to exist; independently testable via rotation feature counts
- **US3 (P3)**: Genuinely independent (validates the default/opt-in contract established in Foundational)
- **US4 (P4)**: Extends US1's placement path; removes the T019 fallback

> **Note on US1 → US4 sequencing**: T019 adds a deliberate temporary legacy fallback for crossing-bearing diagrams so that FR-006 (same knot) holds at *every* checkpoint, not just at the end. US4 removes it. This keeps each intermediate state shippable rather than transiently incorrect.

### Parallel Opportunities

- T001 and T002 (Setup) can run together
- Within Foundational, T003 and T007 touch different files; T004–T006 are sequential on `src/diagram.rs`
- All test tasks within a story (T011–T014, T021–T023, T027–T029, T032–T034) are [P] — different concerns, authored before implementation
- US3 (Phase 5) can proceed in parallel with US1/US2 by a second developer
- Polish tasks T040, T041, T042 are [P] (different files)

---

## Parallel Example: User Story 1

```bash
# Author all US1 tests together (they must FAIL first):
Task: "Unit test for peak-row precalculation in src/raw_lines.rs"
Task: "Snapshot test rendering terrace in PrecalculatedHeights in src/diagram/tests.rs"
Task: "Transfer-count reduction + flat-strand assertions in src/diagram/tests.rs"
Task: "Edge-case tests for empty and single-pair diagrams in src/diagram/tests.rs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (baseline)
2. Complete Phase 2: Foundational (CRITICAL — blocks everything)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: render `terrace` in both modes; confirm fewer transfers, same knot, legacy snapshots untouched
5. Demo the flattened diagram

### Incremental Delivery

1. Setup + Foundational → `RenderMode` exists, behavior provably unchanged
2. Add US1 → flat strands for crossing-free diagrams (MVP)
3. Add US2 → rotation stability, the motivating payoff
4. Add US3 → opt-in/operating-context contract verified
5. Add US4 → crossing support, fallback removed → feature complete

### Constitution Gates (every phase)

- `cargo check --target wasm32-unknown-unknown` before marking any task done (Principle II)
- `cargo insta review` to accept new snapshots before committing (Principle III)
- No `Cargo.toml` additions (Principle V)
- Conventional commit prefixes, one logical change per commit

---

## Notes

- [P] tasks = different files, no dependencies
- Verify tests fail before implementing (Principle III)
- Never accept a changed pre-existing snapshot — that is a regression, not an update (SC-004)
- The crossing-alignment construction (T035, T036) is the highest-uncertainty area per research.md R4; develop it against the small fixtures in T032 before generalizing
- Commit after each task or logical group
