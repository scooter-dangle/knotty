---
description: "Task list for retiring the split-cell rendering"
---

# Tasks: Retire the Split-Cell Rendering

**Input**: Design documents from `/specs/005-retire-standard-rendering/`

**Prerequisites**: [plan.md](./plan.md), [spec.md](./spec.md), [research.md](./research.md), [data-model.md](./data-model.md), [contracts/public-api.md](./contracts/public-api.md), [quickstart.md](./quickstart.md)

**Tests**: Test tasks are included and are the bulk of the work. The spec asks for them directly —
User Story 1 is nothing but verification, and FR-003, FR-017 and FR-018 are test requirements — and
constitution III forbids removing covered behaviour without a replacement.

**Organization**: One phase per user story, in the order the user asked for: verify, unblock
rotation, remove the mode, remove the half-cells. **Phases 3–6 must run in order.** This feature is a
removal, so the phases are not independently startable the way a feature's stories usually are: each
one deletes what the previous one made safe to delete. They *are* independently landable and
revertable, which is what FR-005 and SC-007 require.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

Library at `src/`, tests inline in `#[cfg(test)]` modules and in `src/diagram/{test,tests}.rs`,
recorded pictures in `src/snapshots/` and `src/diagram/snapshots/`, surfaces at
`examples/ascii_print.rs` and `examples/knot-so-good/src/`.

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Capture the baseline this whole feature is measured against (SC-002, SC-003, SC-006).

- [X] T001 Record the pre-change baseline in `specs/005-retire-standard-rendering/baseline.md`: `cargo test` count (expect 92 passing), the output of `cargo check --target wasm32-unknown-unknown` and `cargo check --package knot-so-good --target wasm32-unknown-unknown`, and the current commit SHA
- [X] T002 [P] Copy the eight `src/diagram/snapshots/knotty__diagram__tests__snapshot_ascii_print_opening_centered*.snap` files to `specs/005-retire-standard-rendering/baseline-pictures/` so SC-002 can be checked byte-for-byte after Phase 3 renames them

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: The audit gate. FR-002 requires it, and SC-004 makes it blocking: no deletion may land against an open entry.

**⚠️ CRITICAL**: No work in Phases 3–6 may begin until the audit exists and every entry names a replacement.

- [X] T003 Write the audit to `specs/005-retire-standard-rendering/audit.md`, one entry per remaining split-cell use with its named replacement, seeded from `research.md` R2 (the removal-surface table) and R4 (the test-fate table), covering `src/render.rs`, `src/raw_lines.rs`, `src/diagram.rs`, `src/lib.rs`, `src/diagram/tests.rs`, `src/diagram/test.rs`, both snapshot directories, `examples/ascii_print.rs`, `examples/knot-so-good/src/{main,tests}.rs` and `README.md`
- [X] T004 Add to `specs/005-retire-standard-rendering/audit.md` the three gaps `research.md` did not cover, each found by inspecting the tree: (a) `src/raw_lines.rs` has **no** test of the surviving `OpeningCentered` builder — its only two tests, `snapshot_raw_lines_append` and `snapshot_raw_lines_expand_contract`, drive the split-cell functions, so deleting those leaves the file with zero coverage; (b) `src/render.rs`'s test constants `UNKNOT` and `TREFOIL` (lines ~800–814) are written in split-cell diagram text using the retired characters `A a ' ,`, and about thirteen tests depend on them; (c) `src/snapshots/knotty__snapshot_raw_lines_expand_above.snap` is an orphan (`source: src/lib.rs`) matching no current test
- [X] T005 Confirm in `specs/005-retire-standard-rendering/audit.md` that every entry names an existing replacement, a replacement to be built in Phase 3 (US1), or a reason none is possible; record explicitly that `src/rotate.rs` and `src/moves.rs` need no entry, per `research.md` R1

**Checkpoint**: Audit complete with no open entries — Phase 3 may begin.

---

## Phase 3: User Story 1 - Prove coverage before deleting anything (Priority: P1) 🎯 MVP

**Goal**: Every behaviour, invariant and recorded picture that only the split-cell rendering covers gets an opening-centered equivalent that passes — landed while both renderings still exist (FR-001, FR-003).

**Independent Test**: `git diff --stat` shows additions only, with no deletions in `src/` or `examples/`; `cargo test` passes with the new tests on top of the existing 92; `RenderMode::Standard` is still the default everywhere.

**This is the MVP.** Landed alone it delivers value even if the removal never happens: the surviving rendering ends up with test and snapshot coverage matching the older one's.

### Tests for User Story 1

> These tests are the deliverable of this phase, not a preamble to it. Each must pass on landing — they assert what is already true, so that the later phases can remove the split-cell side without losing it.

- [X] T006 [US1] Add a differential rotation test to `src/diagram/tests.rs` that enumerates valid abbreviated diagrams by DFS over the notation's own rules (an opening at `idx <= height` raises height by 2, a closing lowers it, a crossing needs `idx + 2 <= height`, the diagram closes at height 0), rotates each one off both renderings via `crate::rotate::scan_row`, and asserts the resulting notation matches item for item (FR-008a). Bound it so the suite stays fast — length 2–6, height ≤ 6, ~12,900 diagrams, about 30s in debug — and see `research.md` R1 for the reference implementation and its results at the wider bound
- [X] T007 [US1] Add two guards to the differential test in `src/diagram/tests.rs`: assert that at least one diagram carrying transfer cells was compared (transfers are where the two renderings legitimately differ, so a corpus without them proves nothing), and that every comparison had **both** sides return `Ok` — agreement by failing alike is not agreement (FR-018, SC-005)
- [X] T008 [US1] Add an absolute climb-cost test to `src/diagram/tests.rs`: under the opening-centered rendering a strand climbing N levels occupies exactly N columns, asserted directly rather than as a ratio against the split-cell rendering (FR-024, SC-009), carrying an `assert!(measured >= 1, ...)` guard in the style of the existing `transfer_free_knots_render_identically_in_both_modes` (FR-018)
- [X] T009 [P] [US1] Add an `insta` snapshot test of the surviving `OpeningCentered` builder to `src/raw_lines.rs`'s `mod tests`, mirroring what `snapshot_raw_lines_append` does for the split-cell builder — append a sequence of openings, closings and crossings and snapshot the resulting `Vec<Vec<Horiz>>` — closing the coverage gap recorded in T004(a); accept with `cargo insta review`
- [X] T010 [P] [US1] Add a test to `examples/knot-so-good/src/tests.rs` that a persisted state JSON carrying `"render_mode":"standard"` alongside other settings deserializes into a `PersistedState` with those other settings intact, pinning the FR-012 guarantee that `PersistedState` does not set `serde(deny_unknown_fields)` before the field is removed
- [X] T011 [US1] Confirm by inspection and record in `specs/005-retire-standard-rendering/audit.md` that `snapshot_ascii_print_opening_centered` already covers every knot `snapshot_ascii_print` does — both iterate `sample_knots()` — so FR-003 is satisfied for recorded pictures with no new snapshot needed

### Gate for User Story 1

- [X] T012 [US1] Run the Phase 1 gate from `specs/005-retire-standard-rendering/quickstart.md`: `cargo test` green, both wasm checks pass, `git diff --stat` shows no deletions under `src/` or `examples/`, and `RenderMode::Standard` is still the default

**Checkpoint**: The surviving rendering now carries every guarantee the split-cell one does. Deletion is safe to start.

---

## Phase 4: User Story 2 - Free rotation from the split-cell rendering (Priority: P1)

**Goal**: Rotation stops depending on the rendering being removed, without changing a single rotation result (FR-006, FR-007, FR-008).

**Independent Test**: All eight rotation tests and all three rotation regression tests pass with their expectations unchanged; the Phase 3 differential test still passes (both renderings still exist here, so it can still run).

**Depends on**: T006–T007 — the differential test is what makes this safe, and it must be landed and passing first.

- [X] T013 [US2] In `src/diagram.rs`, change `full_render_lines` (around line 909) to build with `RenderMode::OpeningCentered` in both places — the `VerboseDiagram::from_abbreviated` call and the `line.display::<false>(...)` call — and replace the three-line "Pinned to Standard" comment above it with one recording that the read-back reads the surviving rendering. **Do not edit `src/rotate.rs`**: `research.md` R1 establishes over 175,536 diagrams that `scan_row` and its six regexes already read opening-centered pictures identically
- [X] T014 [US2] Run `cargo test rotate` and then the full `cargo test`, confirming the eight rotation tests, `test_try_rotate_90_ccw_period_4`, `test_try_rotate_90_ccw_period_4_regressions` and `rotate_then_render_out_of_bounds_regression` all pass with expectations unchanged, and that `git diff src/rotate.rs` is empty

**Checkpoint**: Nothing outside display depends on the split-cell rendering any more.

---

## Phase 5: User Story 3 - One rendering, no choice to make (Priority: P1)

**Goal**: `RenderMode` and every trace of the split-cell rendering leave the library, the example program, the app and the documentation (FR-009 through FR-019).

**Independent Test**: `grep -rn "RenderMode\|render_mode\|KNOTTY_OPENING_CENTERED" src examples README.md` returns nothing; `cargo test` green; both wasm checks pass; the app's four manual checks in `quickstart.md` pass.

**Depends on**: Phase 4 complete (rotation must already be free) and Phase 3 complete (the replacements must already exist).

### Library

- [X] T015 [US3] In `src/render.rs`, delete the `RenderMode` enum (lines ~24–29); inline `opening_centered_display` into `display` and `opening_centered_display_with_borders` into `display_with_borders`, deleting `standard_display` (lines ~53–135) and `standard_display_with_borders` (lines ~206–303) and dropping the `mode` parameter from both public methods; delete `Horiz::in_mode` (lines ~398–417) and `Horiz::subsequent` (lines ~371–396, used only by the split-cell builder per `research.md` R2)
- [X] T016 [US3] In `src/render.rs`, drop the `mode` parameter from `VerboseLine::display` (line ~473), `VerboseDiagram::display` (line ~516) — collapsing its mode-dependent blank-line filter to the `OpeningCentered` arm alone — and `VerboseDiagram::to_text` (line ~549), whose `.map(|horiz| horiz.in_mode(mode))` step disappears with `in_mode`; update `impl Display for VerboseDiagram` (line ~618) to call the one-argument `to_text`
- [X] T017 [US3] In `src/raw_lines.rs`, delete the split-cell builder — `is_empty_above`, `advance`, `expand_above`, `contract_above` and `append` (lines 5–170) — leaving `OpeningCentered` as the module's only builder, and delete the now-dangling `snapshot_raw_lines_append` and `snapshot_raw_lines_expand_contract` tests along with their six `.snap` files under `src/snapshots/`; the replacement coverage landed as T009
- [X] T018 [US3] In `src/diagram.rs`, remove the `mode` parameter from all ten entry points per `contracts/public-api.md` — `VerboseDiagram::from_abbreviated` (line ~118, keeping only the `OpeningCentered` arm), the four `AbbreviatedDiagram` methods `try_ascii_print`/`ascii_print`/`try_ascii_print_compact`/`ascii_print_compact` (lines ~1525–1578) and the four free functions of the same names (lines ~1587–1612) — and update the `use crate::render::{...}` and `use crate::raw_lines::{...}` imports at the top
- [X] T019 [US3] In `src/lib.rs`, drop `RenderMode` from the `pub use render::{...}` re-export (line 10)

### Tests and recorded pictures

- [X] T020 [US3] In `src/diagram/tests.rs`, delete the tests whose subject no longer exists: `transfer_free_knots_render_identically_in_both_modes`, `opening_centered_spends_two_columns_where_standard_spends_three` (replaced by T008), the `transfer_columns`/`grid` helpers if they become unused, and the Phase 3 differential test from T006–T007 (its second operand is gone) — then drop the mode argument from every remaining call (FR-017)
- [X] T021 [US3] In `src/diagram/tests.rs`, delete the `snapshot_ascii_print` test and rename `snapshot_ascii_print_opening_centered` to `snapshot_ascii_print`, so the surviving recordings are named for what they record rather than for the rendering that produced them (FR-016)
- [X] T022 [US3] Delete the eight `src/diagram/snapshots/knotty__diagram__tests__snapshot_ascii_print{,-2..-8}.snap` files and `git mv` the eight `..._snapshot_ascii_print_opening_centered{,-2..-8}.snap` files onto those names, in the same commit as T021; the diff must show pure renames with **file contents unchanged** — verify each against its copy in `specs/005-retire-standard-rendering/baseline-pictures/` from T002 (SC-002)
- [X] T023 [US3] Re-record `snapshot_from_abbreviated` in `src/diagram/tests.rs` and its `src/diagram/snapshots/knotty__diagram__tests__snapshot_from_abbreviated.snap`: it snapshots the debug of a grid built in the split-cell mode, so its **contents legitimately change** to the opening-centered grid — unlike T022, this is a re-record, not a rename; accept with `cargo insta review`
- [X] T024 [US3] In `src/diagram/test.rs`, drop the mode argument from the `assert_rotate_features!` and `assert_rotate_depths!` macros and from every `ascii_print`/`try_ascii_print` call in them (lines ~40, 87, 216, 284, 288, 296, 427, 431, 439, 540)
- [X] T025 [US3] In `src/render.rs`'s `mod tests`, delete `transfer_cells_are_the_same_in_both_modes`, `standard_text_still_round_trips_every_character` and `to_text_matches_display_in_standard_mode`; collapse the `for mode in [...]` loop in `bordered_cells_are_the_plain_cells_behind_a_rule` to a single pass; rename `opening_centered_cells_match_the_table` to `cells_match_the_table` and `retired_characters_are_read_but_never_written_in_opening_centered` to drop its mode suffix; drop the mode argument from the `render` helper (line ~824) and every remaining call
- [X] T026 [US3] Re-record `snapshot_parsed_diagram_render_with_borders` in `src/render.rs` and its `src/snapshots/knotty__render__tests__snapshot_parsed_diagram_render_with_borders.snap`: it renders parsed text with cell boundaries in the split-cell mode, so its contents change; confirm the new picture draws every crossing, opening and closing whole inside one box (FR-015) before accepting with `cargo insta review`

### Surfaces and documentation

- [X] T027 [P] [US3] In `examples/ascii_print.rs`, delete the `KNOTTY_OPENING_CENTERED` lookup and the `mode` binding (lines ~37–41) and drop the mode argument from the `func(&knot, mode)` call; `KNOTTY_GRID`, `KNOTTY_COMPACT` and `KNOTTY_PRINT_ABBREV` and all four compact × borders combinations stay (FR-013)
- [X] T028 [US3] In `examples/knot-so-good/src/main.rs`, delete the `PersistedRenderMode` enum (lines ~17–24), the `render_mode` field of `PersistedState` and its arm in `from_model` (lines ~57, 78–81), the `Msg::SetRenderMode` variant (line ~138) and its `update` arm (lines ~604–612), the `Model::render_mode` field (line ~172) and its restore arm (lines ~535–540), the `render_mode_toggle` method (lines ~274–288), and both `{ self.render_mode_toggle(link) }` call sites (lines ~328 and ~865); drop the mode argument from `render_manual` and from every `display`/`to_text`/`ascii_print` call
- [X] T029 [US3] In `examples/knot-so-good/src/tests.rs`, delete `missing_render_mode_field_defaults_to_standard`, `unknown_render_mode_string_deserializes_to_other` and `round_trip_carries_the_render_mode` (lines ~350–378), keeping the T010 legacy-state test as their replacement, and remove `PersistedRenderMode` from the `use` at line 6 and the `render_mode:` field from the fixtures at lines ~214 and ~371
- [X] T030 [P] [US3] Regenerate the rendered knot in `README.md` (the second code block) with `cargo run --example ascii_print` over the notation in the block above it; the expected output is in `research.md` R5 — it genuinely differs, because that knot contains strand transfers (FR-019)

### Gate for User Story 3

- [X] T031 [US3] Run the Phase 3 gate from `specs/005-retire-standard-rendering/quickstart.md`: `grep -rn "RenderMode\|render_mode\|KNOTTY_OPENING_CENTERED" src examples README.md` returns nothing (SC-001, SC-013); `cargo test` green; both wasm checks pass; the five `ascii_print` invocations produce the documented output and `KNOTTY_OPENING_CENTERED=true` is inert rather than an error; the app's four manual checks pass, including loading a pre-feature saved state carrying `render_mode`

**Checkpoint**: One rendering everywhere. The project is correct and complete without the split-cell rendering; Phase 6 is cleanup.

---

## Phase 6: User Story 4 - Stop carrying the half-cells (Priority: P2)

**Goal**: The eight cell kinds that only the split-cell rendering drew leave the vocabulary, and their characters stop naming a cell (FR-020, FR-020a, FR-020b, FR-022).

**Independent Test**: The cell vocabulary enumerates eight kinds, all drawn; diagram text naming `A a ' , j r 2 L` is rejected with the character and its one-based row and column; text over `. _ x y ( ) / \` still round-trips byte for byte; the app's symbol table shows eight rows.

**Depends on**: Phase 5 complete — the split-cell builder that was the eight variants' only producer must already be gone.

**⚠️ This is the one phase that narrows the diagram text format.** Text that parses today stops parsing. Land it on its own so the breaking change is visible rather than buried.

- [X] T032 [US4] In `src/render.rs`, delete the eight variants `CrossUpOver`, `CrossUpUnder`, `OpenedAbove`, `ClosedAbove`, `TransferUpStart`, `TransferUpFinish`, `TransferDownStart` and `TransferDownFinish` from the `Horiz` enum (lines ~4–21) and their entries from `as_byte` (lines ~423–445) and `from_byte` (lines ~447–469), freeing the characters `A a ' , j r 2 L`; no new error code is needed — `VerboseDiagram::from_str` already reports an unmapped byte by name with one-based row and column (FR-020a)
- [X] T033 [US4] Rewrite the split-cell test fixtures in `src/render.rs`'s `mod tests` as opening-centered text over the surviving eight characters, per the gap recorded in T004(b): `UNKNOT` becomes `"..\n()\n"` and `TREFOIL` becomes `"..___..\n.(._.).\n._y.y_.\n(__x__)\n"` (lines ~800–814), and the scattered literals using `'` or `,` follow — the ragged fixture (lines ~869–870), `parse("()\n',\n")` and its siblings (lines ~884, 889, 894, 898), the error fixture (line ~910) and the canonical fixture (lines ~1007–1008). Verify each rewritten constant by rendering it and comparing against the notation it claims to depict, as `parsed_trefoil_renders_as_the_notation_does` already does
- [X] T034 [US4] In `src/render.rs`'s `mod tests`, shrink `ALL_HORIZ` from 16 to 8 entries and delete the `RETIRED` constant and the tests that iterate it — `retired_cells_are_blank_in_opening_centered` and the read-but-never-written test — and shrink the `cells_match_the_table` table from 16 rows to 8 (FR-002, SC-001)
- [X] T035 [US4] In `src/render.rs`'s `mod tests`, add `A a ' , j r 2 L` to the byte list in `unrecognized_bytes_have_no_mapping` (line ~799), and add a test that parsing text containing each of the eight fails with a message naming that character and its one-based row and column, in the style of the existing `error_position_is_one_based` (FR-020a, SC-012)
- [X] T036 [US4] In `src/diagram/tests.rs`, delete `opening_centered_never_emits_a_retired_cell` — the variants it names no longer exist — folding its surviving assertion, that the grid's top row carries only `Empty` or `Line`, into a test that remains (FR-017)
- [X] T037 [P] [US4] In `examples/knot-so-good/src/main.rs`, shrink `SYMBOL_TABLE` (lines ~209–230) from sixteen rows to the eight surviving cells, so the in-app table lists exactly the characters the parser accepts (FR-022); it builds its characters from `Horiz::as_byte`, so no character literals need editing
- [X] T038 [US4] Confirm and record that an app snapshot whose stored diagram text uses a freed character now reports as invalid without taking the rest of the app down (FR-020b) — the snapshot catalog in `examples/knot-so-good/src/main.rs` renders each entry through `parse::<VerboseDiagram>().map(...).unwrap_or_default()`, so a failing entry renders empty rather than panicking; add a test to `examples/knot-so-good/src/tests.rs` pinning that
- [X] T039 [US4] Run the Phase 4 gate from `specs/005-retire-standard-rendering/quickstart.md`: `cargo test` green; each freed character rejected with its position; text over the surviving eight round-trips byte for byte; the app's symbol table shows eight rows

**Checkpoint**: Every cell the project carries is one it draws.

---

## Phase 7: Polish & Cross-Cutting Concerns

- [ ] T040 Run the full-feature acceptance in `specs/005-retire-standard-rendering/quickstart.md` and walk every Success Criterion SC-001 through SC-013, recording the result in `specs/005-retire-standard-rendering/audit.md`
- [ ] T041 [P] Delete the orphan snapshot `src/snapshots/knotty__snapshot_raw_lines_expand_above.snap` (`source: src/lib.rs`, matching no current test) recorded in T004(c). **Pre-existing cruft, not created by this feature** — separable, and fine to drop from scope if you would rather keep the diff strictly about the rendering
- [ ] T042 [P] Delete the scaffolding this feature used but does not ship: `specs/005-retire-standard-rendering/baseline-pictures/` once T022 has verified against it
- [ ] T043 Review the commit history for the four phases with `git log --oneline` and squash any fixups, so each phase is one conventional commit per the project's minimal-commit style

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: no dependencies
- **Foundational (Phase 2)**: depends on Setup; **blocks Phases 3–6** (SC-004 — no deletion against an open audit entry)
- **US1 (Phase 3)**: depends on Phase 2. Deletes nothing; landable and shippable alone
- **US2 (Phase 4)**: depends on **T006–T007** specifically — the differential test is what makes the re-point safe
- **US3 (Phase 5)**: depends on Phase 3 (replacements must exist) **and** Phase 4 (rotation must be free)
- **US4 (Phase 6)**: depends on Phase 5 — the split-cell builder was the eight variants' only producer
- **Polish (Phase 7)**: depends on Phase 6

### Why the stories are ordered, not parallel

In a normal feature the user stories are independent slices. Here they are a removal sequence: US2 needs US1's evidence, US3 needs US1's replacements and US2's unblocking, US4 needs US3's deletion. Running them out of order breaks the verify-first ordering the user asked for. Each phase is still independently **landable** — a complete, green, revertable commit — which is what FR-005 and SC-007 require.

### Within Each Phase

- Phase 3: tests only, all additive
- Phase 5: library first (T015–T019), then tests and recorded pictures (T020–T026), then surfaces (T027–T030) — constitution I. Do not land the library edits without the surface edits in the same commit, or the app stops compiling and FR-005 breaks
- Phase 6: the enum edit (T032) first, then every fixture it invalidates (T033) in the same commit, or the test module will not compile

### Parallel Opportunities

- **T001 / T002** — different outputs
- **T006–T008** (`src/diagram/tests.rs`) run against **T009** (`src/raw_lines.rs`) and **T010** (`examples/knot-so-good/src/tests.rs`) — three different files, no shared state. T006, T007 and T008 are sequential with each other, sharing one file
- **T027** (`examples/ascii_print.rs`) and **T030** (`README.md`) run against the library tasks
- **T037** (`examples/knot-so-good/src/main.rs`) runs against T033–T036 (`src/`)
- **T041 / T042** — different paths

Everything else in Phases 5 and 6 touches `src/render.rs` or `src/diagram.rs` and must be sequential.

---

## Parallel Example: User Story 1

```bash
# Three files, no shared state — the coverage that makes deletion safe:
Task: "Differential rotation test in src/diagram/tests.rs"                  # T006-T008
Task: "OpeningCentered builder snapshot test in src/raw_lines.rs"           # T009
Task: "Legacy persisted-state test in examples/knot-so-good/src/tests.rs"   # T010
```

---

## Implementation Strategy

### MVP First (User Story 1 only)

1. Phase 1: Setup — baseline captured
2. Phase 2: Foundational — audit written, no open entries
3. Phase 3: User Story 1 — the verification coverage
4. **STOP and VALIDATE**: `cargo test` green, `git diff --stat` shows additions only, both renderings still present, `Standard` still the default
5. This is shippable on its own. It leaves the project with the surviving rendering fully covered, and it is the phase the user's "verifying first" asks for

### Incremental Delivery

1. Setup + Foundational → the gate is in place
2. US1 → coverage lands, nothing removed → **ship (MVP)**
3. US2 → rotation freed, one line, no behaviour change → ship
4. US3 → the rendering is gone; pictures for transfer-carrying knots change as intended → ship
5. US4 → the half-cells are gone; the text format narrows → ship

Each step leaves the project building, green and usable (FR-005, SC-007).

### Where this can go wrong quietly

- **Deleting before restating.** Every absolute restatement is in Phase 3; every deletion is in Phase 5 or later. If a Phase 5 task wants to delete something Phase 3 did not replace, stop and go back to the audit — that is exactly the case SC-004 exists to catch
- **T022 changing snapshot contents.** It is a rename. Contents must be byte-identical to the T002 copies. T023 and T026, by contrast, *are* re-records and their contents must change
- **Landing a library edit without its surface edit.** Phase 5's gate includes the app's wasm check for this reason

---

## Notes

- `src/rotate.rs` and `src/moves.rs` are never edited. If a task seems to require touching `rotate.rs`, re-read `research.md` R1 first
- Commit one phase at a time, conventional-commit prefixed (`test:` for Phase 3, `refactor:` for Phases 4–6, `doc:` for the README)
- `cargo insta review` is needed for T009, T023 and T026 only
- Total: 43 tasks — 2 setup, 3 foundational, 7 (US1), 2 (US2), 17 (US3), 8 (US4), 4 polish
