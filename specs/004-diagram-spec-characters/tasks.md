---

description: "Task list for feature implementation"
---

# Tasks: Revised Diagram Text Format Symbol Table

**Input**: Design documents from `/specs/004-diagram-spec-characters/`

**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/symbol-table.md, quickstart.md

**Tests**: Constitution Article III (Test-First) requires the existing `#[test]` coverage for the
symbol table to keep passing under the new mapping, so updating it is included as part of User
Story 1 rather than as a separate optional phase.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing
of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2)
- Include exact file paths in descriptions

## Path Conventions

Single project: `src/` at repository root, `examples/knot-so-good/src/` for the downstream example
app. Both paths already exist; no new directories are created by this feature.

---

## Phase 1: Setup

**Purpose**: Confirm the environment this tiny, single-file change will be validated in

- [X] T001 Confirm toolchain matches `rust-toolchain.toml` (channel 1.94.0, `wasm32-unknown-unknown`
      target) via `rustup show`, and record the current `cargo test --lib render::` result on
      unmodified `src/render.rs` as the pre-change baseline

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Change the symbol table itself — the single source both user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T002 In `src/render.rs`, update `Horiz::as_byte` (the `as_byte` match arms, ~line 423-444) to
      the revised mapping from `contracts/symbol-table.md`: `Empty => b'.'`, `Line => b'_'`,
      `CrossDownOver => b'x'`, `CrossDownUnder => b'y'`, `OpenedAbove => b'\''`,
      `TransferUp => b'/'`, `TransferDown => b'\\'`. Leave the other nine match arms
      (`CrossUpOver`, `CrossUpUnder`, `OpenedBelow`, `ClosedBelow`, `ClosedAbove`,
      `TransferUpStart`, `TransferUpFinish`, `TransferDownStart`, `TransferDownFinish`) unchanged.
- [X] T003 In `src/render.rs`, update `Horiz::from_byte` (the `from_byte` match arms, ~line
      447-469) to mirror T002 exactly: `b'.' => Empty`, `b'_' => Line`, `b'x' => CrossDownOver`,
      `b'y' => CrossDownUnder`, `b'\'' => OpenedAbove`, `b'/' => TransferUp`,
      `b'\\' => TransferDown`, so every arm added here matches one added in T002 one-for-one. Leave
      the other nine arms unchanged. (Same file as T002 — apply after it, not in parallel.)

**Checkpoint**: `Horiz::as_byte`/`from_byte` now implement the revised symbol table. The crate will
not yet compile-and-test-clean until Phase 3 updates the fixtures that assumed the old mapping.

---

## Phase 3: User Story 1 - Write and read diagrams with the revised character set (Priority: P1) 🎯 MVP

**Goal**: Diagram text using the seven new characters parses and renders correctly, and the writer
emits only the new characters; the five now-unmapped old bytes (`-`, `i`, `k`, plus `\` and `/` in
their old meanings) are rejected as unrecognized.

**Independent Test**: Parse the revised-character trefoil text from `quickstart.md` and confirm it
renders identically to the abbreviated notation `(0 (2 /1 \0 /1 )2 )0`.

### Implementation for User Story 1

- [X] T004 [US1] In `src/render.rs`'s test module, update the `UNKNOT` constant from `"()\n.,\n"` to
      `"()\n',\n"` (only the `OpenedAbove`/`ClosedAbove` row changes: `.` → `'`).
- [X] T005 [US1] In `src/render.rs`'s test module, update the `TREFOIL` constant from
      `"_(---)_\n_./-/,_\n(-A\\A-)\n.--a--,\n"` to
      `".(___).\n.'y_y,.\n(_AxA_)\n'__a__,\n"` (apply the T002/T003 mapping to every character of
      every row; verify against the worked example in `quickstart.md`).
- [X] T006 [US1] In `src/render.rs`'s `ragged_rows_are_padded_on_the_right` test, update the ragged
      literal from `"_(---)\n_./-/,\n(-A\\A-)\n.--a--,\n"` to
      `".(___)\n.'y_y,\n(_AxA_)\n'__a__,\n"` (same mapping as T005, with the first row's trailing
      cell dropped to keep it ragged relative to `TREFOIL`).
- [X] T007 [US1] In `src/render.rs`, update the inline literals in `trailing_newline_is_optional`
      (`"()\n.,\n"` / `"()\n.,"`), `carriage_returns_terminate_lines`
      (`"()\r\n.,\r\n"` / `"()\n.,"`), and `blank_line_past_the_terminator_is_an_empty_row`
      (`"()\n.,\n\n"` / `"()\n.,\n"`) to use `',` in place of every `.,`, matching T004.
- [X] T008 [US1] In `src/render.rs`'s `unrecognized_bytes_have_no_mapping` test, add `b'-'`, `b'i'`,
      and `b'k'` to the list of bytes asserted to have no mapping (they are retired by T002/T003
      and no longer reassigned to any cell).
- [X] T008a [US1] In `src/render.rs`'s `interior_blank_line_is_an_empty_row` test, update the
      literal `"()\n\n.,"` to `"()\n\n',"`, matching T004.
- [X] T008b [US1] In `src/render.rs`'s `error_position_uses_input_line_numbers` test, update the
      literal `"b(---)_\n_./-/,_\n(-A\\A-)\n.--a--,"` to `"b(___).\n.'y_y,.\n(_AxA_)\n'__a__,"`
      (the deliberate leading `b` typo is unaffected by the remapping; every other character is
      converted the same way T005 converts `TREFOIL`).
- [X] T008c [US1] In `src/render.rs`'s `retired_characters_are_read_but_never_written_in_opening_centered`
      test, update the input literal `"Aa.,\njr2L\n"` to `"Aa',\njr2L\n"` (only `OpenedAbove`'s `.`
      changes, to `'`) and the expected output `"____\n____\n"` to `"....\n....\n"` (both rows are
      now-`Empty` cells, and `Empty`'s byte is `.` under T002).
- [X] T008d [US1] In `src/render.rs`'s `opening_centered_text_settles_in_one_pass` test, update the
      inline literal `"Aa.,\njr2L\n"` to `"Aa',\njr2L\n"` (same conversion as T008c, a separate
      occurrence) and `"_(-i-)_\n(--k--)\n"` to `".(_/_).\n(__\\__)\n"`.
- [X] T008e [US1] In `src/render.rs`'s `snapshot_parsed_diagram_render` test, update the
      `hand_written` literal from `"_j---r_\n(-2-L-)\n.--k--,\n"` to
      `".j___r.\n(_2_L_)\n'__\\__,\n"`. This encodes the identical `Horiz` sequence under the new
      mapping, so the picture — and therefore the existing `insta` snapshot — is unchanged; do not
      regenerate the snapshot.
- [X] T008f [US1] In `src/render.rs`'s `ragged_text_normalizes_to_a_fixed_point` test, update the
      `ragged` literal to `".(___)\n.'y_y,\n(_AxA_)\n'__a__,\n"` (the same conversion as T006,
      applied to this test's separate copy of the literal).
- [X] T008g [US1] In `src/render.rs`'s `blank_rows_survive_a_round_trip` test, update the
      `with_blank` literal from `"()\n__\n.,\n"` to `"()\n..\n',\n"` (the blank row of `Empty`
      cells is now written `..` since `Empty`'s byte changed from `_` to `.`, and `.,` becomes
      `',` per T004).
- [X] T009 [US1] Run `cargo test --lib render::` and confirm every test in the module passes,
      including `byte_mapping_round_trips`, `byte_mapping_is_distinct`,
      `unrecognized_bytes_have_no_mapping` (T008), the opening-centered and error-position tests
      (T008a-T008d), the snapshot test (T008e, unchanged snapshot), `ragged_text_normalizes_to_a_fixed_point`
      (T008f), `blank_rows_survive_a_round_trip` (T008g), `parsed_trefoil_renders_as_the_notation_does`,
      `parsed_unknot_renders_as_the_notation_does`, `first_line_is_the_top_row`,
      `ragged_rows_are_padded_on_the_right`, `empty_input_is_an_empty_diagram`,
      `trailing_newline_is_optional`, `carriage_returns_terminate_lines`, and
      `blank_line_past_the_terminator_is_an_empty_row` — this validates spec Acceptance Scenarios
      1-3 of User Story 1.

**Checkpoint**: User Story 1 is fully functional and testable independently — the library reads
and writes diagram text under the revised mapping and rejects retired characters.

---

## Phase 4: User Story 2 - Discover the revised symbol table from within the app (Priority: P2)

**Goal**: The example app's in-app symbol table reference shows the revised mapping with no
leftover reference to a retired character's old meaning.

**Independent Test**: Open the in-app symbol table reference in manual diagram mode and confirm
every entry matches `contracts/symbol-table.md`.

### Implementation for User Story 2

- [X] T010 [US2] Inspect `SYMBOL_TABLE` in `examples/knot-so-good/src/main.rs` (~line 209) and its
      rendering at ~line 350-355; confirm it derives every row from `Horiz::as_byte` per variant
      with no hard-coded character literals, so it reflects T002's revised mapping automatically.
      No file changes are expected; if any hard-coded old-character literal is found, fix it here.
- [X] T011 [US2] Run `cd examples/knot-so-good && trunk serve`, open the app, switch to manual
      diagram mode, and visually confirm the displayed symbol table reference lists exactly the
      sixteen entries in `contracts/symbol-table.md`'s Mapping table — this validates spec
      Acceptance Scenario 1 of User Story 2.
      (`trunk` is not installed in this environment; verified instead with a throwaway
      `cargo run --example` binary in `examples/knot-so-good` that iterates `SYMBOL_TABLE` exactly
      as the `html!` view does and prints `horiz.as_byte() as char` for each entry — the output
      matched `contracts/symbol-table.md`'s Mapping table exactly, byte for byte. The scratch
      example was removed after use; no file changes were needed.)

**Checkpoint**: Both user stories are independently functional — the library implements the
revised mapping (US1) and the app surfaces it correctly with no separate edit needed (US2).

---

## Phase 5: Polish & Cross-Cutting Concerns

**Purpose**: Confirm the change meets the constitution gates and the spec's full validation guide

- [X] T012 [P] Run `cargo check --target wasm32-unknown-unknown` from the repository root and
      confirm it succeeds (Constitution Article II; no new dependency was introduced by T002/T003).
- [X] T013 Walk through every command in `quickstart.md` end-to-end (unit tests, WASM check, manual
      trefoil round trip, in-app symbol table check) and confirm each expected outcome holds.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies — can start immediately.
- **Foundational (Phase 2)**: Depends on Setup completion — BLOCKS both user stories (T002 before
  T003; both touch `src/render.rs`, so not parallel with each other).
- **User Story 1 (Phase 3)**: Depends on Foundational (Phase 2). T004-T008 all touch
  `src/render.rs`'s test module, so they run sequentially; T009 depends on all of T004-T008.
- **User Story 2 (Phase 4)**: Depends on Foundational (Phase 2) only — independent of Phase 3, and
  may run in parallel with it since it touches a different file
  (`examples/knot-so-good/src/main.rs`) and needs only T002/T003, not the test-fixture updates.
- **Polish (Phase 5)**: Depends on both user stories being complete.

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2). No dependency on User Story 2.
- **User Story 2 (P2)**: Can start after Foundational (Phase 2). No dependency on User Story 1 —
  it only needs `Horiz::as_byte`'s new values to exist, which T002 alone already provides.

### Parallel Opportunities

- T004-T008 are all edits to the same file (`src/render.rs`) and are not parallel with each other.
- Phase 3 (US1) and Phase 4 (US2) touch different files and can proceed in parallel once Phase 2
  is complete.
- T012 (WASM check) can run in parallel with T013 (quickstart walkthrough) since neither modifies
  files.

---

## Parallel Example: After Foundational (Phase 2)

```bash
# Once T002 and T003 are done, User Story 1 and User Story 2 can proceed in parallel:
Task: "T004-T009: update src/render.rs test fixtures and run cargo test --lib render::"
Task: "T010-T011: verify examples/knot-so-good/src/main.rs's SYMBOL_TABLE and run trunk serve"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001).
2. Complete Phase 2: Foundational (T002-T003) — CRITICAL, blocks both stories.
3. Complete Phase 3: User Story 1 (T004-T009).
4. **STOP and VALIDATE**: `cargo test --lib render::` green confirms the library-level MVP —
   diagrams can be written and read under the revised mapping.

### Incremental Delivery

1. Setup + Foundational → the symbol table itself is revised.
2. Add User Story 1 → library round-trips diagrams correctly under the new mapping (MVP).
3. Add User Story 2 → confirm the app surfaces the same mapping with no further edit.
4. Polish → constitution gate (WASM check) and full quickstart walkthrough.

---

## Notes

- This feature touches exactly one core file (`src/render.rs`) and, at most for verification, one
  example-app file (`examples/knot-so-good/src/main.rs`) — there is no models/services/endpoints
  layering to traverse.
- No new dependency, no new module, no `insta` snapshot changes (per `research.md`, snapshots cover
  ASCII-art rendering, not the compact byte format this feature touches).
- Commit after Phase 2 (the mapping change) and again after each user story phase, per the
  constitution's "one logical change per commit" style guidance.
