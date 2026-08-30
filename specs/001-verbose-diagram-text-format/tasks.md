---

description: "Task list for the verbose diagram text format"
---

# Tasks: Verbose Diagram Text Format

**Input**: Design documents from `/specs/001-verbose-diagram-text-format/`

**Prerequisites**: [plan.md](./plan.md), [spec.md](./spec.md), [research.md](./research.md),
[data-model.md](./data-model.md), [contracts/diagram-text-format.md](./contracts/diagram-text-format.md)

**Tests**: REQUIRED, not optional. Constitution Article III mandates `#[test]` coverage for new
behaviour in `src/` before or alongside the implementation, and `insta` snapshot tests for new
diagram operations. Test tasks below are therefore first-class, not conditional.

**Organization**: Tasks are grouped by user story. Note the honest dependency shape — see
[Dependencies](#dependencies--execution-order): US3 genuinely depends on US1 **and** US2, because
manual mode needs reading and mode-seeding needs writing. Only US1 and US2 are independent of
each other.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: US1, US2, US3 — maps to the user stories in spec.md
- Exact file paths are given in every task

## Path Conventions

Single Rust crate at the repository root (`src/`), with a wasm example app under
`examples/knot-so-good/`. There is no `tests/` directory — the crate uses inline `#[cfg(test)]`
modules and `insta` snapshots under `src/snapshots/`, per plan.md's structure decision.

---

## Phase 1: Setup

**Purpose**: Establish a green baseline so any later failure is attributable to this feature.

- [ ] T001 Run the full CI-parity command set from `.github/workflows/test.yml` (`cargo check --target wasm32-unknown-unknown`, `cargo build`, `cargo test`, then `cargo test` and `trunk build --release` in `examples/knot-so-good/`) and confirm all pass before changing anything

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: The symbol table. Both US1 (reading) and US2 (writing) depend on it, so it cannot live
inside either story.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [ ] T002 Add `Horiz::as_byte(&self) -> u8` and `Horiz::from_byte(u8) -> Option<Self>` as `const fn` matches in `src/render.rs`, placed adjacent to `Horiz::display()` so the two tables stay visibly paired, using exactly the 16 mappings in contracts/diagram-text-format.md
- [ ] T003 Add `#[cfg(test)] mod tests` to `src/render.rs` (inline, matching `src/raw_lines.rs`) with a test asserting `from_byte(as_byte(v)) == Some(v)` for all 16 variants listed explicitly with no wildcard arm, and a test asserting the 16 bytes are pairwise distinct — contract guarantees C-1 and C-2
- [ ] T004 Add tests in `src/render.rs` asserting `from_byte` returns `None` for `b' '`, `b'\t'`, `b'l'`, `b'B'`, and `b'\r'` — guards FR-003 case sensitivity and FR-004 whitespace rejection at the table level

**Checkpoint**: The mapping is proven bidirectional and total. Reading and writing can now proceed
independently.

---

## Phase 3: User Story 1 - Specify an expected rendering directly (Priority: P1) 🎯 MVP

**Goal**: Text in the format produces the rendered picture, with no knot notation involved.

**Independent Test**: Write the canonical trefoil text, parse it, render it, and confirm the picture
is byte-for-byte identical to the one `(0 (2 /1 \0 /1 )2 )0` produces. No app changes needed.

### Tests for User Story 1

> Write these first. They must fail — `VerboseDiagram` has no `FromStr` yet, so they will not compile
> until T009 adds the impl. Add the tests, watch the compile error name the missing impl, then
> implement.

- [ ] T005 [US1] Add a test in `src/render.rs` parsing the canonical trefoil text (`_(---)_` / `_./-/,_` / `(-A\A-)` / `.--a--,`) and asserting its `display::<false>()` output equals `AbbreviatedDiagram::from_str("(0 (2 /1 \\0 /1 )2 )0").unwrap().ascii_print::<false>()` — FR-006, and the reference picture in contracts/diagram-text-format.md
- [ ] T006 [US1] Add a row-order test in `src/render.rs` asserting the arc described by the **first** text line appears in the **top** rows of the rendered output — FR-005. Assert on rendered text, not `is_ok()`; a reversed implementation passes any weaker check (research.md, Trap 1)
- [ ] T007 [US1] Add tests in `src/render.rs` for input normalization: ragged rows render identically to their right-padded equivalent (FR-010); `""` parses to a zero-row diagram (FR-012); `"()\n"` and `"()"` parse identically; `"()\r\n.,"` equals `"()\n.,"`; and `"()\n\n"` yields a second, all-`Empty` row (FR-013)
- [ ] T008 [US1] Add error-message tests in `src/render.rs` asserting that a stray `b` on the **first** text line reports line 1 (not the last line), that column numbers are 1-based, and that only the first offending character is reported — FR-009, contract C-8. This is the single most likely defect in the feature; a test asserting only `is_err()` does not catch it
- [ ] T009 [US1] Add an `insta` snapshot test in `src/render.rs` capturing the rendered ASCII for a parsed hand-written diagram, with the snapshot stored under `src/snapshots/` — Constitution Article III

### Implementation for User Story 1

- [ ] T010 [US1] Implement `impl FromStr for VerboseDiagram` with `type Err = String` in `src/render.rs`: strip one trailing `\n`, split on `\n`, strip a trailing `\r` per line, map each byte via `Horiz::from_byte`, and record the offending position in **input coordinates during the forward pass** before any reversal
- [ ] T011 [US1] In the same `FromStr` impl in `src/render.rs`, add the second pass: compute the widest row, `resize` every row to that width with `Horiz::Empty`, then reverse the row order before constructing `VerboseDiagram` — padding cannot happen in the per-line loop because the width is unknown until parsing ends (research.md, Trap 2)
- [ ] T012 [US1] Run `cargo insta review`, accept the new snapshot, and commit the `.snap` file under `src/snapshots/`
- [ ] T013 [US1] Run `cargo check --target wasm32-unknown-unknown` and confirm the new code compiles for wasm — Constitution Article II, non-negotiable

**Checkpoint**: US1 is complete and shippable on its own. The format can be written by hand and
rendered, which is the whole MVP.

---

## Phase 4: User Story 2 - Read an existing rendering back out as compact text (Priority: P2)

**Goal**: Any renderable diagram emits canonical text that reads back to the identical diagram.

**Independent Test**: Emit the compact text for each built-in knot, parse it back, and confirm the
round trip is byte-stable. Independent of US1's parsing tests, though it shares the `FromStr` impl to
verify against.

### Tests for User Story 2

- [ ] T014 [US2] Add tests in `src/render.rs` asserting `VerboseDiagram::from_abbreviated` output for the unknot and trefoil serializes to exactly the canonical text in data-model.md and contracts/diagram-text-format.md, and that every emitted row has equal length — FR-007, contract C-7
- [ ] T015 [US2] Add round-trip tests in `src/render.rs` covering contract guarantees C-3, C-4, and C-5: `parse(write(d)) == d`; `write(parse(write(d))) == write(d)` byte for byte; and ragged input normalizing to canonical text that is then a fixed point under a further write — FR-008
- [ ] T016 [US2] Add a round-trip test in `src/render.rs` iterating the built-in knots used by the example app (unknot, trefoil, square knot, knot 5_1) plus the fixtures in `examples/samples/`, asserting byte-stability for each — SC-003

### Implementation for User Story 2

- [ ] T017 [US2] Implement `impl fmt::Display for VerboseDiagram` in `src/render.rs`: iterate `self.0` in reverse so the last stored row prints first, emit `Horiz::as_byte` per cell, and terminate each row with `\n` — FR-007
- [ ] T018 [US2] In the same `Display` impl in `src/render.rs`, pad each emitted row to the diagram's widest row so a ragged `VerboseDiagram` (reachable via `Default`) cannot silently break the byte-for-byte round trip — research.md, Trap 2 consequence

**Checkpoint**: The library half is complete. US1 and US2 together satisfy every non-app requirement
(FR-001..FR-014) and can ship with no example-app change.

---

## Phase 5: User Story 3 - Manual diagram mode in the example app (Priority: P3)

**Goal**: A separate app mode where typing the format renders live, with its own snapshots.

**Independent Test**: Open the app, switch to manual mode, type the format, and confirm the picture
appears and updates on every keystroke without pressing a button.

**⚠️ Depends on US1 and US2** — manual mode needs `FromStr`, and mode-seeding needs `Display`.

### Persistence (do first — the existing tests already establish the pattern)

- [ ] T019 [US3] Add `PersistedMode` (`Notation | Manual` with `#[serde(other)] Other`) and `PersistedManualSnapshot` (single `diagram_text` field) to `examples/knot-so-good/src/main.rs`, following the existing `PersistedDisplayMode` pattern exactly
- [ ] T020 [US3] Add `mode`, `manual_diagram`, and `manual_snapshots` fields to `PersistedState` in `examples/knot-so-good/src/main.rs`, each `#[serde(default)]`, and extend `PersistedState::from_model` to populate them — FR-025
- [ ] T021 [US3] Add tests in `examples/knot-so-good/src/tests.rs`: a full round trip carrying manual fields; a pre-feature JSON blob with no `mode`/`manual_diagram` loading as notation mode with empty manual state (FR-026); and an unknown `mode` string deserializing to `Other` and falling back to `Notation` — mirror the existing `missing_fields_use_defaults` and `display_mode_unknown_string_deserializes_to_other` tests

### Model and message plumbing

- [ ] T022 [US3] Add a `Mode` enum and a `mode` field to `Model` in `examples/knot-so-good/src/main.rs`, keeping notation state and manual state as sibling field groups so neither is disturbed by a switch — FR-024
- [ ] T023 [US3] Add manual-mode fields to `Model` in `examples/knot-so-good/src/main.rs`: `manual_text`, the parse `Result<VerboseDiagram, String>`, `last_valid_render: Option<String>`, and `manual_snapshots`
- [ ] T024 [US3] Add `Msg` variants and `update` arms in `examples/knot-so-good/src/main.rs` for `SetMode`, `ManualText`, `ManualSnapshot`, `RestoreManualSnapshot`, and `DeleteManualSnapshot`, saving to storage on each state change as the existing arms do
- [ ] T025 [US3] Implement the manual re-parse pipeline in `examples/knot-so-good/src/main.rs`: on every text edit, parse and render via `VerboseDiagram::display::<false>()`; on success store the render in `last_valid_render`; on failure keep the previous value — FR-016, FR-011

### Manual mode view

- [ ] T026 [US3] Implement the manual mode view branch in `examples/knot-so-good/src/main.rs`: a textarea bound to `manual_text` with an `oninput` callback, and the ASCII picture rendered through the existing `ascii_diagram_to_html` — FR-016, FR-020
- [ ] T027 [US3] Implement the three render states in the manual view in `examples/knot-so-good/src/main.rs`: valid → normal picture; invalid with a prior render → that picture marked stale plus the error beside it (FR-017); invalid with no prior render → error alone (FR-018)
- [ ] T028 [US3] Implement the manual snapshot catalog in `examples/knot-so-good/src/main.rs`, reading only `manual_snapshots`, re-rendering each preview as ASCII from its stored text rather than caching it, with restore and delete buttons — FR-021, FR-022
- [ ] T029 [US3] Extend `Model::snapshot_disabled` (or add a manual-mode equivalent) in `examples/knot-so-good/src/main.rs` so the snapshot button is unavailable while the manual text is invalid — FR-023
- [ ] T030 [US3] Add a symbol table reference to the manual mode surface in `examples/knot-so-good/src/main.rs`, listing all 16 character-to-cell mappings — FR-027

### Notation mode changes and the bridge

- [ ] T031 [US3] Gate every notation-only control behind `Mode::Notation` in the `view` of `examples/knot-so-good/src/main.rs` — the notation and moves textareas, the built-in knot buttons, the move pickers, the rotate button, the encoding readout, the display-mode and compact toggles, and the SVG download — FR-019, FR-020
- [ ] T032 [US3] Add the compact-text readout to notation mode in `examples/knot-so-good/src/main.rs`, computed via `VerboseDiagram::from_abbreviated(&diagram)?.to_string()`, wrapped in a `<details>` element so it is collapsed by default and the existing encoding line keeps its prominence — FR-028, FR-029
- [ ] T033 [US3] Implement seeding in the `SetMode` arm in `examples/knot-so-good/src/main.rs`: when switching to manual mode, fill `manual_text` from the current diagram's compact text **only if `manual_text` is empty**, leaving entered text untouched on every later switch — FR-030, FR-031
- [ ] T034 [US3] Add the mode toggle control to `examples/knot-so-good/src/main.rs`, a single button that switches into manual mode and back, placed with the existing top-row buttons — FR-015

### Styling

- [ ] T035 [P] [US3] Add styles to `examples/knot-so-good/index.html` for the stale render marking, the symbol table, the collapsed compact-text readout, and manual snapshot entries, following the existing disabled-control styling convention

**Checkpoint**: All three stories are functional. The app has two independent modes with separate
state and separate snapshots.

---

## Phase 6: Polish & Cross-Cutting Concerns

- [ ] T036 Walk all ten steps of Scenario 5 in [quickstart.md](./quickstart.md) against `trunk serve --port 3000` in `examples/knot-so-good/`, confirming each cited requirement by hand — the browser behaviour has no automated coverage
- [ ] T037 [P] Update `examples/knot-so-good/README.md` to describe manual diagram mode and the character set, so the app's own docs do not describe only half of it
- [ ] T038 Re-run the full CI-parity set from Scenario 6 of [quickstart.md](./quickstart.md), including `trunk build --release`, since the example app is built by a separate CI job from the library tests
- [ ] T039 Review the complete diff against the conventions in `CLAUDE.md`: no comments or docstrings added to untouched code, no abstractions beyond what the tasks required, and one logical change per commit with a conventional-commit prefix

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: no dependencies
- **Foundational (Phase 2)**: depends on Setup — **blocks all three stories**
- **US1 (Phase 3)** and **US2 (Phase 4)**: both depend only on Foundational, and are independent of
  each other
- **US3 (Phase 5)**: depends on **both** US1 and US2 completing
- **Polish (Phase 6)**: depends on all desired stories

### User Story Dependencies — read this before parallelising

The template's usual "all stories are independent" shape does **not** hold here, and pretending it
does would produce a broken plan:

- **US1 (P1)**: independent once T002–T004 land. This is the MVP.
- **US2 (P2)**: independent of US1. Its tests verify against `FromStr`, so running them needs US1's
  impl, but the `Display` implementation itself does not.
- **US3 (P3)**: **not independent**. Manual mode cannot parse without US1's `FromStr` (T010–T011),
  and seeding (T033) plus the notation readout (T032) cannot work without US2's `Display` (T017–T018).
  Starting US3 before both land means writing against APIs that do not exist.

### Within Each Story

- Tests before implementation (Constitution Article III). In US1 the tests will not compile until
  `FromStr` exists — that compile error *is* the failing state.
- In US3, persistence (T019–T021) before model plumbing (T022–T025) before views (T026–T034).

### Parallel Opportunities

Genuinely limited, and worth stating plainly rather than inflating: the entire library change lives
in one file (`src/render.rs`) and the entire app change lives in one file
(`examples/knot-so-good/src/main.rs`). Tasks touching the same file are sequential edits, not
parallel work, so most tasks here carry no `[P]`.

What can actually run in parallel:

- **T035** (`index.html`) against any task in `main.rs` — different files entirely
- **T037** (`README.md`) against anything
- **T021** (`src/tests.rs`) against `index.html` work, though it depends on T019–T020 landing first
- **US1 and US2 as whole stories**, if two people split them — but both edit `src/render.rs`, so they
  will conflict unless one takes the impls and the other takes the tests

---

## Parallel Example

```bash
# The only clean cross-file split in Phase 5:
Task: "T035 Add styles to examples/knot-so-good/index.html"
Task: "T026 Implement the manual mode view in examples/knot-so-good/src/main.rs"

# And during Polish:
Task: "T037 Update examples/knot-so-good/README.md"
Task: "T038 Re-run the CI-parity command set"
```

---

## Implementation Strategy

### MVP First (User Story 1)

1. Phase 1 — confirm the baseline is green
2. Phase 2 — the symbol table (T002–T004)
3. Phase 3 — parsing and rendering (T005–T013)
4. **Stop and validate**: hand-write the trefoil text, render it, compare against the notation-derived
   picture. Verify a stray character on line 1 reports line 1.
5. This is shippable. The format can be authored by hand and rendered, with no app change at all.

### Incremental Delivery

1. Setup + Foundational → the symbol table is proven bidirectional
2. **+ US1** → author renderings by hand (MVP, library-only)
3. **+ US2** → round-trip an existing rendering out to text (library-only; completes FR-001..FR-014)
4. **+ US3** → the app's manual mode, seeded from a real knot

Steps 2 and 3 together satisfy every requirement that does not mention the app, so the library can
merge and be useful before any UI work starts.

### Parallel Team Strategy

With two developers, the honest split is by **file**, not by story: one takes `src/render.rs`
(Phases 2–4) and the other prepares `examples/knot-so-good/index.html` styling and README changes,
then picks up `main.rs` once the library API is merged. Splitting US1 and US2 across two people means
both editing `src/render.rs` and resolving conflicts for no gain.

---

## Notes

- `[P]` means a genuinely different file with no incomplete dependency — it is used sparingly here on
  purpose
- Commit after each task or logical group, with a conventional-commit prefix
- `cargo check --target wasm32-unknown-unknown` is non-negotiable before any task is called done
- Snapshots must be reviewed with `cargo insta review` and committed; never hand-edit a `.snap`
- The reversal between text order and storage order is the defect most likely to slip through — T006
  and T008 exist specifically to catch it, so do not weaken them to `is_ok()`/`is_err()` checks
