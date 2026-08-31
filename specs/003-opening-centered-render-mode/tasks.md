---

description: "Task list for the opening-centered rendering mode"
---

# Tasks: Opening-Centered Rendering Mode

**Input**: Design documents from `/specs/003-opening-centered-render-mode/`

**Prerequisites**: [plan.md](./plan.md), [spec.md](./spec.md), [research.md](./research.md),
[data-model.md](./data-model.md),
[contracts/opening-centered-rendering.md](./contracts/opening-centered-rendering.md),
[quickstart.md](./quickstart.md)

**Tests**: REQUIRED, not optional. Constitution Article III mandates test coverage for new behaviour
in `src/` and `insta` snapshots for new diagram operations. Unlike feature 002, this feature adds real
library behaviour, so its tests have a genuine red phase — T009, T010 and T011 fail until T012–T016
land, and they are meant to.

**Organization**: Tasks are grouped by user story. The stories are not mutually independent: US1 *is*
the rendering, and US2 and US3 are selectors over it. See [Dependencies](#dependencies--execution-order).

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: US1, US2, US3 — maps to the user stories in spec.md
- Exact file paths are given in every task

## Path Conventions

Single Rust crate at the repository root (`src/`), with a wasm example app under
`examples/knot-so-good/` and a CLI example at `examples/ascii_print.rs`. There is no `tests/`
directory — the crate uses inline `#[cfg(test)]` modules and `insta` snapshots under `src/snapshots/`
and `src/diagram/snapshots/`; the app has host-target tests in `examples/knot-so-good/src/tests.rs`.

---

## Phase 1: Setup

**Purpose**: Establish a green baseline so any later failure is attributable to this feature.

- [X] T001 Run the CI-parity command set from `.github/workflows/test.yml` — `cargo check --target wasm32-unknown-unknown`, `cargo build`, `cargo test`, then `cargo test` and `trunk build --release` in `examples/knot-so-good/` — and confirm all pass before changing anything

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Make the mode expressible everywhere it has to reach, without changing a single byte of
output. Every task here threads `RenderMode` through a signature and passes `Standard` at the call
site; the parameter is **accepted and ignored** until US1 turns each ignore into a real match. This
keeps the crate compiling and panic-free at every commit, and makes the FR-008 guard (T008) meaningful:
if a snapshot moves in this phase, the threading is wrong.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [X] T002 Add `RenderMode` to `src/render.rs` — `#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)] pub enum RenderMode { #[default] Standard, OpeningCentered }` — and export it from `src/lib.rs` with `pub use render::RenderMode;` (research R11, contracts "New export"). `Default` must be `Standard` so `RenderMode::default()` is today's behaviour
- [X] T003 Add `mode: RenderMode` as a parameter to `Horiz::display` and `Horiz::display_with_borders` in `src/render.rs`, keeping both `const fn` and returning the existing tables for either value for now; US1's T012 replaces this with a real match
- [X] T004 Add `mode: RenderMode` to `VerboseLine::display` and `VerboseDiagram::display` in `src/render.rs`, keeping `GRID_BORDERS` a const generic and passing `mode` down to `Horiz` (research R1 — the mode is a runtime value, `GRID_BORDERS` stays generic; do not add a second const generic)
- [X] T005 Add `mode: RenderMode` to `VerboseDiagram::from_abbreviated` in `src/diagram.rs`, to `AbbreviatedDiagram::{try_ascii_print, ascii_print, try_ascii_print_compact, ascii_print_compact}`, and to the four free functions of the same names at the bottom of `src/diagram.rs`, per the signatures in `contracts/opening-centered-rendering.md`
- [X] T006 Pin `AbbreviatedDiagram::full_render_lines` in `src/diagram.rs` to `RenderMode::Standard` explicitly, with a comment citing research R9 — `rotate::scan_row` recognises the Standard tile shapes by regex, so rotation must never see opening-centered output. Leave `src/rotate.rs` and `try_rotate_90_ccw` untouched
- [X] T007 Update every remaining call site to pass `RenderMode::Standard` (~55 sites): the `#[cfg(test)] mod tests` in `src/render.rs`, `src/diagram/tests.rs`, `src/diagram/test.rs`, `examples/ascii_print.rs`, `examples/knot-so-good/src/main.rs` and `examples/knot-so-good/src/tests.rs`. This is one mechanical change — the crate does not compile until all of it lands, so do not split it across commits
- [X] T008 Verify the threading is behaviour-neutral: `cargo test` green with **no** snapshot in `src/snapshots/` or `src/diagram/snapshots/` reported as changed, and `cargo check --target wasm32-unknown-unknown` clean. A pending `.snap.new` for an existing snapshot at this point is a bug in T003–T007, not a snapshot to accept

**Checkpoint**: `RenderMode` reaches every entry point, `Standard` reproduces today byte for byte, and
rotation is pinned. Worth committing on its own — it is a pure signature change with no behaviour
attached, so it reviews cleanly and isolates any regression from the feature that follows.

---

## Phase 3: User Story 1 - Draw a knot with every feature in one cell (Priority: P1) 🎯 MVP

**Goal**: The library can draw any diagram opening-centered — parentheses on the middle line of their
cell, every feature whole inside one cell, no retired character ever emitted.

**Independent Test**: Render the unknot and the trefoil both ways and confirm the pictures are
identical, then render the square knot both ways and confirm they differ only in how the climbing
strands are stepped. Delivers value with no app or CLI change at all.

### Tests for User Story 1

> These have a real red phase. Write them first and watch them fail.

- [ ] T009 [P] [US1] Add a table-driven cell test to the `#[cfg(test)] mod tests` in `src/render.rs` asserting, for all sixteen `Horiz` variants, that `display(RenderMode::OpeningCentered)` equals the cell given in `data-model.md` — including that the eight retired variants return `["   ", "   ", "   "]`, that `TransferUp` and `TransferDown` are byte-identical in both modes, and that `Line` puts its underscores on the **bottom** line. Cover `display_with_borders` the same way. This is SC-001
- [ ] T010 [P] [US1] Add a cross-mode equality test to `src/diagram/tests.rs` over the eight sample knots already in `snapshot_ascii_print`: **derive** the transfer-free set by checking whether the `Standard` grid from `from_abbreviated` contains any of the six transfer variants, and for those knots assert `ascii_print::<false>(Standard) == ascii_print::<false>(OpeningCentered)`, plus the same for `ascii_print_compact::<false>` and `ascii_print::<true>`. Do not hard-code which knots qualify — deriving it keeps the test working as samples are added (research R7, FR-007, SC-003)
- [ ] T011 [P] [US1] Add normalisation tests to the `#[cfg(test)] mod tests` in `src/render.rs`: diagram text naming the retired characters parses without error under either mode; `to_text(OpeningCentered)` writes `_` for each of the eight; applying `to_text(OpeningCentered)` twice equals applying it once; and `to_text(Standard)` still round-trips all sixteen characters byte for byte for the existing `TREFOIL` and `UNKNOT` fixtures (FR-005, FR-016, SC-008)

### Implementation for User Story 1

- [ ] T012 [US1] Replace the ignored parameter from T003 with a real `match mode` in `Horiz::display` and `Horiz::display_with_borders` in `src/render.rs`, transcribing the `OpeningCentered` column of the cell table in `data-model.md` exactly. Turns T009 green
- [ ] T013 [US1] Make the trim in `VerboseDiagram::display` in `src/render.rs` mode-aware (research R6): `Standard` keeps today's rule — the last row emitted drops its final two lines — and `OpeningCentered` mirrors it at the top, so the **first** row emitted keeps only its last content line, plus the `+---` line when `GRID_BORDERS` is on so the picture keeps its top rule. Note that the bordered case is not a plain `skip(2)`: of the four lines `[border, l0, l1, l2]` the first row keeps indices 0 and 3
- [ ] T014 [US1] Add the opening-centered grid builder to `src/raw_lines.rs` beside the existing one — a `live: Vec<bool>` of levels, a column writer that fills a live level with `Line` unless its row is taken, `raise_once`/`lower_once` that move every live level at or above an index by one row in a single column, and `append_opening_centered`. Three rules decide correctness (research R3–R5, data-model "Validation rules"): a feature at abbreviated index `idx` sits at grid row `idx` alone; the cell directly above it — its *shadow row* — is `Empty` in that column; and filler cells come from `live`, never from the cell to the left, because `Horiz::subsequent` cannot see past a shadow row. Making room for an opening is two rise columns then the opening's own column; closing into an occupied stack is the closing's own column then two fall columns; neither may share a column with the feature
- [ ] T015 [US1] Dispatch on `mode` in `VerboseDiagram::from_abbreviated` in `src/diagram.rs`, calling `append` for `Standard` and `append_opening_centered` for `OpeningCentered`. Turns T010 green
- [ ] T016 [US1] Add `VerboseDiagram::to_text(&self, mode: RenderMode) -> String` to `src/render.rs`, mapping the eight retired variants to `Empty` before `as_byte` when the mode is `OpeningCentered` and otherwise behaving exactly as `Display` does; leave `impl Display` and `Horiz::{as_byte, from_byte}` untouched (research R8). Turns T011 green
- [ ] T017 [US1] Add `insta` snapshots of the opening-centered rendering for all eight sample knots to `src/diagram/tests.rs`, mirroring `snapshot_ascii_print`, then `cargo insta review` and commit the accepted snapshots under `src/diagram/snapshots/`
- [ ] T018 [P] [US1] Add a test to `src/diagram/tests.rs` asserting that `from_abbreviated(knot, OpeningCentered)` emits **no** retired variant for any of the eight sample knots (FR-005), and that the top grid row holds only `Line` and `Empty`
- [ ] T019 [P] [US1] Add a geometry test to `src/diagram/tests.rs`: for every sample knot the two modes produce the same **uncompacted** width and the same number of lines (SC-009), the opening-centered picture has no leading or trailing all-blank line that the `Standard` one lacks (FR-015), and for a knot that triggers a transfer each level of movement occupies exactly one column (FR-017)
- [ ] T020 [US1] Re-run the FR-008 guard: `cargo test` green with no pre-existing snapshot in `src/snapshots/` or `src/diagram/snapshots/` changed, and `cargo check --target wasm32-unknown-unknown` clean

**Checkpoint**: The library renders both ways and the transfer-free equality holds. Quickstart
scenarios 1–4, 6 and 7 pass. This is a complete, useful feature on its own — the mode simply cannot be
selected from either consumer yet.

---

## Phase 4: User Story 2 - Swap renderings in the app to hunt regressions (Priority: P1)

**Goal**: One toggle, shared by both app modes and remembered across reloads, redraws the diagram in
the other rendering.

**Independent Test**: Open the app, enter a knot, press the toggle, and confirm the picture is redrawn
opening-centered with the notation text untouched; switch to manual mode and confirm the toggle is
where you left it; reload and confirm it stuck.

### Tests for User Story 2

- [ ] T021 [P] [US2] Add persistence tests to `examples/knot-so-good/src/tests.rs`: state JSON with no `render_mode` key deserializes to `Standard`; a `PersistedState` carrying `OpeningCentered` round-trips through `serde_json`; and an unrecognised stored value deserializes to the `Other` variant and loads as `Standard` rather than discarding the whole saved state (FR-013)

### Implementation for User Story 2

- [ ] T022 [US2] Add `enum PersistedRenderMode { Standard, OpeningCentered, #[serde(other)] Other }` and a `#[serde(default)] render_mode: PersistedRenderMode` field to `PersistedState` in `examples/knot-so-good/src/main.rs`, following `PersistedDisplayMode` exactly, and map it in `PersistedState::from_model` (data-model "App persisted state")
- [ ] T023 [US2] Add `render_mode: knotty::RenderMode` to `Model` in `examples/knot-so-good/src/main.rs` and read it in `create`, mapping `PersistedRenderMode::{OpeningCentered => OpeningCentered, Standard | Other => Standard}` — one field for both app modes, not one per mode (FR-012)
- [ ] T024 [US2] Add `Msg::SetRenderMode(knotty::RenderMode)` in `examples/knot-so-good/src/main.rs` with an update arm returning `false` when the value is unchanged and otherwise setting the field, calling `update_modified()` so the notation-mode picture and its SVG are rebuilt, and returning `true` — matching the `Compact` arm
- [ ] T025 [US2] Add the toggle button to **both** views in `examples/knot-so-good/src/main.rs` — the notation control row beside the compact toggle, and `manual_view` beside the bordered toggle — labelled in the app's existing idiom, "switch to opening-centered view" / "switch to standard view", naming the view it moves to
- [ ] T026 [US2] Change `render_manual` in `examples/knot-so-good/src/main.rs` to `fn render_manual(diagram: &knotty::VerboseDiagram, mode: knotty::RenderMode, borders: bool) -> String` and pass `self.render_mode` from both call sites — the main picture and the snapshot previews — so a stale picture is redrawn in the selected mode as soon as the text is valid again
- [ ] T027 [US2] Make `update_modified` and `compact_text` in `examples/knot-so-good/src/main.rs` mode-aware: pass `self.render_mode` to `try_ascii_print[_compact]`, and in `compact_text` build the grid with `from_abbreviated(knot, self.render_mode)` and serialise it with `to_text(self.render_mode)` so the readout matches the picture beside it. Also pass the mode to the `display` call in `update_manual`'s `has_picture` check
- [ ] T028 [US2] Hand-verify scenario 8 steps 1–7 of `specs/003-opening-centered-render-mode/quickstart.md` against `examples/knot-so-good/` under `trunk serve`, including the shared-setting check (step 3), the stale-picture toggle (step 5), the reload (step 6) and the SVG display following the mode (step 7)

**Checkpoint**: The two renderings can be compared by eye, in either app mode, and the choice sticks.
This is the increment that delivers the feature's stated purpose.

---

## Phase 5: User Story 3 - Pick the rendering outside the app (Priority: P2)

**Goal**: The example program renders either way, in every combination with its existing options.

**Independent Test**: Run the example over a sample once per mode and confirm the two outputs, then
walk all eight combinations of the three display variables.

### Implementation for User Story 3

- [ ] T029 [US3] Read `KNOTTY_OPENING_CENTERED` in `examples/ascii_print.rs` alongside the existing `KNOTTY_GRID` and `KNOTTY_COMPACT`, mapping `Some("true")` to `RenderMode::OpeningCentered` and anything else to `Standard`, and pass the value through the existing four-arm `match (compact, grid)` rather than doubling it to eight — the mode is a runtime value, which is the whole point of research R1
- [ ] T030 [US3] Verify scenarios 2, 3 and 5 of `specs/003-opening-centered-render-mode/quickstart.md` against `examples/ascii_print.rs`: the trefoil sample renders identically in both modes (same `md5sum`), the square knot differs, and all eight combinations of the three variables produce well-formed output with no panic

**Checkpoint**: All three surfaces can select the mode. FR-010 and SC-007 hold.

---

## Phase 6: Polish & Cross-Cutting Concerns

- [ ] T031 [P] Document the rendering toggle in `examples/knot-so-good/README.md`: a paragraph beside the bordered-view section explaining what opening-centered draws, and a note on the manual-mode character table that under that view the eight characters `A a . , j r 2 L` are synonyms of `_` — accepted, drawn blank, written back as `_`
- [ ] T032 Run the full CI-parity command set from T001 and confirm every check passes, including `trunk build --release` in `examples/knot-so-good/`
- [ ] T033 Walk `specs/003-opening-centered-render-mode/quickstart.md` end to end and confirm every stated expectation, then mark the spec's Status as Implemented

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: no dependencies
- **Foundational (Phase 2)**: depends on Setup — **blocks every story**
- **US1 (Phase 3)**: depends on Foundational; blocks US2 and US3
- **US2 (Phase 4)**: depends on US1 — there is nothing to toggle until the library can draw both ways
- **US3 (Phase 5)**: depends on US1 for the same reason; independent of US2
- **Polish (Phase 6)**: depends on whichever stories shipped

### User Story Dependencies

The stories are **not** mutually independent. US1 is the rendering itself; US2 and US3 are two
selectors over it and are independent *of each other*, so they can be done in either order or
concurrently. US1 alone is a complete, demonstrable feature.

### Within Each User Story

- Tests before implementation in US1 (T009–T011 before T012–T016), with a genuine red phase
- In US1, T012 (cells) and T013 (trim) are independent of T014 (the grid); T015 needs T014; T017–T019
  need everything before them, since they measure the finished rendering
- In US2, persisted state → model field → message → view → call sites, in that order, because each
  references the previous
- Hand-verification tasks (T028, T030, T033) come after the code they check

### Parallel Opportunities

Most US2 tasks edit `examples/knot-so-good/src/main.rs` and are deliberately **not** marked `[P]`. The
genuine parallelism is:

- T009 (`src/render.rs`), T010 (`src/diagram/tests.rs`) and T011 (`src/render.rs` — sequence after T009,
  same file) at the start of US1
- T018 and T019 together, once T017 is done
- T021 (`examples/knot-so-good/src/tests.rs`) alongside T029 (`examples/ascii_print.rs`)
- T031 (`README.md`) alongside anything

---

## Parallel Example: User Story 1

```bash
# The two red-phase tests that live in different files:
Task: "Add the table-driven cell test for display(OpeningCentered) in src/render.rs"
Task: "Add the cross-mode equality test in src/diagram/tests.rs"

# Once the rendering is in, the two measurement tests are independent:
Task: "Assert no retired variant is ever emitted, in src/diagram/tests.rs"
Task: "Assert equal uncompacted width and no stray blank lines, in src/diagram/tests.rs"

# T012-T016 touch src/render.rs, src/raw_lines.rs and src/diagram.rs in a chain — run in sequence.
```

---

## Implementation Strategy

### MVP First (User Story 1 only)

1. Phase 1: Setup — green baseline
2. Phase 2: Foundational — thread `RenderMode`, behaviour-neutral
3. Phase 3: US1 — the rendering
4. **STOP and VALIDATE**: quickstart scenarios 1–4, 6, 7
5. This is already a complete library feature: both renderings exist and the transfer-free equality
   guarantee holds. It simply cannot be selected from the app or the CLI yet

### Incremental Delivery

1. Setup + Foundational → the mode is expressible everywhere, output unchanged
2. US1 → the library renders both ways → demo against the samples (MVP)
3. US2 → the two can be compared by eye in the app → demo
4. US3 → the CLI can produce either, in every combination → demo

---

## Notes

- `[P]` = different files, no dependencies
- Commit after each task or logical group, one logical change per commit, conventional-commit prefixes
- The FR-008 guard runs twice on purpose (T008, T020). An existing snapshot that moves is always a bug
  in this feature, never a snapshot to accept
- Do **not** add a second const generic. The mode is a runtime parameter; if an eight-arm `match`
  appears anywhere, research R1 is being ignored
- Do **not** let rotation see opening-centered output (T006). `rotate::scan_row` reads the Standard
  tile shapes by regex and would silently produce wrong notation
- Do **not** normalise retired characters at parse time. They stay distinct in `Horiz` and in
  `Display`; normalisation is a mode-aware serializer only (research R8, FR-009)
- The transfer difference between the modes is intended, not a regression. Uncompacted widths match;
  compacted, the opening-centered picture can be a few columns wider because the compact pass strips
  different columns (research R5)
