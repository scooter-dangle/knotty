---

description: "Task list for the cell boundary view in manual diagram mode"
---

# Tasks: Cell Boundary View in Manual Diagram Mode

**Input**: Design documents from `/specs/002-diagram-boundary-view/`

**Prerequisites**: [plan.md](./plan.md), [spec.md](./spec.md), [research.md](./research.md),
[data-model.md](./data-model.md), [contracts/bordered-rendering.md](./contracts/bordered-rendering.md)

**Tests**: REQUIRED, not optional. Constitution Article III mandates test coverage for new behaviour
in `src/` and `insta` snapshots for diagram operations. Note the honest shape here: this feature adds
no library behaviour, so its tests **pin** an existing rendering rather than driving a new one — they
pass the moment they are written. That is stated per task; do not manufacture a red phase that the
code cannot produce.

**Organization**: Tasks are grouped by user story. The dependency shape is real, not decorative:
Phase 2 is a pure refactor that all three stories sit on, and US2 and US3 both build on US1's flag.
See [Dependencies](#dependencies--execution-order).

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: US1, US2, US3 — maps to the user stories in spec.md
- Exact file paths are given in every task

## Path Conventions

Single Rust crate at the repository root (`src/`), with a wasm example app under
`examples/knot-so-good/`. There is no `tests/` directory — the crate uses inline `#[cfg(test)]`
modules and `insta` snapshots under `src/snapshots/`; the app has host-target tests in
`examples/knot-so-good/src/tests.rs`.

---

## Phase 1: Setup

**Purpose**: Establish a green baseline so any later failure is attributable to this feature.

- [X] T001 Run the CI-parity command set from `.github/workflows/test.yml` (`cargo check --target wasm32-unknown-unknown`, `cargo build`, `cargo test`, then `cargo test` and `trunk build --release` in `examples/knot-so-good/`) and confirm all pass before changing anything

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Make both views derivable at any moment. Today `Model.manual_render` caches a rendered
`String`, which cannot be re-rendered the other way — so every story here is blocked on replacing that
cache with the last valid diagram (research R2, data-model.md). This phase changes **no behaviour**:
the app still renders exactly the plain picture it does now.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [X] T002 Change `render_manual` in `examples/knot-so-good/src/main.rs` to `fn render_manual(diagram: &knotty::VerboseDiagram, borders: bool) -> String`, branching `if borders { diagram.display::<true>().collect() } else { diagram.display::<false>().collect() }` — this is the feature's only dispatch over the `GRID_BORDERS` const generic (research R1); update both existing call sites to pass `false`
- [X] T003 Change `Model.manual_render` in `examples/knot-so-good/src/main.rs` from `Option<String>` to `Option<knotty::VerboseDiagram>`, and in `update_manual` store the parsed diagram itself, using `diagram.display::<false>().next().is_some()` in place of the current `!render.is_empty()` check — the two are equivalent and the new form allocates nothing (research R2); leave the `Err` arm exactly as it is, since not touching `manual_render` on a parse failure is what keeps the stale picture on screen
- [X] T004 Update `manual_view` in `examples/knot-so-good/src/main.rs` to render at view time — `render_manual(diagram, false)` for the main picture — keeping the existing `manual-render` / `manual-render stale` class logic untouched
- [ ] T005 Verify the refactor is behaviour-neutral: run `cargo test` in `examples/knot-so-good/`, then `trunk serve` and confirm manual mode still renders, still keeps a stale picture on a bad character, and still shows nothing for empty text

**Checkpoint**: The app holds the last valid diagram rather than a rendered string. Either view can now
be produced at any moment, including for a stale picture.

---

## Phase 3: User Story 1 - See which character drew which part of the picture (Priority: P1) 🎯 MVP

**Goal**: A control in manual mode redraws the picture with cell boundaries, one box per typed
character.

**Independent Test**: In manual mode, enter the trefoil text, turn the boundary view on, and confirm
the picture gains a 4-row × 7-column grid matching the 4 lines × 7 characters of text — and that the
text box is untouched.

### Tests for User Story 1

> These pin the contract in [contracts/bordered-rendering.md](./contracts/bordered-rendering.md).
> `VerboseDiagram::display::<true>()` already works, so they pass immediately — their job is to stop it
> changing under a feature that now shows it to users. No red phase to stage.

- [ ] T006 [P] [US1] Add an `insta` snapshot test to the `#[cfg(test)] mod tests` in `src/render.rs` covering `display::<true>()` for the trefoil parsed from `_(---)_\n_./-/,_\n(-A\A-)\n.--a--,\n`, then run `cargo insta review` and commit the accepted snapshot under `src/snapshots/`
- [ ] T007 [P] [US1] Add a geometry test to `examples/knot-so-good/src/tests.rs` asserting, for diagrams parsed from text of 1, 2 and 4 rows: total bordered lines are `4 × rows − 2` against `3 × rows − 2` plain, there is exactly one `+`-prefixed border line per row, each border line holds one `+---` group per cell, and `display::<false>()` and `display::<true>()` are empty for exactly the same inputs (`""` included) — this is SC-001 and the FR-008 precondition

### Implementation for User Story 1

- [ ] T008 [US1] Add `manual_borders: bool` to `Model` in `examples/knot-so-good/src/main.rs`, beside `compact`, and initialise it to `false` in `create` (persistence is wired in US2)
- [ ] T009 [US1] Add `Msg::ManualBorders(bool)` in `examples/knot-so-good/src/main.rs` with an update arm that returns `false` when the value is unchanged and otherwise sets the flag and returns `true` — matching the `Compact` and `DisplayMode` arms; it must **not** call `update_manual`, since the text and its parse result are unaffected (FR-004)
- [ ] T010 [US1] Add the toggle button to `manual_view` in `examples/knot-so-good/src/main.rs`, next to the `snapshot` button, labelled in the app's existing idiom — `switch to bordered view` / `switch to plain view`, naming the view it moves to (FR-017, research R6)
- [ ] T011 [US1] Pass `self.manual_borders` to `render_manual` for the main picture in `manual_view` in `examples/knot-so-good/src/main.rs` (FR-003, FR-005)

**Checkpoint**: The boundary view works end to end for the session. Quickstart scenario 4 passes; the
setting does not yet survive a reload.

---

## Phase 4: User Story 2 - The view survives mistakes and reloads (Priority: P2)

**Goal**: The chosen view is remembered across reloads and mode switches, and behaves correctly while
the picture is stale.

**Independent Test**: Turn the boundary view on, reload the page, and confirm manual mode returns with
the view still on and the same text.

### Tests for User Story 2

- [ ] T012 [P] [US2] Add persistence tests to `examples/knot-so-good/src/tests.rs`: state JSON with no `manual_borders` key deserializes with the flag `false` (FR-002, FR-014 — the guarantee that silently regresses if `#[serde(default)]` is ever dropped), and a `PersistedState` with `manual_borders: true` round-trips through `serde_json` (FR-012)

### Implementation for User Story 2

- [ ] T013 [US2] Add `#[serde(default)] manual_borders: bool` to `PersistedState` in `examples/knot-so-good/src/main.rs` and copy it in `PersistedState::from_model` — a plain `bool`, not an enum, since a two-state view has nothing to be forward-compatible with (research R5)
- [ ] T014 [US2] Read `manual_borders` from the loaded state in `Model::create` in `examples/knot-so-good/src/main.rs`, as a straight assignment with no enum mapping, replacing the `false` placeholder from T008
- [ ] T015 [US2] Verify the stale-toggle behaviour by hand per scenario 5 of `specs/002-diagram-boundary-view/quickstart.md`: with a stale bordered picture on screen, toggle the view and confirm the *same* picture is redrawn plain, still dimmed, with the error still beside it (FR-006, FR-007) — this is the check that fails if T003 was skipped
- [ ] T016 [US2] Verify per scenario 6 steps 4–5 of `specs/002-diagram-boundary-view/quickstart.md` that notation mode shows no boundary-view control and its own display controls are untouched (FR-015), and that switching modes and back preserves the setting (FR-013)

**Checkpoint**: The setting persists and behaves correctly around errors and mode switches.

---

## Phase 5: User Story 3 - Snapshots look like what is on screen (Priority: P3)

**Goal**: Manual-mode snapshot previews are drawn in whichever view is currently selected.

**Independent Test**: Take two manual-mode snapshots, toggle the view, and confirm every preview
follows.

### Implementation for User Story 3

- [ ] T017 [US3] Pass `self.manual_borders` to `render_manual` in the snapshot-preview loop of `manual_view` in `examples/knot-so-good/src/main.rs` (FR-010) — a one-argument change, since the previews already parse and render per view pass
- [ ] T018 [US3] Verify per scenario 6 steps 1–3 of `specs/002-diagram-boundary-view/quickstart.md` that previews follow the toggle and that restoring a snapshot changes the text but **not** the selected view (FR-011); confirm `PersistedManualSnapshot` still holds only `diagram`

**Checkpoint**: All three stories are complete and independently demonstrable.

---

## Phase 6: Polish & Cross-Cutting Concerns

- [ ] T019 [P] Document the boundary view in the "Manual diagram mode" section of `examples/knot-so-good/README.md`, alongside the existing character reference
- [ ] T020 Self-review the diff against the plan's constraints: `ascii_diagram_to_html`'s byte allow-list in `examples/knot-so-good/src/main.rs` is unnarrowed (it must keep `+` and `|`, research R4), no CSS was added to `examples/knot-so-good/index.html` (research R6), no new dependency in either `Cargo.toml`, and no library behaviour changed — the only `src/` diff is the test and its snapshot
- [ ] T021 Run the full CI-parity command set from T001 plus `cargo check --manifest-path examples/knot-so-good/Cargo.toml --target wasm32-unknown-unknown`, and confirm all pass (constitution Article II)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: no dependencies
- **Foundational (Phase 2)**: depends on Setup — **blocks all three stories**, because every story
  renders through the helper T002 changes and reads the cache T003 changes
- **US1 (Phase 3)**: depends on Phase 2
- **US2 (Phase 4)**: depends on US1 — there is no setting to persist until T008 adds it, and T015's
  stale-toggle check needs T011's toggle
- **US3 (Phase 5)**: depends on US1 for the same reason; independent of US2
- **Polish (Phase 6)**: depends on whichever stories shipped

### User Story Dependencies

Unlike a typical feature, the stories here are **not** mutually independent: US2 and US3 are both
refinements of the single control US1 introduces. They are independent *of each other* and can be
done in either order or concurrently. US1 alone is a complete, demonstrable feature.

### Within Each User Story

- Tests before implementation where a test can exist (T006/T007 before T008–T011; T012 before
  T013/T014). These tests pass on first run — they pin existing library behaviour and the serde
  contract; that is expected, not a sign the test is wrong.
- Model field → message → view, in that order, because each references the previous.
- Hand-verification tasks (T015, T016, T018) come after the code they check.

### Parallel Opportunities

Most implementation tasks touch `examples/knot-so-good/src/main.rs`, so they are deliberately **not**
marked `[P]`. The genuine parallelism is between test files:

- T006 (`src/render.rs`) and T007 (`examples/knot-so-good/src/tests.rs`)
- T012 (`examples/knot-so-good/src/tests.rs`) alongside any US3 work in `main.rs`
- T019 (`README.md`) alongside anything

---

## Parallel Example: User Story 1

```bash
# The two US1 tests live in different files and share nothing:
Task: "Add an insta snapshot test for display::<true>() in src/render.rs"
Task: "Add the bordered geometry test in examples/knot-so-good/src/tests.rs"

# T008–T011 all edit examples/knot-so-good/src/main.rs — run them in sequence, not in parallel.
```

---

## Implementation Strategy

### MVP First (User Story 1 only)

1. Phase 1: Setup — green baseline
2. Phase 2: Foundational — the cache refactor, behaviour-neutral
3. Phase 3: US1 — the toggle
4. **STOP and VALIDATE**: quickstart scenario 4
5. This is already a complete, useful feature: the view just resets on reload

### Incremental Delivery

1. Setup + Foundational → both views derivable
2. US1 → toggle works → demo (MVP)
3. US2 → it is remembered and survives errors → demo
4. US3 → the snapshot catalog agrees with the picture → demo

Phase 2 is worth committing on its own: it is a pure refactor with no behaviour change, so it reviews
cleanly and isolates any regression it might cause from the feature that follows.

---

## Notes

- `[P]` = different files, no dependencies
- Commit after each task or logical group, one logical change per commit, conventional-commit prefixes
- The `GRID_BORDERS` const generic means exactly one `if borders` branch should exist in the whole
  diff (T002). If a second appears, the flag is being threaded too far
- Do not "fix" the picture's open right and bottom outer edge — that is a library behaviour change,
  explicitly out of scope (spec Assumptions, research R3)
- Do not add a `manual_borders` field to `PersistedManualSnapshot` (FR-011)
