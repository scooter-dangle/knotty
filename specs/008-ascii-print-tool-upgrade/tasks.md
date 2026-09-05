# Tasks: ASCII Print Tool Upgrade

**Input**: Design documents from `/specs/008-ascii-print-tool-upgrade/`

**Prerequisites**: [plan.md](./plan.md), [spec.md](./spec.md), [research.md](./research.md), [data-model.md](./data-model.md), [contracts/cli-interface.md](./contracts/cli-interface.md), [contracts/succinct-round-trip.md](./contracts/succinct-round-trip.md), [quickstart.md](./quickstart.md)

**Tests**: Test tasks ARE included. Constitution Principle III (Test-First) is
binding for the one library change and one library addition this feature
makes (`try_ascii_print_compact`, `try_from_succinct_text`, both in
`src/diagram.rs`): write the test first, confirm it fails (or fails to
compile, for the not-yet-existing function), then implement.

## Organization

Tasks are grouped by user story (spec.md priorities P1–P4), per the
project's standard task organization. Unlike 007-strand-height-precalc, this
feature's stories genuinely are independent implementation increments — User
Story 1 alone is a complete, shippable MVP; each later story adds a
self-contained capability without changing an already-shipped story's
behavior *except* User Story 3, which changes `ascii_print_compact`'s output
width (research R7) — a deliberate, spec-anticipated change to the shared
succinct renderer, not a break of Story 1's contract (Story 1 promises
*succinct style by default*, not a specific column width).

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files or independent test
  functions, no dependency on an incomplete task)
- **[Story]**: Which user story this task serves (US1–US4); Setup and
  Foundational tasks carry none
- Include exact file paths in descriptions

## Path Conventions

Single Rust library crate. CLI in `examples/ascii_print.rs`; library changes
in `src/diagram.rs` (`try_ascii_print_compact`, new `try_from_succinct_text`)
and `src/rotate.rs` (new `scan_row` test cases); `insta` snapshots in
`src/diagram/snapshots/`.

**Constitution gate on every implementation task touching `src/`**: `cargo
check --target wasm32-unknown-unknown` must still pass (Principle II,
NON-NEGOTIABLE). No `Cargo.toml` entry beyond the two this feature explicitly
adds, and only as `[dev-dependencies]` (Principle V).

---

## Phase 1: Setup

**Purpose**: Bring in the new tooling without changing any behavior yet.

- [ ] T001 Add `clap` (version `4.6.6`, `features = ["derive"]`) and
      `clap_complete` (version `4.6.9`) to `[dev-dependencies]` in
      `Cargo.toml` (research R2); confirm `cargo build --example ascii_print`
      still succeeds with the file otherwise untouched
- [ ] T002 Capture the pre-change baseline: run `cargo test` and record the
      pass count plus the list of the 15 `ascii_print_compact`-driven
      snapshot files under `src/diagram/snapshots/` that research.md R9
      identifies as about to change, in the PR description
- [ ] T003 [P] Confirm `cargo check --target wasm32-unknown-unknown` passes
      against current `main` before any `src/` change, as the pre-change
      baseline for constitution II

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Stand up the clap-derived argument surface every story dispatches
through. No story-specific rendering behavior is wired yet.

**⚠️ CRITICAL**: no user story work begins until T006 passes.

- [ ] T004 Define the clap-derived `Cli` struct in
      `examples/ascii_print.rs` per `data-model.md`: positional `diagram:
      Option<PathBuf>` and `moves: Option<PathBuf>`; `--input-format
      <encoded|succinct>` (default `encoded`); `--style
      <succinct|full-spaced>` (default `succinct`); `--placement
      <precalculated-heights|index-aligned>` (default
      `precalculated-heights`); `--grid-borders` and `--echo-diagram` flags;
      `--completions <bash|zsh|fish|powershell|elvish>`. Every option
      long-form only — no `.short()` call anywhere in the struct (FR-010,
      FR-011)
- [ ] T005 Replace `examples/ascii_print.rs`'s `main` to call `Cli::parse()`
      and read `diagram`/`moves` the same way `read_input` does today
      (`-`/absent → stdin), leaving the actual dispatch on
      `--input-format`/`--style`/`--placement`/`--completions` as `todo!()`
      stubs for the stories below to fill in
- [ ] T006 Gate: `cargo build --example ascii_print`, `ascii_print --help`
      shows every option long-form with its default, `cargo check --target
      wasm32-unknown-unknown` (library only) still passes (depends on T004,
      T005)

**Checkpoint**: the CLI parses its full argument surface and shows correct
help; no diagram is rendered yet by any path.

---

## Phase 3: User Story 1 - Succinct output with precalculated placement by default (Priority: P1) 🎯 MVP

**Goal**: `ascii_print diagram.txt` with no other options prints the succinct
style, placed with `precalculated-heights` — no environment variables
needed (FR-001, FR-002, FR-003, FR-014).

**Independent Test**: quickstart.md Scenario 1 — run against the trefoil
fixture with no flags and confirm succinct + precalculated-heights output.

### Implementation for User Story 1

- [ ] T007 [US1] Wire the `--input-format encoded` path (the default) in
      `examples/ascii_print.rs`: parse `diagram` via
      `AbbreviatedDiagram::from_str`, apply `moves` via `try_apply_all` when
      given (unchanged from today — research R3), then call `.set_mode(...)`
      using `--placement` (default `precalculated-heights`) (FR-001, FR-002,
      FR-003)
- [ ] T008 [US1] Wire the `--style succinct` path (the default): call
      `AbbreviatedDiagram::ascii_print_compact::<GRID_BORDERS>()` with
      `GRID_BORDERS` selected by `--grid-borders`, then print the notation
      afterward when `--echo-diagram` is set — replacing
      `KNOTTY_GRID`/`KNOTTY_PRINT_ABBREV` with identical effect (FR-003,
      FR-014, contract C9)
- [ ] T009 [US1] Remove the old `std::env::var("KNOTTY_*")` reads from
      `examples/ascii_print.rs` entirely (contract C10 — this is an
      intentional breaking change, see plan.md Risks)
- [ ] T010 [US1] Run quickstart.md Scenario 1 by hand: confirm
      `ascii_print /tmp/trefoil.txt` with no flags matches today's
      `KNOTTY_COMPACT=true KNOTTY_PRECALC=true ascii_print /tmp/trefoil.txt`
      output exactly
- [ ] T011 [US1] Gate: `cargo build --example ascii_print`, `cargo check
      --target wasm32-unknown-unknown` (depends on T007, T008, T009)

**Checkpoint**: User Story 1 is fully functional and independently
shippable as the MVP.

---

## Phase 4: User Story 2 - Selecting the fully-spaced output style (Priority: P2)

**Goal**: `--style full-spaced` prints the uncompacted grid; omitting it
still defaults to succinct (FR-005, FR-006).

**Independent Test**: quickstart.md Scenario 2 — same trefoil fixture, once
with `--style full-spaced`, once without, confirming the two differ and the
default remains succinct.

### Implementation for User Story 2

- [ ] T012 [US2] Wire the `--style full-spaced` path in
      `examples/ascii_print.rs`: call
      `AbbreviatedDiagram::ascii_print::<GRID_BORDERS>()` instead of the
      compact variant, sharing US1's `--input-format encoded` parsing and
      `--grid-borders`/`--echo-diagram` handling unchanged (FR-005, FR-006)
- [ ] T013 [US2] Run quickstart.md Scenario 2 by hand: confirm
      `--style full-spaced` output differs from Scenario 1's, and that
      re-running without `--style` still reproduces Scenario 1 exactly (no
      regression to US1's default)
- [ ] T014 [US2] Gate: `cargo build --example ascii_print`, `cargo check
      --target wasm32-unknown-unknown` (depends on T012)

**Checkpoint**: User Stories 1 and 2 both work independently; the succinct
default is unaffected by the new style option existing.

---

## Phase 5: User Story 3 - Expanding a succinct diagram into the fully-spaced style (Priority: P3)

**Goal**: `--input-format succinct --style full-spaced` reconstructs
notation from previously-printed succinct text and renders it fully spaced,
preserving topology and (by default) placement (FR-007, FR-008, FR-009).

**Independent Test**: quickstart.md Scenario 3 — pipe Scenario 1's output
back in with `--input-format succinct --style full-spaced` and diff against
Scenario 2's direct output; they must be identical.

### Tests for User Story 3

> Write these FIRST; confirm they fail (or fail to compile, for T017 against
> a not-yet-existing function) before the implementation tasks below.

- [ ] T015 [P] [US3] Add round-trip unit tests in `src/diagram/tests.rs`
      asserting, for every fixture in `sample_knots()`, that
      `AbbreviatedDiagram::try_from_succinct_text(&d.ascii_print_compact::<false>())`
      returns tuples equal to `d.to_tuples()` (contracts/succinct-round-trip.md
      G1). Will not compile until T019 lands — that is expected under
      Test-First here, since the signature is fully specified by
      data-model.md
- [ ] T016 [P] [US3] Add unit tests in `src/diagram/tests.rs` for the
      `unknot` and `trefoil` fixtures capturing the new collapse-to-two-column
      behavior: derive the expected compact string by hand from each
      fixture's existing (unchanged) `ascii_print::<false>()` output, per
      research R7 — never from running the modified
      `ascii_print_compact`
- [ ] T017 [P] [US3] Add new `scan_row` unit tests in `src/rotate.rs`,
      alongside `mod test_scan_row`, fed hand-written collapsed-to-two-column
      input directly (not derived from a full-width fixture), per research
      R7's residual-risk note

### Implementation for User Story 3

- [ ] T018 [US3] Implement the collapse-to-two-column behavior in
      `AbbreviatedDiagram::try_ascii_print_compact`
      (`src/diagram.rs:1597-1627`): replace full deletion of each maximal
      all-blank column run with exactly two placeholder columns, preserving
      each row's own constant character across the run (research R7)
      (depends on T016)
- [ ] T019 [US3] Regenerate the 15 affected `insta` snapshots via `cargo
      insta review`, confirming every diff is exactly "wider blank run" and
      nothing else, per research R9 (depends on T018)
- [ ] T020 [US3] Implement `AbbreviatedDiagram::try_from_succinct_text` in
      `src/diagram.rs`, mirroring `try_rotate_90_ccw`'s scan loop
      (bottom-to-top row order, `scan_row(cur, prev)`,
      `Self::new_from_tuples(out)`) but without rotation's left-right
      character reversal (research R5) (depends on T018, T015)
- [ ] T021 [US3] Implement the trailing `# placement: <mode>` metadata line
      (research R6, data-model.md): appended after the diagram art whenever
      `--style succinct` is used in `examples/ascii_print.rs`; read back and
      stripped before calling `try_from_succinct_text` when
      `--input-format succinct`, supplying the default for `--placement`
      when the flag wasn't explicitly passed
- [ ] T022 [US3] Wire the `--input-format succinct` path in
      `examples/ascii_print.rs`: read `diagram`, apply T021's metadata
      handling, call `try_from_succinct_text`, then continue through the
      same `--style`/`--placement`/`--grid-borders`/`--echo-diagram`
      rendering path US1/US2 already wired (FR-007, FR-008)
- [ ] T023 [US3] Add the validation rule rejecting `--input-format succinct`
      combined with a `moves` positional argument, with a clear error naming
      the conflict (research R4, contract C5)
- [ ] T024 [US3] Run quickstart.md Scenario 3 by hand: confirm the `diff`
      against Scenario 2's direct full-spaced output is empty
- [ ] T025 [US3] Gate: `cargo test` (new round-trip tests, regenerated
      snapshots, new `scan_row` cases all passing), `cargo check --target
      wasm32-unknown-unknown` (depends on T019, T020, T022, T023)

**Checkpoint**: succinct text produced by this tool can be expanded back to
the fully-spaced style with a single command, faithfully.

---

## Phase 6: User Story 4 - Discoverable, script-friendly command line (Priority: P4)

**Goal**: every option is long-form only (already true from Phase 2) and
`--completions <shell>` emits a valid completion script (FR-012).

**Independent Test**: quickstart.md Scenario 4 — `--help` output and a
generated completion script.

### Implementation for User Story 4

- [ ] T026 [US4] Wire `--completions <shell>` in `examples/ascii_print.rs`:
      when present, use `clap_complete::generate` to print the completion
      script for the named shell to stdout and exit, before any
      diagram/input processing occurs (FR-012)
- [ ] T027 [US4] Make `--completions` mutually exclusive with every other
      option (clap `conflicts_with_all` or an `ArgGroup`), erroring clearly
      if combined with anything else (contract C7)
- [ ] T028 [P] [US4] Run quickstart.md Scenario 4 by hand: `--help` lists
      every option long-form only (already satisfied by Phase 2, re-verify
      here); `ascii_print --completions zsh` output passes `zsh -n`
- [ ] T029 [US4] Gate: `cargo build --example ascii_print`, `cargo check
      --target wasm32-unknown-unknown` (depends on T026, T027)

**Checkpoint**: all four user stories are independently functional.

---

## Phase 7: Polish & Cross-Cutting Concerns

- [ ] T030 [P] Grep the repo for any remaining mention of
      `KNOTTY_PRECALC`/`KNOTTY_GRID`/`KNOTTY_COMPACT`/`KNOTTY_PRINT_ABBREV`
      (docs, comments) and update or remove them, since contract C10 retires
      all four
- [ ] T031 Walk all four `quickstart.md` scenarios end-to-end in one sitting
      and confirm every pass condition holds together, not just per-story
- [ ] T032 Final gate: `cargo build`, `cargo test`, `cargo check --target
      wasm32-unknown-unknown`, and confirm `Cargo.toml` gained no dependency
      beyond `clap`/`clap_complete` under `[dev-dependencies]` (constitution
      II and V)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (1)**: no dependencies
- **Foundational (2)**: depends on Setup — BLOCKS all user stories; T006 is
  the gate
- **US1 (3)**: depends on Foundational only
- **US2 (4)**: depends on Foundational; independent of US1's tasks but
  naturally follows it (shares the rendering dispatch shape)
- **US3 (5)**: depends on Foundational; independent of US1/US2 code-wise, but
  its snapshot regeneration (T019) touches output US1/US2 also produce, so
  sequencing after US1/US2 avoids rebasing test expectations mid-story
- **US4 (6)**: depends on Foundational only — fully independent of US1/US2/US3
- **Polish (7)**: depends on all four stories

### User Story Dependencies

- **User Story 1 (P1)**: no dependency on other stories
- **User Story 2 (P2)**: no dependency on US1's tasks, but shares its
  parsing/mode-setting code path (T007) — implement after US1 to avoid
  duplicating that wiring
- **User Story 3 (P3)**: no dependency on US1/US2's tasks; changes the
  shared `ascii_print_compact` that US1's output also goes through (research
  R7) — this is a deliberate width change to a shared renderer, not a
  functional break of US1's contract
- **User Story 4 (P4)**: fully independent — touches only `--completions`,
  untouched by US1–US3

### Parallel Opportunities

- T001 and T003 (Setup) can run in parallel
- All of Phase 2 must complete before any story starts, but T004 and the
  `--help`/wasm checks in T006 have no story-specific dependencies to race
- T015, T016, T017 (US3 tests) can all be written in parallel — independent
  test functions in different files (`src/diagram/tests.rs`,
  `src/rotate.rs`)
- US4 (Phase 6) can run in parallel with US1–US3 once Phase 2 is done — it
  touches only the `--completions` branch

---

## Parallel Example: User Story 3's test-first tasks

```bash
# All three can be written at once, before any US3 implementation:
Task: "T015 Round-trip unit tests in src/diagram/tests.rs"
Task: "T016 Collapse-to-two-column unit tests in src/diagram/tests.rs"
Task: "T017 New scan_row unit tests in src/rotate.rs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL — blocks all stories)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: run quickstart.md Scenario 1
5. Ship — this alone is the "better defaults" ask from the feature request

### Incremental Delivery

1. Setup + Foundational → CLI skeleton ready, parses and shows help
2. Add User Story 1 → validate → ship (MVP)
3. Add User Story 2 → validate → ship
4. Add User Story 3 → validate (including full snapshot regeneration) → ship
5. Add User Story 4 → validate → ship
6. Polish

### Recommended Sequence (one person)

1. Phases 1–2 → dependencies, CLI skeleton
2. Phase 3 (US1) → the MVP, and the smallest path to "better defaults"
3. Phase 4 (US2) → nearly free once US1's dispatch exists
4. Phase 6 (US4) → also nearly free, fully independent — good filler between
   US2 and the heavier US3
5. Phase 5 (US3) → the substantial one: library change + new function +
   snapshot regeneration
6. Phase 7 → polish

### Risk Notes

**User Story 3 is where the real risk lives.** Everything else is
straightforward CLI wiring. Research R7's collapse-to-two-column change and
R5's reuse of `scan_row` are both backed by a proven invariant
(`Grid::column`'s no-event-no-change behavior) and existing, tested code
(`try_rotate_90_ccw`), but T017's new direct `scan_row` tests are the one
piece of that reasoning not yet exercised by any existing test — do not skip
them.

**Snapshot regeneration (T019) is mechanical, not optional.** Every diff
should be explainable as "a blank run got wider by exactly one or two
columns" — anything else is a real defect in T018, not noise to wave through
`cargo insta review`.

---

## Notes

- [P] = different files or independent test functions, no dependency on an
  incomplete task
- [Story] labels mark story-specific tasks; Setup, Foundational, and Polish
  tasks carry none
- Verify tests fail (or fail to compile, where noted) before implementing
- Commit per task or logical group, conventional-commit prefixes (`feat:`,
  `fix:`, `refactor:`, `test:`, `doc:`)
- Never wave through an unexplained snapshot diff — R9's collapse-to-two
  change has one specific expected shape
- Run `cargo check --target wasm32-unknown-unknown` before marking any
  `src/`-touching task done (Principle II, NON-NEGOTIABLE)
