# Tasks: ASCII Print Tool Upgrade

**Input**: Design documents from `/specs/008-ascii-print-tool-upgrade/`

**Prerequisites**: [plan.md](./plan.md), [spec.md](./spec.md), [research.md](./research.md), [data-model.md](./data-model.md), [contracts/cli-interface.md](./contracts/cli-interface.md), [contracts/succinct-round-trip.md](./contracts/succinct-round-trip.md), [quickstart.md](./quickstart.md)

**Revised during implementation** (2026-09-05): User Story 3's tasks were
rewritten after the original succinct-round-trip design (reusing `scan_row`
to reconstruct notation) was directly tested and found to compute a
*rotated* diagram, not the original one — see `research.md` R5/R10/R11. The
corrected design needs **no library change**, so the tasks that used to add
`try_from_succinct_text` and rework `ascii_print_compact` are gone; what
replaced them (T015–T018 below) is CLI-only, built on `VerboseDiagram`'s
existing, already-tested `to_text()`/`FromStr` round-trip.

**Tests**: No new `#[test]`/`insta` tasks are needed. This feature makes no
`src/` changes (constitution Principle III's Test-First applies to new
behavior in `src/`; there is none), and `examples/ascii_print.rs` follows
this codebase's existing convention of validating example binaries manually
via `quickstart.md` rather than with `#[test]`.

## Organization

Tasks are grouped by user story (spec.md priorities P1–P4). Each story is a
genuinely independent implementation increment — User Story 1 alone is a
complete, shippable MVP — and, unlike the original plan, no story changes
another's output: every story composes existing, unmodified library
functions.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files or independent, unrelated
  edits, no dependency on an incomplete task)
- **[Story]**: Which user story this task serves (US1–US4); Setup and
  Foundational tasks carry none
- Include exact file paths in descriptions

## Path Conventions

Single Rust library crate. This feature touches exactly one file besides
`Cargo.toml`: `examples/ascii_print.rs`. No file under `src/` is changed.

**Constitution gate on every task**: `cargo check --target
wasm32-unknown-unknown` must still pass (Principle II, NON-NEGOTIABLE) — true
by construction here, since `src/` is untouched, but still checked at every
gate per the constitution's own instruction. No `Cargo.toml` entry beyond the
two this feature explicitly adds, and only as `[dev-dependencies]`
(Principle V).

---

## Phase 1: Setup

**Purpose**: Bring in the new tooling without changing any behavior yet.

- [X] T001 Add `clap` (version `4.6.6`, `features = ["derive"]`) and
      `clap_complete` (version `4.6.9`) to `[dev-dependencies]` in
      `Cargo.toml` (research R2); confirm `cargo build --example ascii_print`
      still succeeds with the file otherwise untouched
- [X] T002 Capture the pre-change baseline: run `cargo test` and record the
      pass count plus the list of the 15 `ascii_print_compact`-driven
      snapshot files under `src/diagram/snapshots/`, in the PR description.
      **Note**: research R9 originally expected these to change; R10/R12
      correct that — this feature is expected to leave them byte-for-byte
      identical, and this baseline is what T027 confirms that against
      — **baseline: 110 tests passed, 0 failed**; snapshot list: 8
      `snapshot_ascii_print*`, 5 `snapshot_precalculated_heights*`, 2
      `snapshot_precalculated_heights_with_crossings*`
- [X] T003 [P] Confirm `cargo check --target wasm32-unknown-unknown` passes
      against current `main` before any change, as the pre-change baseline
      for constitution II

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
      `--completions <SHELL>` using `clap_complete::Shell` as the value type
      (its variants already match bash/zsh/fish/powershell/elvish). Every
      option long-form only — no `.short()` call anywhere in the struct
      (FR-010, FR-011)
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

**Goal**: `--input-format succinct --style full-spaced` recovers the exact
grid embedded in previously-printed succinct text and renders it fully
spaced (FR-007, FR-008, FR-009). Corrected design — see the note at the top
of this file and `research.md` R10/R11: no library change, no notation
reconstruction, just a hidden trailer built from `VerboseDiagram`'s existing
`to_text()`/`FromStr`.

**Independent Test**: quickstart.md Scenario 3 — pipe Scenario 1's output
back in with `--input-format succinct --style full-spaced` and diff against
Scenario 2's direct output; they must be identical.

### Implementation for User Story 3

- [ ] T015 [US3] Implement the succinct-output trailer in
      `examples/ascii_print.rs`: whenever `--style succinct` is used, after
      printing `ascii_print_compact::<GRID_BORDERS>()`, build
      `VerboseDiagram::from_abbreviated(&knot)?` and print each line of its
      `.to_text()`, each prefixed with the literal marker
      `# ascii_print-grid: ` (research R10, data-model.md)
- [ ] T016 [US3] Implement the succinct-input trailer parser in
      `examples/ascii_print.rs`: collect every `# ascii_print-grid:
      `-prefixed line from the input (in order), strip the marker, rejoin
      with `\n`, and parse the result with `str::parse::<VerboseDiagram>()`;
      return a clear error if no such line is found (data-model.md
      Validation Rules, contract C6)
- [ ] T017 [US3] Wire the `--input-format succinct` path in
      `examples/ascii_print.rs`: on `--style full-spaced`, render the
      recovered `VerboseDiagram` directly via `.display::<GRID_BORDERS>()`;
      on `--style succinct`, replicate `ascii_print_compact`'s existing
      blank-column-stripping loop (`src/diagram.rs:1597-1627`) as a small
      local helper over the recovered grid's rendered lines, since no
      `AbbreviatedDiagram` is available on this path (FR-007, FR-008,
      contract C4)
- [ ] T018 [US3] Add the three validation rules for `--input-format
      succinct`: reject a `moves` positional, reject an explicit
      `--placement`, and reject `--echo-diagram`, each with a clear error
      naming the conflict (research R11, contracts C5/C5a)
- [ ] T019 [US3] Run quickstart.md Scenario 3 by hand: confirm the `diff`
      against Scenario 2's direct full-spaced output is empty
- [ ] T020 [US3] Gate: `cargo build --example ascii_print`, `cargo check
      --target wasm32-unknown-unknown` (depends on T015, T016, T017, T018)

**Checkpoint**: succinct text produced by this tool can be expanded back to
the fully-spaced style with a single command, exactly.

---

## Phase 6: User Story 4 - Discoverable, script-friendly command line (Priority: P4)

**Goal**: every option is long-form only (already true from Phase 2) and
`--completions <shell>` emits a valid completion script (FR-012).

**Independent Test**: quickstart.md Scenario 4 — `--help` output and a
generated completion script.

### Implementation for User Story 4

- [ ] T021 [US4] Wire `--completions <shell>` in `examples/ascii_print.rs`:
      when present, use `clap_complete::generate` to print the completion
      script for the named shell to stdout and exit, before any
      diagram/input processing occurs (FR-012)
- [ ] T022 [US4] Make `--completions` mutually exclusive with every other
      option (clap `conflicts_with_all` or an `ArgGroup`), erroring clearly
      if combined with anything else (contract C7)
- [ ] T023 [P] [US4] Run quickstart.md Scenario 4 by hand: `--help` lists
      every option long-form only (already satisfied by Phase 2, re-verify
      here); `ascii_print --completions zsh` output passes `zsh -n`
- [ ] T024 [US4] Gate: `cargo build --example ascii_print`, `cargo check
      --target wasm32-unknown-unknown` (depends on T021, T022)

**Checkpoint**: all four user stories are independently functional.

---

## Phase 7: Polish & Cross-Cutting Concerns

- [ ] T025 [P] Grep the repo for any remaining mention of
      `KNOTTY_PRECALC`/`KNOTTY_GRID`/`KNOTTY_COMPACT`/`KNOTTY_PRINT_ABBREV`
      (docs, comments) and update or remove them, since contract C10 retires
      all four
- [ ] T026 Walk all four `quickstart.md` scenarios end-to-end in one sitting
      and confirm every pass condition holds together, not just per-story
- [ ] T027 Confirm the 15 snapshot files and pass count recorded in T002's
      baseline are still exactly 110 passed / byte-for-byte unchanged —
      the corrected US3 design (R10/R12) predicts zero snapshot impact, and
      this is what proves that prediction true rather than assumed
- [ ] T028 Final gate: `cargo build`, `cargo test`, `cargo check --target
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
- **US3 (5)**: depends on Foundational only — fully independent of US1/US2
  and, unlike the original plan, changes nothing either of them produces
- **US4 (6)**: depends on Foundational only — fully independent of US1/US2/US3
- **Polish (7)**: depends on all four stories

### User Story Dependencies

- **User Story 1 (P1)**: no dependency on other stories
- **User Story 2 (P2)**: no dependency on US1's tasks, but shares its
  parsing/mode-setting code path (T007) — implement after US1 to avoid
  duplicating that wiring
- **User Story 3 (P3)**: no dependency on US1/US2's tasks or output — the
  corrected design touches no shared library function, so it can genuinely
  run in parallel with them
- **User Story 4 (P4)**: fully independent — touches only `--completions`,
  untouched by US1–US3

### Parallel Opportunities

- T001 and T003 (Setup) can run in parallel
- All of Phase 2 must complete before any story starts
- Once Phase 2 is done, **US1, US3, and US4 can all run in parallel** —
  none of them shares mutable state with another (US2 is the one exception,
  sharing US1's parsing/mode code, so sequence it after US1)

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
4. Add User Story 3 → validate → ship
5. Add User Story 4 → validate → ship
6. Polish

### Recommended Sequence (one person)

1. Phases 1–2 → dependencies, CLI skeleton
2. Phase 3 (US1) → the MVP, and the smallest path to "better defaults"
3. Phase 4 (US2) → nearly free once US1's dispatch exists
4. Phase 6 (US4) → also nearly free, fully independent
5. Phase 5 (US3) → the trailer embed/extract logic — the most novel CLI code
   in this feature, though it touches no library function
6. Phase 7 → polish

### Risk Notes

**User Story 3 was where the real risk lived, and the risk has already been
retired.** The original design (reusing `scan_row`) was tested directly and
found wrong before any task here was written against it — see
`research.md` R5. The corrected design (T015–T018) builds on
`VerboseDiagram`'s `to_text()`/`FromStr`, which already has passing
round-trip tests in `src/render.rs`, unmodified by this feature. What
remains is ordinary CLI plumbing: embed a trailer, parse it back, handle the
three input-format validation rules.

**Zero snapshot impact is a claim to verify, not assume (T027).** Because a
previous version of this plan predicted snapshot changes that turned out
not to be needed, T027 exists specifically to confirm the corrected
prediction against the T002 baseline rather than take it on faith.

---

## Notes

- [P] = different files or independent, unrelated edits, no dependency on an
  incomplete task
- [Story] labels mark story-specific tasks; Setup, Foundational, and Polish
  tasks carry none
- Commit per task or logical group, conventional-commit prefixes (`feat:`,
  `fix:`, `refactor:`, `test:`, `doc:`)
- Run `cargo check --target wasm32-unknown-unknown` at every gate
  (Principle II, NON-NEGOTIABLE), even though this feature does not expect
  it to ever fail
