# Implementation Plan: Retire the Split-Cell Rendering

**Branch**: `claude/speckit-rendering-mode-migration-cbdxrp` | **Date**: 2026-09-02 | **Spec**: [spec.md](./spec.md)

**Input**: Feature specification from `/specs/005-retire-standard-rendering/spec.md`

## Summary

Remove `RenderMode::Standard` — the rendering that splits a knot's features across two vertically
adjacent cells — leaving the opening-centered rendering from spec 003 as the only one, and then
remove the eight `Horiz` variants that only it ever drew.

Four phases, in the order the user asked for: verify, unblock rotation, remove the mode, remove the
half-cells. Phase 1 deletes nothing — it lands the tests and recorded pictures that will have to
stand in for the split-cell ones, while both renderings still exist and every existing test still
passes. Phase 2 clears the one hard dependency. Phases 3 and 4 are then subtraction.

**The feature's open question is settled.** Rotation recovers notation by rendering a diagram and
reading the picture back with regexes that encode the split-cell tile shapes, so whether those
patterns could read opening-centered pictures decided how large this feature was. Phase 0 research
answered it by experiment: over **every valid diagram of length 2–8 and height ≤ 8 — 175,536 of them,
170,928 containing strand transfers, all rotating successfully under both renderings — the two
produce identical notation in 100% of cases**, and the existing suite passes unchanged with the pin
flipped. `src/rotate.rs` therefore needs no edit at all; Phase 2 is a one-line re-point, guarded by a
differential test written first. See [research.md](./research.md) R1, and the amendment it forced on
spec FR-008a.

## Technical Context

**Language/Version**: Rust, channel pinned in `rust-toolchain.toml` (1.94.0)

**Primary Dependencies**: None new, none removed. `regex` stays — `rotate::scan_row` still uses it.
`insta` for recorded pictures; `pretty_assertions` in tests; `yew` + `serde` in the example app.

**Storage**: Browser local storage, for the app's persisted state only. The `render_mode` key stops
being written; states that still carry it must keep loading (FR-012).

**Testing**: `cargo test` (92 passing at `d318bdf`); `insta` snapshots under `src/diagram/snapshots/`
and `src/snapshots/`; `cargo insta review` to accept renames.

**Target Platform**: `src/` must compile for `wasm32-unknown-unknown` (constitution II); the example
app is a wasm browser app; `examples/ascii_print` is a native CLI.

**Project Type**: Library (`knotty`) with two example surfaces — a CLI and a browser app.

**Performance Goals**: N/A. Removal only; no algorithm changes. The surviving grid builder is the one
already in use for opening-centered rendering.

**Constraints**: Every phase must leave the workspace building, `cargo test` green, and both example
surfaces compiling (FR-005, SC-007). Rotation output must not change (FR-006). Pictures must be
byte-for-byte what opening-centered produces today (FR-009).

**Scale/Scope**: ~6,200 lines across `src/` and `examples/`. Touched: `src/render.rs` (1,029),
`src/diagram.rs` (1,616), `src/raw_lines.rs` (312), `src/diagram/tests.rs` (372),
`src/diagram/test.rs` (552), `examples/ascii_print.rs` (65), `examples/knot-so-good/src/main.rs`
(1,066) and its `tests.rs` (379), plus `README.md` and 16 snapshot files. `src/rotate.rs` (372) and
`src/moves.rs` (441) are **not** touched.

## Constitution Check

*GATE: must pass before Phase 0 research, re-checked after Phase 1 design.* Both passes clean; no
entries in Complexity Tracking.

| Principle | Assessment |
|---|---|
| **I. Library-First** | Every phase changes `src/` first. The two example surfaces follow inside the same phase, because a phase that left them uncompilable would break FR-005. No new public API — the surface only shrinks ([contracts/public-api.md](./contracts/public-api.md)). |
| **II. WASM-Compatible** (non-negotiable) | No dependency changes, so no new risk. `cargo check --target wasm32-unknown-unknown` is in every phase's gate ([quickstart.md](./quickstart.md)); the target is installed. |
| **III. Test-First** | Phase 1 is *only* tests, landed before any deletion — the constitution's ordering and the user's request coincide. Phases 2–4 restate or re-point each test before removing what it replaces. Snapshot coverage never dips: the opening-centered recordings exist before the split-cell ones go. Rotation is a `rotate.rs` behaviour, so its regression tests must keep passing with expectations unchanged (they do). |
| **IV. Notation Fidelity** | The abbreviated notation is untouched and remains the source of truth. FR-006 — identical rotation output — is the fidelity guarantee, evidenced by R1's 175,536-diagram differential. |
| **V. Minimal Dependencies** | Nothing added. `Cargo.toml` unchanged. |

## Project Structure

### Documentation (this feature)

```text
specs/005-retire-standard-rendering/
├── plan.md              # This file
├── research.md          # Phase 0: the rotation experiment, removal surface, test fates
├── data-model.md        # Phase 1: RenderMode removed, Horiz 16 -> 8, signature changes
├── quickstart.md        # Phase 1: per-phase validation gates
├── contracts/
│   └── public-api.md    # Phase 1: library, CLI and app contract, before and after
├── checklists/
│   └── requirements.md  # From /speckit-specify
└── tasks.md             # Phase 2 output (/speckit-tasks — NOT created here)
```

### Source Code (repository root)

```text
src/
├── lib.rs               # drop RenderMode from the re-exports                    [P3]
├── render.rs            # delete RenderMode, in_mode, subsequent, the split-cell
│                        #   tile tables and the mode-dependent trim; drop 8 Horiz
│                        #   variants and their bytes                          [P3, P4]
├── raw_lines.rs         # delete append/advance/expand_above/contract_above/
│                        #   is_empty_above; OpeningCentered becomes the module     [P3]
├── diagram.rs           # drop the mode parameter from 10 entry points; re-point
│                        #   full_render_lines                                 [P2, P3]
├── rotate.rs            # UNTOUCHED — scan_row and its regexes stay as they are
├── moves.rs             # UNTOUCHED
├── diagram/
│   ├── tests.rs         # differential + absolute tests in; comparisons out [P1, P3, P4]
│   ├── test.rs          # drop the mode argument from assertion macros           [P3]
│   └── snapshots/       # delete 8 split-cell .snap; rename 8 opening-centered   [P3]
└── snapshots/           # raw_lines/render snapshots follow their tests      [P3, P4]

examples/
├── ascii_print.rs       # drop KNOTTY_OPENING_CENTERED                           [P3]
└── knot-so-good/src/
    ├── main.rs          # drop PersistedRenderMode, SetRenderMode, the toggle
    │                    #   (rendered twice), and 8 SYMBOL_TABLE rows        [P3, P4]
    └── tests.rs         # replace the three render_mode tests with one
                         #   legacy-state test                                [P3, P4]

README.md                # regenerate the rendered knot                           [P3]
```

**Structure Decision**: The existing layout is kept exactly. This feature adds no module and moves no
file; it deletes from the files above. `src/rotate.rs` and `src/moves.rs` are listed only to record
that they are deliberately untouched — `rotate.rs` because R1 showed its patterns already read the
surviving rendering, `moves.rs` because it never referenced a rendering.

## Phase plan

Each phase maps to one user story and is independently landable and revertable.

### Phase 1 — Verify before deleting (US1, P1)

*Deletes nothing. Additions only — `git diff --stat` should show no removals in `src/` or `examples/`.*

1. **Audit** every remaining split-cell use into the feature directory: library drawing paths, tests,
   the 8 split-cell snapshots, rotation, `ascii_print`, the app, and the README picture. R2 and R4 in
   [research.md](./research.md) are that audit; the task is to confirm it against the tree and record
   anything it missed (FR-002).
2. **Differential rotation test** — rotate a wide generated corpus both ways and assert the notation
   matches, including transfer-carrying diagrams, with a control that both sides really succeeded
   (FR-008a). This is what makes Phase 2 safe, and it is deleted in Phase 3 when its second operand
   goes.
3. **Restate the comparative invariants absolutely**, so nothing is lost when the comparisons go:
   a climb of N levels occupies N columns (FR-024, replacing the "two columns where standard spends
   three" ratio), each carrying a guard that it measured at least one case (FR-018).
4. Confirm the opening-centered snapshots already cover every knot the split-cell ones do (they do —
   both families are driven by `sample_knots()`), so FR-003 is satisfied for pictures by inspection.

**Gate**: full suite green with the additions; `RenderMode::Standard` still the default everywhere.

### Phase 2 — Free rotation (US2, P1)

5. Point `full_render_lines` at the surviving rendering and retire its "pinned to Standard" comment.
   `src/rotate.rs` is not edited.

**Gate**: every rotation test and both regression tests pass with expectations unchanged; the Phase 1
differential test still passes (both renderings still exist here, so it can still run).

### Phase 3 — One rendering (US3, P1)

6. **Library**: delete `RenderMode`; collapse each two-arm match to its opening-centered body; drop
   the parameter from the ten entry points in [contracts/public-api.md](./contracts/public-api.md);
   delete `Horiz::in_mode`, `Horiz::subsequent`, and the five split-cell functions in `raw_lines.rs`.
7. **Recorded pictures**: delete the eight split-cell snapshots; rename
   `snapshot_ascii_print_opening_centered` to `snapshot_ascii_print` and its `.snap` files with it
   (insta derives the filename from the test name), so the surviving recordings are named for what
   they record (FR-016).
8. **Comparison tests out**: delete the ones whose subject is gone — the identical-picture test, the
   column-ratio test, `transfer_cells_are_the_same_in_both_modes`,
   `standard_text_still_round_trips_every_character`, `to_text_matches_display_in_standard_mode`, and
   the Phase 1 differential test (FR-017). Their surviving stand-ins landed in Phase 1.
9. **Example program**: drop `KNOTTY_OPENING_CENTERED`; the other three variables and all four
   compact × borders combinations stay (FR-013).
10. **App**: drop `PersistedRenderMode`, the `render_mode` field, `Msg::SetRenderMode`,
    `Model::render_mode` and both renderings of the toggle. Replace the three `render_mode` tests
    with one asserting a pre-feature state carrying `render_mode` still loads with its other settings
    intact (FR-012 — structurally already true, since `PersistedState` does not set
    `deny_unknown_fields`; the test pins it).
11. **README**: regenerate the rendered knot (FR-019); the expected output is in research R5.

**Gate**: `grep -rn "RenderMode\|render_mode\|KNOTTY_OPENING_CENTERED" src examples README.md` returns
nothing (SC-001, SC-013); full suite green; both wasm checks pass; the app's four manual checks in
[quickstart.md](./quickstart.md) pass.

### Phase 4 — Remove the half-cells (US4, P2)

12. Delete the eight `Horiz` variants and their `as_byte`/`from_byte` entries; the parser's existing
    unrecognised-character path then rejects `A a ' , j r 2 L` by name and position with no new code
    (FR-020, FR-020a).
13. Shrink `SYMBOL_TABLE` to eight rows (FR-022); confirm an app snapshot whose text uses a freed
    character reports as invalid without taking the app down (FR-020b).
14. Retire the tests that name the eight (`retired_cells_are_blank_in_opening_centered`,
    `opening_centered_never_emits_a_retired_cell`, the read-but-never-written test), folding their
    surviving content — the top-row assertion, the round trip over the eight survivors — into tests
    that remain; add the eight characters to `unrecognized_bytes_have_no_mapping`.

**Gate**: full suite green; the freed characters are rejected with position; text over the surviving
eight round-trips byte for byte (FR-021, SC-012).

## Risks

| Risk | Mitigation |
|---|---|
| The rotation re-point breaks a diagram outside the researched bounds (length > 8 or height > 8) | The Phase 1 differential test is the guard, and it is landed *before* the re-point. If it is ever extended and fails, Phase 2 reverts to one line. |
| Snapshot rename loses history or silently changes content | Rename the test and `git mv` the `.snap` files in the same commit; the diff must show pure renames, with file contents unchanged. |
| A comparison test is deleted before its absolute restatement exists | Phase ordering: every restatement lands in Phase 1, every deletion in Phase 3 or later. FR-003 and SC-004 make this the gate. |
| The app's persisted-state compatibility is assumed rather than checked | Phase 3 replaces the removed tests with one that deserializes a real pre-feature state. |
| A phase lands with the app uncompilable because the library moved first | Each phase's gate includes `cargo check --package knot-so-good --target wasm32-unknown-unknown`. |

## Complexity Tracking

No constitution violations. Table omitted.
