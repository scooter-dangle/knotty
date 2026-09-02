# Phase 0 Research: Retire the Split-Cell Rendering

**Feature**: [spec.md](./spec.md) | **Date**: 2026-09-02

All research was run against the working tree at `d318bdf`, with the full suite green (92 tests).

## R1 — Can rotation read the surviving rendering's pictures? (spec FR-008, FR-008a)

**Decision**: Point `AbbreviatedDiagram::full_render_lines` at the opening-centered rendering and
leave `rotate::scan_row` and its six regexes **completely unchanged**. No re-derivation is needed.

**Rationale**: This was the feature's one hard blocker, so it was settled by experiment rather than
by reading. Two experiments were run.

*Experiment 1 — flip the pin.* `full_render_lines` was switched from `RenderMode::Standard` to
`RenderMode::OpeningCentered` with no other change. The full suite passed: **92/92**, including all
eight rotation tests and the two rotation regression tests (`test_try_rotate_90_ccw_period_4`,
`test_try_rotate_90_ccw_period_4_regressions`, `rotate_then_render_out_of_bounds_regression`).

*Experiment 2 — differential over an exhaustive corpus.* Every valid abbreviated diagram of length
2–8 with height ≤ 8 was enumerated by DFS over the notation's own well-formedness rules (an opening
at `idx ≤ height` raises height by 2, a closing lowers it, a crossing needs `idx + 2 ≤ height`, and
the diagram closes at height 0). Each was rotated twice — once off the split-cell picture, once off
the opening-centered picture — and the resulting notation compared item for item:

| Measure | Count |
|---|---|
| Diagrams enumerated and rotated | **175,536** |
| Results identical between the two renderings | **175,536 (100%)** |
| Results that differed | **0** |
| Diagrams whose split-cell rendering contains transfer cells | 170,928 |
| …of those, differing | **0** |
| Rotations that succeeded under **both** renderings | 175,536 |
| Rotations that errored under both (trivial agreement) | **0** |

The last two rows are the control: agreement is not an artefact of both paths failing. Every one of
the 175,536 rotations produced a real result, and every pair matched.

**Why it holds.** Rotation does not read the picture a person sees. `full_render_lines` calls
`VerboseLine::display` directly, bypassing `VerboseDiagram::display` and therefore bypassing the
mode-dependent blank-line trimming. What rotation sees is the raw three-lines-per-row picture, and
there:

- *For transfer-free diagrams the two pictures are byte-identical up to a two-line shift.* The
  split-cell picture carries its content at the top with two trailing blank lines; the
  opening-centered picture carries the same content lines at the bottom with two leading blank
  lines. Measured on the unknot (6 lines each) and the trefoil (12 lines each), the content lines
  match exactly. The shift is invisible to `try_rotate_90_ccw`, which accumulates results in row
  order and lets blank rows contribute nothing; the only edge is the bottom content row, whose
  `prev` is a blank line in one mode and `None` in the other — and `scan_row` treats those alike,
  because a blank `prev` matches none of its `prev`-dependent patterns.
- *For diagrams with transfers the pictures genuinely differ*, as spec 003 says they must: the
  split-cell rendering climbs two levels over three columns, opening-centered climbs one level per
  column. But `scan_row` extracts only openings, closings and crossings, and it tracks depth by
  counting `(`, `)`, `/` and `\` characters along a row. The uncompacted picture is the same width
  in both renderings, and the diagonal passes through the same character columns on any given output
  line, so the depth counts come out the same. The stepping difference is in which *grid cell* owns
  a mark, which the picture text does not record.

**Alternatives considered**:

- *Re-derive the scan patterns against the opening-centered tile shapes*, which is what spec FR-008a
  currently mandates. Rejected: the evidence above shows there is nothing to re-derive. Doing it
  anyway would be the highest-risk work in the feature — rewriting a hand-tuned regex scanner that
  has already accumulated two regression fixes (commits `c60b408`, `27e03a6`) — in exchange for no
  behaviour change. **This requires an amendment to FR-008a; see *Spec amendments* below.**
- *Rework rotation to read the diagram's own representation instead of a picture.* Rejected as out
  of scope, and already recorded as an assumption in the spec. It remains the right long-term shape
  — a display concern should not gate a diagram operation — but it is a separate feature.
- *Keep the split-cell tile shapes privately for rotation.* Rejected by the user at `/speckit-specify`
  time, and now moot: nothing needs them.

**Consequence for phasing**: Phase 2 shrinks from "rewrite the scanner" to "flip one pin, guarded by
a differential test written first". The differential test is the deliverable that makes the flip
safe, and it is written in Phase 1 while both renderings still exist — which is exactly the ordering
the user asked for. It is deleted in Phase 3, when its second operand no longer exists.

## R2 — What actually comes out of the library

**Decision**: Removal is a subtraction, not a rewrite. `RenderMode` and every parameter carrying it
disappear; the opening-centered branch of each match becomes the only body.

**Rationale**: The mode is threaded through as an explicit parameter everywhere, so each site has a
mechanical resolution. Surveyed sites:

| Site | Today | After |
|---|---|---|
| `render.rs: RenderMode` | 2-variant enum | deleted |
| `render.rs: Horiz::display` | dispatches to two tables | `opening_centered_display` body, inlined |
| `render.rs: Horiz::display_with_borders` | dispatches to two tables | opening-centered body, inlined |
| `render.rs: Horiz::in_mode` | maps 8 variants to `Empty` under one mode | deleted (its 8 inputs are gone — R3) |
| `render.rs: VerboseDiagram::display` | mode-dependent blank-line filter | the opening-centered arm only |
| `render.rs: VerboseDiagram::to_text` | takes a mode, calls `in_mode` | takes no mode |
| `render.rs: Display for VerboseDiagram` | `to_text(Standard)` | `to_text()` |
| `diagram.rs: from_abbreviated` | two-arm match | `OpeningCentered` arm only |
| `diagram.rs: ascii_print{,_compact}, try_*` (6 fns) | each takes a mode | each takes none |
| `diagram.rs: full_render_lines` | pinned to `Standard` | no mode; comment retired |
| `raw_lines.rs` | `append`, `advance`, `expand_above`, `contract_above`, `is_empty_above` serve Standard; `OpeningCentered` serves the other | the Standard five deleted; `OpeningCentered` is the module |
| `render.rs: Horiz::subsequent` | used only by the Standard five | deleted |
| `examples/ascii_print.rs` | `KNOTTY_OPENING_CENTERED` env var | deleted |
| `knot-so-good/main.rs` | `PersistedRenderMode`, `Msg::SetRenderMode`, `render_mode` field, `render_mode_toggle` (rendered twice) | all deleted |

**Alternatives considered**: keeping `RenderMode` as a one-variant enum for future modes. Rejected —
the constitution's minimalism, and a single-variant enum is noise a later feature can reintroduce.

## R3 — The eight half-cells have no producer once Standard goes

**Decision**: Delete the eight variants from `Horiz`, delete their entries from `as_byte`/`from_byte`
and from the app's `SYMBOL_TABLE`, and let the parser reject their characters through its existing
unrecognised-character path.

**Rationale**: `CrossUpOver`, `CrossUpUnder`, `OpenedAbove`, `ClosedAbove`, `TransferUpStart`,
`TransferUpFinish`, `TransferDownStart` and `TransferDownFinish` are written into a grid in exactly
one place — `raw_lines::{append, expand_above, contract_above}`, all of which serve the split-cell
rendering and all of which are deleted in Phase 3. After that nothing can construct one. The parser
would still accept their characters, but only to produce a cell that draws nothing and normalizes
away, which is the situation spec FR-020 removes.

Characters freed, after spec 004's remapping: `A`, `a`, `'`, `,`, `j`, `r`, `2`, `L`. Surviving
eight: `.` `_` `x` `y` `(` `)` `/` `\`.

**Error path already exists.** `VerboseDiagram::from_str` reports an unrecognised character by name
with one-based row and column and stops at the first one — covered today by
`error_position_uses_input_line_numbers`, `error_position_is_one_based`,
`only_the_first_bad_character_is_reported` and `whitespace_is_rejected_like_any_other_unknown_character`.
Removing the eight from `from_byte` routes them into that path with no new code, which is what
FR-020a asks for. `unrecognized_bytes_have_no_mapping` gains the eight to its list.

**Alternatives considered**: keeping the eight as normalizing aliases. Rejected by the user; recorded
under *Clarifications* in the spec.

## R4 — Which tests and recorded pictures move, and how

**Decision**: Re-point, rename, restate, delete — in that order, split across the phases.

| Existing test | Fate |
|---|---|
| `snapshot_ascii_print` (8 snapshots, Standard) | deleted in Phase 3 |
| `snapshot_ascii_print_opening_centered` (8 snapshots) | renamed to `snapshot_ascii_print` in Phase 3; its `.snap` files renamed with it (insta derives the filename from the test name) |
| `transfer_free_knots_render_identically_in_both_modes` | deleted in Phase 3 — its subject is gone. Its non-comparative half (a transfer-free picture is unchanged) is already held by the renamed snapshots |
| `opening_centered_spends_two_columns_where_standard_spends_three` | **restated in Phase 1** as an absolute: a climb of N levels occupies N columns (spec FR-024, SC-009). The ratio form is deleted in Phase 3 |
| `opening_centered_never_emits_a_retired_cell` | survives Phase 3 unchanged; deleted in Phase 4, when the variants it names no longer exist. Its top-row assertion is kept by folding it into a surviving test |
| `transfer_cells_are_the_same_in_both_modes` | deleted in Phase 3 |
| `bordered_cells_are_the_plain_cells_behind_a_rule` | loops over both modes; the loop collapses to one pass. Survives |
| `opening_centered_cells_match_the_table` | renamed `cells_match_the_table`; survives as the FR-002/SC-001 check |
| `retired_cells_are_blank_in_opening_centered` | survives Phase 3; deleted in Phase 4 |
| `retired_characters_are_read_but_never_written_in_opening_centered` | replaced in Phase 4 by a test that each of the eight characters is now rejected with its position |
| `opening_centered_text_settles_in_one_pass` | its fixtures drop the retired-character cases in Phase 4; becomes a plain round-trip test |
| `standard_text_still_round_trips_every_character`, `to_text_matches_display_in_standard_mode` | deleted in Phase 3; round-trip coverage passes to `round_trips_through_text` over the surviving eight |
| `missing_render_mode_field_defaults_to_standard`, `unknown_render_mode_string_deserializes_to_other`, `round_trip_carries_the_render_mode` (app) | deleted in Phase 3, replaced by one test that a persisted state **containing** `render_mode` still loads and keeps its other settings (FR-012) |

**FR-012 is already satisfied structurally**: `PersistedState` does not set `deny_unknown_fields`, so
serde ignores a `render_mode` key it no longer knows. The replacement test pins that rather than
assuming it.

**Vacuity guard (FR-018, SC-005)**: the existing comparison test already asserts `checked >= 2`. The
absolute restatement in Phase 1 must carry the same guard, since it also filters its inputs.

## R5 — Recorded pictures in documentation

**Decision**: Regenerate the rendered knot in `README.md` in Phase 3.

**Rationale**: It is a split-cell picture of a knot containing transfers, so it genuinely changes.
Rendered with the surviving mode it becomes:

```
               ______   ___
              /      \ /   \
             /        /     \
            /     ___/ \     \
           /     /      \     \
          /     (        )     \
      ___/       \   ___/       \___
     /            \ /               \
    (              /                 )
     \   _________/ \____________   /
      \ /                        \ /
       \                          \
   ___/ \________________________/ \___
  /                                    \
 (                                      )
  \____________________________________/
```

Produced by `cargo run --example ascii_print` over the notation in the README's first block. The
first code block (the notation itself) is unchanged.

## R6 — Constitution compliance

- **Library-First**: every phase changes `src/` first; the app and the example program follow inside
  the same phase, since leaving them uncompilable would violate FR-005.
- **WASM**: `wasm32-unknown-unknown` is installed; `cargo check --target wasm32-unknown-unknown` runs
  at the end of every phase. No dependency changes, so no new risk.
- **Test-First**: Phase 1 is nothing *but* tests, landed before any deletion. Phases 2–4 each restate
  or re-point their tests before removing what they replace.
- **Minimal Dependencies**: nothing added; `regex` stays, still used by `scan_row`.
- **Notation Fidelity**: the abbreviated notation is untouched. FR-006 (identical rotation output) is
  the fidelity guarantee, evidenced by R1.

## Spec amendments arising from research

**FR-008a** asserts that the rotation read-back "MUST be re-derived against the surviving rendering's
tile shapes rather than adapted from the existing patterns". R1 disproves the premise: the existing
patterns already read the surviving rendering correctly, over an exhaustive 175,536-diagram corpus.
A requirement that mandates rewriting a working, twice-regression-fixed scanner for no behavioural
change is a defect in the spec, so FR-008a is amended to require the *evidence* rather than the
rewrite — the differential test that proves the existing patterns transfer. The user's decision at
clarification time is untouched: the read-back is pointed at the surviving rendering, and the
split-cell tile shapes are kept nowhere, publicly or privately. Only the assumption about how much
work that takes changes.
