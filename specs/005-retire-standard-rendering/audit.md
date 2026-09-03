# Audit: every remaining use of the split-cell rendering

**Feature**: [spec.md](./spec.md) | **Gate**: FR-002, FR-003, SC-004 — no deletion may land against an open entry.

Seeded from [research.md](./research.md) R2 and R4, then checked against the tree at `ee3030c`.
Status is one of **covered** (a replacement already exists), **US1** (a replacement lands in Phase 3),
or **open** (no replacement possible — blocks the deletion that depends on it).

## Library drawing paths

| # | Site | Replacement | Status |
|---|---|---|---|
| 1 | `src/render.rs` `RenderMode` enum (~24–29) | deleted; no replacement needed | covered |
| 2 | `src/render.rs` `Horiz::display` dispatch (45–50) | `opening_centered_display` inlined | covered |
| 3 | `src/render.rs` `standard_display` (53–135) | deleted | covered |
| 4 | `src/render.rs` `Horiz::display_with_borders` dispatch (195–204) | `opening_centered_display_with_borders` inlined | covered |
| 5 | `src/render.rs` `standard_display_with_borders` (206–303) | deleted | covered |
| 6 | `src/render.rs` `Horiz::in_mode` (398–417) | deleted with its 8 inputs (Phase 6) | covered |
| 7 | `src/render.rs` `Horiz::subsequent` (371–396) | deleted — only the split-cell builder called it | covered |
| 8 | `src/render.rs` `VerboseDiagram::display` mode filter (~535–545) | opening-centered arm only | covered |
| 9 | `src/render.rs` `VerboseDiagram::to_text` mode param (549) | mode dropped | covered |
| 10 | `src/render.rs` `Display for VerboseDiagram` (~619) | calls one-arg `to_text` | covered |
| 11 | `src/raw_lines.rs` `is_empty_above`/`advance`/`expand_above`/`contract_above`/`append` (5–170) | `OpeningCentered` is the surviving builder | covered |
| 12 | `src/diagram.rs` `from_abbreviated` two-arm match (118–143) | opening-centered arm only | covered |
| 13 | `src/diagram.rs` 4 methods + 4 free fns taking a mode (1525–1612) | mode dropped ([contracts/public-api.md](./contracts/public-api.md)) | covered |
| 14 | `src/diagram.rs` `full_render_lines` pinned to Standard (909–927) | re-pointed in Phase 4 | covered |
| 15 | `src/lib.rs` `RenderMode` re-export (10) | dropped | covered |

## Rotation

| # | Site | Replacement | Status |
|---|---|---|---|
| 16 | `src/rotate.rs` `scan_row` + 6 regexes | **no entry needed** — R1 establishes over 175,536 diagrams that the existing patterns read opening-centered pictures identically. Not edited. | covered |
| 17 | Evidence that #16 holds | differential test, T006–T007 | US1 |
| 18 | `src/moves.rs` | **no entry needed** — never referenced a rendering | covered |

## Recorded pictures

| # | Site | Replacement | Status |
|---|---|---|---|
| 19 | `snapshot_ascii_print` + 8 `.snap` (split-cell) | `snapshot_ascii_print_opening_centered` + its 8, renamed onto these names | covered — see #20 |
| 20 | Do the two families cover the same knots? | **Yes.** Both iterate `sample_knots()` (8 knots) with `ascii_print_compact::<false>`. Confirmed by reading `src/diagram/tests.rs` — the only difference is the mode argument. No new snapshot needed (T011). | covered |
| 21 | `snapshot_from_abbreviated` + `.snap` | snapshots a *split-cell grid debug*; contents legitimately change → **re-record**, not rename | covered |
| 22 | `snapshot_parsed_diagram_render_with_borders` + `.snap` | renders parsed text bordered in split-cell mode; contents change → **re-record** | covered |
| 23 | `snapshot_raw_lines_append` (+4 `.snap`), `snapshot_raw_lines_expand_contract` (+2 `.snap`) | drive the split-cell builder; deleted. **Replacement needed** — see #29 | US1 |
| 24 | `src/snapshots/knotty__snapshot_raw_lines_expand_above.snap` | orphan (`source: src/lib.rs`), matches no current test. Pre-existing cruft, separable from this feature | covered |

## Tests

| # | Site | Replacement | Status |
|---|---|---|---|
| 25 | `transfer_free_knots_render_identically_in_both_modes` | subject gone; its non-comparative half is held by the renamed snapshots (#19) | covered |
| 26 | `opening_centered_spends_two_columns_where_standard_spends_three` | absolute restatement: N levels ⇒ N columns | US1 (T008) |
| 27 | `transfer_cells_are_the_same_in_both_modes` | subject gone | covered |
| 28 | `standard_text_still_round_trips_every_character`, `to_text_matches_display_in_standard_mode` | round-trip coverage passes to `round_trips_through_text` over the surviving eight | covered |
| 29 | `src/raw_lines.rs` has **no test of the surviving `OpeningCentered` builder** — its only two tests drive the split-cell functions, so #23 would leave the file with zero coverage | new snapshot test of `OpeningCentered` | US1 (T009) |
| 30 | `bordered_cells_are_the_plain_cells_behind_a_rule` loops over both modes | loop collapses to one pass; test survives | covered |
| 31 | `opening_centered_cells_match_the_table`, `retired_cells_are_blank_in_opening_centered`, `opening_centered_never_emits_a_retired_cell` | survive Phase 5; retired or folded in Phase 6 | covered |
| 32 | `src/diagram/test.rs` mode args in the two rotate macros (~40, 87, 216, 284, 288, 296, 427, 431, 439, 540) | mode dropped; expectations unchanged | covered |
| 33 | `missing_render_mode_field_defaults_to_standard`, `unknown_render_mode_string_deserializes_to_other`, `round_trip_carries_the_render_mode` (app) | one test that a legacy state carrying `render_mode` still loads | US1 (T010) |

## Diagram-text fixtures (Phase 6 only)

| # | Site | Replacement | Status |
|---|---|---|---|
| 34 | `src/render.rs` test consts `UNKNOT` (800–803) and `TREFOIL` (805–814) are **split-cell text using the retired `A a ' ,`**; ~13 tests depend on them | opening-centered rewrites, computed and verified: `UNKNOT = "..\n()\n"`, `TREFOIL = "..___..\n.(._.).\n._y.y_.\n(__x__)\n"` | covered |
| 35 | Scattered retired-char literals: ragged fixture (869–870), `parse("()\n',\n")` and siblings (884, 889, 894, 898), error fixture (910), canonical fixture (1007–1008) | rewritten alongside #34 | covered |
| 36 | `retired_characters_are_read_but_never_written_in_opening_centered` (741), `opening_centered_text_settles_in_one_pass` fixtures (750) | replaced by a rejection-with-position test | covered |
| 37 | `ALL_HORIZ` (16) and `RETIRED` (8) test constants | `ALL_HORIZ` shrinks to 8; `RETIRED` deleted | covered |

## Surfaces and documentation

| # | Site | Replacement | Status |
|---|---|---|---|
| 38 | `examples/ascii_print.rs` `KNOTTY_OPENING_CENTERED` (37–41) | deleted; the other three vars and all four compact × borders combinations stay | covered |
| 39 | `knot-so-good/src/main.rs` `PersistedRenderMode` (17–24), `PersistedState.render_mode` (57, 78–81), `Msg::SetRenderMode` (138, 604–612), `Model::render_mode` (172, 535–540), `render_mode_toggle` (274–288) and its two call sites (328, 865) | all deleted | covered |
| 40 | `knot-so-good/src/main.rs` `SYMBOL_TABLE` 16 rows (209–230) | shrinks to 8 (Phase 6) | covered |
| 41 | `README.md` rendered knot | regenerated; expected output in research R5 | covered |

## Resolution

**41 entries. 0 open.** Four are marked US1 and land in Phase 3 before any deletion: the differential
rotation evidence (#17), the absolute climb-cost restatement (#26), the `OpeningCentered` builder
snapshot (#29), and the legacy persisted-state test (#33).

Two entries were **not** in the plan's research and were found by reading the tree — #29 (raw_lines
would lose all coverage) and #34–35 (the split-cell test fixtures). Both are now covered.

Phase 3 may begin.

---

## Acceptance walk (Phase 7)

Every Success Criterion, checked against the tree after the last phase.

| SC | Claim | Result |
|---|---|---|
| SC-001 | Nothing names or selects a rendering | `git grep RenderMode\|KNOTTY_OPENING_CENTERED -- src examples README.md` → no matches |
| SC-002 | Pictures byte-identical to the opening-centered ones before the change | all 8 renamed recordings identical to the copies taken at T002 |
| SC-003 | Rotation results unchanged | every change in `src/diagram/test.rs` is a dropped mode argument — no expectation edited; all rotation tests and all three regressions pass |
| SC-004 | No deletion against an open audit entry | 41 entries, 0 open before Phase 3; the four US1 entries landed in Phase 3, deletions began in Phase 5 |
| SC-005 | Assertion count does not drop | 118 tests at baseline → 110 after. The 8 fewer are comparison tests whose subject no longer exists; each invariant they held is asserted by a surviving test (`a_climb_costs_one_column_per_level`, `pictures_are_rectangular_and_end_flush`, `snapshot_opening_centered_*`, `state_saved_with_a_render_mode_still_loads`, `freed_characters_are_rejected_with_their_position`) |
| SC-006 | Verification phase green with both renderings present | Phase 3 landed 270 insertions, 0 deletions in `src/`/`examples/`, 124 tests passing |
| SC-007 | Each phase green on its own | six commits, each with both suites and both wasm checks passing |
| SC-008 | Pre-feature app state still loads | `state_saved_with_a_render_mode_still_loads` |
| SC-009 | Compact × borders still reachable in all four combinations | verified from `ascii_print`; the app keeps its two toggles |
| SC-010 | Surviving text round-trips byte for byte | `round_trips_through_text`, plus a direct check over the trefoil text |
| SC-011 | Only cells that draw | `Horiz` carries 8 variants |
| SC-012 | Freed characters rejected with position | all eight → `unrecognized character 'X' at line 2, column 2` |
| SC-013 | Split-cell tile shapes nowhere in the project | `git grep` finds no `Standard` or `standard_display`; `src/rotate.rs` and `src/moves.rs` are byte-identical to their pre-feature state |

### Corrections made during implementation

1. **`quickstart.md` app commands.** `cargo --package knot-so-good` fails — the app is a separate crate, not a workspace member. Corrected to `--manifest-path`, and every gate now runs both suites, since the root `cargo test` does not cover the app.
2. **`quickstart.md` Phase 4 rejection check.** It proposed feeding a freed character to `ascii_print`, which parses *knot notation*, not diagram text — it yields a notation error, not the diagram-text parser's message. Replaced with the library test.
3. **Fixture rewrites moved from Phase 6 to Phase 5.** Audit entries #34–35 were scheduled with the character removal, but removing the *mode* already invalidated them: `from_abbreviated` builds opening-centered grids, so the split-cell `UNKNOT`/`TREFOIL` constants stopped describing what the library produces. Three app fixtures had the same problem.
4. **`Horiz::in_mode` carried behaviour, not just dispatch.** It also performed spec 003's normalization of undrawn cells during serialization. Deleting it with the mode would have changed text behaviour in Phase 5, so it became `Horiz::drawn` (no mode) there and was deleted in Phase 6 with the cells themselves.
5. **New entry: `bordered_render_draws_one_box_per_character` and `both_views_are_empty_for_the_same_diagrams`** (app) held split-cell fixtures the audit had not listed. Rewritten in Phase 5.
6. **New entry: the app's snapshot catalog** previewed an unparseable snapshot as an empty picture — silent discarding, which FR-020b forbids. It now reports "unreadable snapshot".
