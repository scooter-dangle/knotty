# Implementation Plan: Height-Precalculated Strand Placement (Placement Mode)

**Branch**: `claude/spec-kit-feature-spec-001-psrhij` | **Replanned**: 2026-09-03 against `origin/main` @ `37b7c09` | **Spec**: [spec.md](./spec.md)

**Input**: Feature specification from `specs/007-strand-height-precalc/spec.md`

## Summary

Add an opt-in **placement mode** that computes, for every strand, the maximum row
it will occupy over its flat run, and opens it directly there — so a strand that
today climbs and descends as other pairs open and close beneath it instead runs
flat. Caps, cups and crossings are drawn at the floored midpoint of the two
strands they join, splitting the unavoidable boundary movement between them.

The mode is an **operating context** on `AbbreviatedDiagram` (default = today's
behavior), so rendering and rotation honor it without any signature change.
Because rotation re-derives notation by scanning the rendered grid, removing
reversed-direction transfers is expected to stop repeated rotation from
compounding artifacts — the motivating use case, and the feature's central
hypothesis (SC-006).

**Placement is orthogonal to the grid mapping.** The opening-centered rendering
(#40, sole survivor after #42) governs how an already-placed diagram becomes
characters. This feature governs which level each strand occupies. Adding a
choice on the placement axis does not reintroduce the rendering choice #42
removed — FR-014 makes that independence testable.

## Replanning note

The pre-rebase revision of this plan was written against `d9a1f16`, before
PRs #38–#44. Its R3 and R5 were built around
`raw_lines::{append, expand_above, contract_above}`, which #42 deleted along with
the split-cell rendering. Everything below is rewritten against current `main`.
Two of its claims are now known false and are corrected in place:

| Old claim | Correction |
|---|---|
| "Total diagram height is unchanged" | Height can **increase** — research R7 |
| Default variant named `Legacy` | The legacy *rendering* is gone; this axis's default is `IndexAligned` — research R1 |

## Technical Context

**Language/Version**: Rust 1.94.0 (pinned in `rust-toolchain.toml`)

**Primary Dependencies**: `itertools`, `regex` (existing); no new runtime dependencies

**Storage**: N/A (in-memory diagram structures)

**Testing**: `cargo test` with `insta` snapshots and `pretty_assertions`, plus
**golden fixtures** — five owner-supplied input/output pairs covering 63
features, in [fixtures/](./fixtures/). Correctness is asserted against those;
snapshots provide regression coverage on top.

**Target Platform**: native + `wasm32-unknown-unknown` (NON-NEGOTIABLE per constitution)

**Project Type**: Single Rust library crate (`knotty`) with example binaries

**Performance Goals**: rendering stays O(features × height); the added
precalculation is two linear walks of the abbreviated sequence. No interactive
latency regression for the example app.

**Constraints**: default-mode output byte-for-byte identical (protects all 24
existing snapshots); all `src/` compiles for wasm32; abbreviated notation remains
the source of truth.

**Scale/Scope**: diagrams of tens–hundreds of features; one public enum, a mode
field plus accessors, a two-pass height calculation, a second placement builder
in `raw_lines.rs`, and fixture-driven tests.

## Architecture

The feature decomposes at a pure-function seam, documented in
[contracts/strand-heights.md](./contracts/strand-heights.md):

```text
         Component A                          Component B
  encoding → per-strand maxima  ──maxima──▶  encoding + maxima → grid
  (pure; no grid, no glyphs)                 (placement + midpoint rules)
```

Component B is built and tested against **fixture-supplied** maxima, never
against A's output, so the two halves can be developed independently and a defect
in one cannot mask a defect in the other. They meet at the contract; an
integration check asserts A's output equals the maxima B's fixtures supply.

### Component A — height calculation

A strand's height is one more than the tallest thing ever beneath it
(research R2):

```text
height(s) = 0                                     if no strand is ever below s
height(s) = 1 + max{ height(t) : t ever below s }  otherwise
```

Simulate the ordered stack of live strands (crossings do not reorder levels),
record the immediately-below relation among adjacent neighbours, and take the
memoized longest path. Adjacent edges suffice — the full below-relation gives
identical heights, so edge collection is O(depth) per feature, not O(depth²).

Heights are **absolute**; a pair's gap is a consequence (`upper − lower − 1`),
never an input. Two earlier attempts to compute the gap directly were wrong, one
of them agreeing with all 23 pairs in all five fixtures — R2 keeps them on record
so they are not rederived.

Verified: reproduces all five fixtures exactly, and returns the correct gap for
`(0 (1 )1 (1 )1 )0`, the case that falsified the earlier formula.

### Component B — render from heights

`src/raw_lines.rs` currently entangles two concerns in `OpeningCentered`:
`column()` and the `Horiz` values are grid mapping; `live`, `raise_once`,
`lower_once` and `append` are placement. Extract the grid state and `column()`
into a shared inner struct and let two placement builders drive it —
`OpeningCentered` unchanged, plus a new `PrecalculatedHeights` builder. One
extraction, no trait, no generics. Sharing the emitter is what makes FR-014 true
by construction rather than by test.

B also owns the **logical level → rendered row** mapping. A notation index names
a level among currently-live strands, which under this mode is not a grid row;
A never sees rendered rows.

## Constitution Check

*GATE: must pass before Phase 0 and again after Phase 1.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Library-First | ✅ PASS | All behavior lands in `src/`; `PlacementMode` is independently useful downstream. Example apps consume, never host. |
| II. WASM-Compatible (NON-NEGOTIABLE) | ✅ PASS | Pure logic, no new deps. Verified per task with `cargo check --target wasm32-unknown-unknown`. |
| III. Test-First | ✅ PASS | Golden fixtures written before implementation; `insta` snapshots for the new diagram operation; regression tests required for any `rotate.rs` change. |
| IV. Notation Fidelity | ✅ PASS | Abbreviated notation stays authoritative; fixtures supply notation inputs and expected renders; equivalence checks confirm the same knot. |
| V. Minimal Dependencies | ✅ PASS | No `Cargo.toml` additions. |

**Result**: no violations; Complexity Tracking not required.

**Post-design re-check**: still PASS. The design adds one enum, a mode field with
accessors, a two-pass calculation, and one placement builder. The single
extraction (grid state + `column()`) is required by FR-014 and replaces no
existing abstraction. Nothing couples the library to a GUI or CLI surface.

## Project Structure

### Documentation (this feature)

```text
specs/007-strand-height-precalc/
├── plan.md                    # This file
├── spec.md                    # Requirements (7 clarifications integrated)
├── research.md                # Phase 0 — R1–R7
├── data-model.md              # Phase 1
├── quickstart.md              # Phase 1 — validation scenarios
├── contracts/
│   ├── public-api.md          # Public surface + behavioral guarantees C1–C11
│   └── strand-heights.md      # Internal seam between Components A and B
├── fixtures/                  # Five golden fixtures, 63 features, all verified
│   ├── README.md
│   ├── rotated-5_1.md
│   ├── square-knot.md
│   ├── non-adjacent-crossing.md
│   ├── little-dumb-link.md
│   └── square-knot-links-encircled.md
├── checklists/requirements.md # Spec quality checklist (16/16)
└── tasks.md                   # Phase 2 output (/speckit-tasks — regenerate)
```

### Source Code (repository root)

```text
src/
├── lib.rs           # CHANGE: re-export PlacementMode
├── diagram.rs       # CHANGE: AbbreviatedDiagram tuple → named struct { items, mode }
│                    #         + mode()/set_mode()/with_mode(); self.0 → self.items (37 sites)
│                    #         from_abbreviated (:118) dispatches on mode
│                    #         full_render_lines (:895) renders under self.mode
├── raw_lines.rs     # CHANGE: extract grid state + column() from OpeningCentered;
│                    #         add height calculation (A) and placement builder (B)
├── render.rs        # CHANGE: none expected — existing Horiz variants suffice
├── rotate.rs        # CHANGE: none expected — scan_row is shape-based (research R6)
├── moves.rs         # CHANGE: none expected — Rotate90 already routes via try_apply
└── snapshots/, diagram/snapshots/   # 24 existing frozen; new ones for the new mode

examples/
├── ascii_print.rs           # OPTIONAL: mode flag
└── knot-so-good/            # OPTIONAL: mode toggle
```

**Structure Decision**: single-crate library. The mode is a runtime field rather
than a const generic because `DiagramMove::Rotate90CounterClockwise` dispatches at
runtime and must honor the active mode through `try_apply_all` without changing
the move API. The existing `GRID_BORDERS` const generic is orthogonal and
untouched.

## Risks

| Risk | Assessment |
|---|---|
| Component A's height algorithm | **Resolved.** The owner's longest-path rule reproduces all five fixtures and the counterexample (research R2). Residual: add regression tests for sequential siblings and for a sibling stacked above a divergent pair — neither is covered by a supplied fixture, and the first is what distinguishes the correct rule from the plausible wrong one. |
| Crossing-alignment construction | **Retired.** Was the highest-uncertainty area; [non-adjacent-crossing](./fixtures/non-adjacent-crossing.md) now specifies it by example. |
| `scan_row` under the new geometry | **Low.** Its regexes match local glyph shapes and its indices come from counters along a scan line, neither row-dependent — deliberately so, per the feature owner. New-geometry regression tests are still worth having: #28 and #31 were defects in realizing that general design, each found by a specific diagram. |
| Height growth surprising callers | **Medium.** Callers assuming `height()` bounds rendered rows are correct only under `IndexAligned`. Documented in C-consequences; the example app should be checked. |
| SC-006 not fully achieved | **Open by design.** SC-006 is the feature's hypothesis, not an established result. If repeated rotation still compounds, the feature is still valuable for FR-003/FR-004 but the motivating claim needs revisiting. |
