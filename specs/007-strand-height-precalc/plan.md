# Implementation Plan: Height-Precalculated Strand Placement (Rendering Mode)

**Branch**: `claude/diagram-strand-height-precalc-p4l2lo` | **Date**: 2026-06-25 | **Spec**: [spec.md](./spec.md)

**Input**: Feature specification from `specs/007-strand-height-precalc/spec.md`

## ⚠️ Rebase impact — this design predates PRs #38–#44 (READ FIRST)

This plan, `research.md`, `data-model.md`, `contracts/public-api.md`,
`quickstart.md`, and `tasks.md` were written against the codebase at `d9a1f16`.
Main has since landed specs 001–006, two of which rewrote the exact code this
feature targets:

- **#40 (spec 003)** added the *opening-centered* rendering.
- **#42 (spec 005)** **retired the split-cell rendering entirely**, leaving one
  rendering and, in its own words, "no choice to make".

### What is now invalid

| Assumption in these docs | Reality on main |
|---|---|
| `raw_lines::{append, expand_above, contract_above, is_empty_above, advance}` are the placement path to extend | **All deleted.** `src/raw_lines.rs` is now `struct OpeningCentered { lines, live }` with `new`/`into_lines`/`column`/`raise_once`/`lower_once`/`append` |
| `RenderMode::Legacy` names the existing renderer | Two corrections. The mode this feature adds is a **placement** mode, not a rendering mode (Clarifications 2026-09-03) — `RenderMode` is the wrong name for the type. And its default variant names *the placement behavior the current renderer performs*, not the deleted split-cell renderer, so `Legacy` is the wrong name for the variant |
| `Legacy` routes through the untouched `append` path (research R5) | There is no such free-function path to route through; the placement logic now lives inside `OpeningCentered` (`live[]` + `raise_once`/`lower_once`). The parity baseline is today's output: existing placement + opening-centered mapping |
| A crossing is two half-glyphs on adjacent rows | A crossing is **one glyph in one cell** (`CrossDownOver`/`CrossDownUnder` at `idx` alone); `CrossUpUnder`/`CrossUpOver` are unused. This changes only the glyph mapping, so **FR-011 survives**: "partners must be on adjacent levels" is a placement constraint. Only prose describing a crossing as two half-glyphs needs rewording |
| Transfers use part-way-through-a-cell halves | `raise_once`/`lower_once` move **one whole level per cell**. Affects FR-004 and the SC-002 transfer counting |
| 16 existing snapshot files (tasks T002) | **24** |
| `src/raw_lines.rs:135`, `:21`, `:74` (tasks T022 and others) | All stale line references |

### What still holds

`AbbreviatedDiagram` is still a tuple struct at `src/diagram.rs:115` with 37
`self.0` sites, and no `RenderMode` exists — so the Phase 2 plumbing
(T003–T009) is unaffected. `height()`, `full_render_lines`, `try_rotate_90_ccw`,
`new_from_tuples`, `try_apply_all`, the `ascii_print` family, and `scan_row` all
survive with their signatures. The two-component seam in
[contracts/strand-heights.md](./contracts/strand-heights.md) is
architecture-level and survives intact; only Component B's *integration point*
moves from free functions to `OpeningCentered`.

### ~~Open product question~~ — RESOLVED 2026-09-03

An earlier revision of this section claimed a tension with spec 005, which
removed rendering-mode selection. **That was wrong.** Per Clarifications
2026-09-03, the two are orthogonal axes: opening-centered governs the *grid
mapping* (how an already-placed diagram becomes characters), while this feature
governs *placement* (which level each strand occupies). Adding a choice on the
placement axis does not reintroduce the rendering choice spec 005 removed. See
FR-014, which makes that independence a testable requirement.

### Newly opened by the rebase

A pair's two strands genuinely diverge: a pair opened at levels 3,4 followed by
`(4` ends up spanning levels **3 and 6**, because `raise_once(4)` and
`raise_once(5)` lift the upper strand twice while the lower stays. But an
opening draws a cap across two *adjacent* levels, so both strands cannot start
at their own maximum when those maxima differ. The spec covers non-adjacent
crossing partners (FR-011) but not this case. Being settled in clarification;
it also resolves the per-strand/per-pair Shape question in
[contracts/strand-heights.md](./contracts/strand-heights.md).

**Recommended**: re-run `/speckit-plan`, then `/speckit-analyze`, against current
main before `/speckit-implement`. The spec's requirements and success criteria
are largely unaffected; the technical design below is what needs revision.

## Summary

Add a second, opt-in diagram rendering mode that places each opening strand at the maximum vertical row it will ever occupy (precalculated from the abbreviated notation), so passing strands run flat instead of zig-zagging up and back down via transfer diagonals. The mode is an **operating context** carried on `AbbreviatedDiagram` (default = the existing legacy renderer), so rendering and the rotation move both honor it without changing any existing method signature. Because rotation re-derives notation by scanning the rendered grid, reducing avoidable (reversed-direction) transfers keeps the scanned feature count stable across repeated rotations — the motivating use case. Crossings whose partners are no longer adjacent under max-height placement are brought together with localized crossing-alignment transfers (which are scanned but do not inflate feature counts).

## Technical Context

**Language/Version**: Rust 1.94.0 (pinned in `rust-toolchain.toml`)

**Primary Dependencies**: `itertools`, `regex` (existing); no new runtime dependencies

**Storage**: N/A (in-memory diagram data structures)

**Testing**: `cargo test` with `insta` snapshot tests (existing) + `pretty_assertions`

**Target Platform**: native + `wasm32-unknown-unknown` (NON-NEGOTIABLE per constitution)

**Project Type**: Single Rust library crate (`knotty`) with example binaries under `examples/`

**Performance Goals**: Rendering remains O(features × height); the added precalculation pass is a single linear walk of the abbreviated sequence. No interactive-latency regression for the example app.

**Constraints**: Default-mode output must be byte-for-byte identical to today (protects existing `insta` snapshots); all `src/` code must compile for `wasm32-unknown-unknown`; abbreviated notation remains the source of truth.

**Scale/Scope**: Diagrams of tens–hundreds of features; one new public enum, a mode field + accessors on `AbbreviatedDiagram`, a new placement path in `raw_lines.rs`/`diagram.rs`, and snapshot coverage.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Library-First | ✅ PASS | All behavior lands in the core `knotty` crate (`src/`); the new `RenderMode` + mode-aware rendering is independently useful to downstream consumers. Example apps consume it but do not host it. |
| II. WASM-Compatible (NON-NEGOTIABLE) | ✅ PASS | Pure-logic change; no new deps, no `std`-only crates. Verified per task with `cargo check --target wasm32-unknown-unknown`. |
| III. Test-First | ✅ PASS | New diagram operation ⇒ `insta` snapshot tests authored alongside; regression tests added for `diagram.rs`/`rotate.rs`. Default-mode snapshots must remain unchanged (proves no regression). |
| IV. Notation Fidelity | ✅ PASS | Abbreviated notation stays the source of truth; the spec supplies example notation inputs (e.g. `terrace`), and round-trip/equivalence checks confirm the same knot. |
| V. Minimal Dependencies | ✅ PASS | No `Cargo.toml` additions. |

**Result**: No violations. Complexity Tracking table not required.

**Post-design re-check (after Phase 1)**: Still PASS. The design adds one enum
and a mode field with accessors, a linear precalculation pass, and a placement
path in `raw_lines.rs` — no new dependencies, no `std`-only crates, no GUI/CLI
coupling, and abbreviated notation stays authoritative. The tuple→named-struct
rename is mechanical and introduces no new abstraction beyond the `RenderMode`
the spec requires.

## Project Structure

### Documentation (this feature)

```text
specs/007-strand-height-precalc/
├── plan.md              # This file (/speckit-plan output)
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/
│   ├── public-api.md      # Phase 1 output (library API + behavioral contract)
│   └── strand-heights.md  # Internal contract: the seam between the two
│                          # independently-implementable components
├── checklists/
│   └── requirements.md  # Spec quality checklist (existing)
└── tasks.md             # Phase 2 output (/speckit-tasks)
```

### Source Code (repository root)

```text
src/
├── lib.rs           # CHANGE: re-export RenderMode
├── diagram.rs       # CHANGE: AbbreviatedDiagram gains `mode` field + accessors;
│                    #         from_abbreviated/full_render_lines/try_rotate read mode;
│                    #         self.0 → self.items mechanical rename
├── raw_lines.rs     # CHANGE: new max-height placement path (append/expand/contract variants)
├── render.rs        # CHANGE (likely none): Horiz glyphs reused; verify transfer glyphs suffice
├── rotate.rs        # CHANGE (likely none): scan_row unchanged; rotation feeds it mode-aware render
├── moves.rs         # CHANGE (likely none): Rotate90 dispatch already routes through try_rotate
└── snapshots/, diagram/snapshots/   # NEW snapshots for max-height mode; existing ones unchanged

examples/
├── ascii_print.rs           # OPTIONAL: expose a flag to select the mode
└── knot-so-good/            # OPTIONAL: expose mode toggle in the mini app
```

**Structure Decision**: Single-crate library (Option 1). The feature is implemented entirely in `src/`; the existing const-generic display flag (`GRID_BORDERS`) is orthogonal and untouched. The rendering mode is carried as a runtime field on `AbbreviatedDiagram` rather than a const generic, because the rotation move is dispatched at runtime via `DiagramMove::Rotate90CounterClockwise` and must honor the active mode through `try_apply_all` without changing the move API.
