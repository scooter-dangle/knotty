# Implementation Plan: Height-Precalculated Strand Placement (Rendering Mode)

**Branch**: `claude/diagram-strand-height-precalc-p4l2lo` | **Date**: 2026-06-25 | **Spec**: [spec.md](./spec.md)

**Input**: Feature specification from `specs/001-strand-height-precalc/spec.md`

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
specs/001-strand-height-precalc/
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
