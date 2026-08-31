# Implementation Plan: Opening-Centered Rendering Mode

**Branch**: `claude/opening-centered-diagram-mode-d150sm` | **Date**: 2026-08-31 | **Spec**: [spec.md](./spec.md)

**Input**: Feature specification from `/specs/003-opening-centered-render-mode/spec.md`

## Summary

Add a second way to draw a diagram in which every feature is whole inside one cell, selectable at
runtime alongside the existing rendering, and reachable from the library, the example program and
the app.

The work has two halves that must agree with each other:

1. **A second grid.** Opening-centered is not a re-skin of the current cell grid. In the current
   rendering a feature at abbreviated index `idx` occupies grid rows `idx` *and* `idx + 1` — the
   glyph at `idx + 1` and its partner half at `idx`. Opening-centered it occupies row `idx` alone.
   Swapping tile tables under the existing grid draws a disconnected picture (verified — see
   [research.md](./research.md) R2), so the mode needs its own builder in `raw_lines.rs`.
2. **A second tile table**, plus the trimming that goes with it. The opening-centered picture sits
   two text lines lower inside its grid than the current one, so the current rendering trims the
   bottom row's last two lines and opening-centered trims the top row's first two.

Threading the mode is the one API decision with teeth. `GRID_BORDERS` is a *const* generic, and the
mode also has to reach `VerboseDiagram::from_abbreviated`, which is not generic at all. Making the
mode a second const generic would express one concept two ways and put an eight-arm match in every
consumer, so the mode is a runtime `RenderMode` parameter instead (R1). That costs a mechanical
argument at existing call sites and keeps every consumer's branch count at one.

The grid rules were prototyped and checked against reality before being written down: a port of the
current builder reproduces all eight checked-in `ascii_print` snapshots byte for byte, and the
opening-centered builder run beside it confirms the spec's central claim — transfer-free knots render
*identically* in both modes, full and compact (R7). Knots containing transfers differ only in how the
diagonal is stepped, at equal uncompacted width.

## Technical Context

**Language/Version**: Rust 1.94.0 (pinned in `rust-toolchain.toml`)

**Primary Dependencies**: none added. Core crate keeps `itertools` + `regex`; the example app keeps
`yew 0.23`, `web-sys`, `js-sys`, `wasm-bindgen`, `svgbob`, `serde`, `serde_json`.

**Storage**: browser `localStorage` under the existing `knotty_state` key (example app only); one new
enum field on the persisted state, with an `Other` fallback variant matching `PersistedDisplayMode`.

**Testing**: `cargo test` with `insta` snapshots and `pretty_assertions`; the example app has
host-target unit tests in `examples/knot-so-good/src/tests.rs` (no browser harness).

**Target Platform**: `wasm32-unknown-unknown` (constitution Article II) plus the host target for tests.

**Project Type**: Rust library with a wasm example app and a CLI example consuming it.

**Performance Goals**: rendering runs on every keystroke in the app. Both modes are a linear walk over
the same number of cells with a `match` per cell; the opening-centered builder tracks a `Vec<bool>` of
live levels instead of reading the previous column. Diagrams are tens of rows by tens of columns, so
no budget is at risk.

**Constraints**:

- `RenderMode` must be paired consistently: a grid built for one mode is only meaningful drawn in that
  mode. The type system will not catch a mismatch (R3), so the pairing is a documented contract and a
  test.
- `rotate::scan_row` recognises the *current* tile shapes with regexes, so `full_render_lines` — and
  therefore `try_rotate_90_ccw` — stays on the current rendering whatever mode is selected (R9). This
  resolves the item deferred by `/speckit-clarify`.
- Existing snapshots are the regression guard for FR-008: not one of them may change.
- `ascii_diagram_to_html`'s allow-list must not be narrowed. Opening-centered draws from the same glyph
  set, so no change is needed there (R10).
- Trimming is unconditional in both modes, matching the current behaviour for hand-written text where a
  feature sits in the trimmed row.
- No new dependency in either `Cargo.toml` (constitution Article V).

**Scale/Scope**: one new public enum, a second tile table, a second grid builder (~120 lines in
`raw_lines.rs`), a mode argument threaded through `render.rs` and `diagram.rs`, one env var in the CLI
example, and one persisted field plus one button in the app.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Article | Verdict | Evidence |
|---|---|---|
| I. Library-First | PASS | `RenderMode`, the second tile table and the second builder all land in `src/` and are useful to any consumer; the app and CLI are thin selectors over them. |
| II. WASM-Compatible (NON-NEGOTIABLE) | PASS | Pure computation over `Vec<Horiz>` and `&'static str`; no new dependency, nothing `std`-only. `cargo check --target wasm32-unknown-unknown` is a task. |
| III. Test-First | PASS | Every new behaviour is covered: a table-driven cell test (SC-001), `insta` snapshots of the new rendering, a cross-mode equality test (FR-007), a round-trip test (FR-016), a transfer-column test (SC-009), and the untouched existing snapshots (FR-008). |
| IV. Notation Fidelity | PASS | Abbreviated notation is unchanged and remains the source of truth; the spec carries notation inputs with expected output in both modes. |
| V. Minimal Dependencies | PASS | No new entry in either `Cargo.toml`. |

Re-checked after Phase 1: still PASS. The design adds one public enum and two methods; no new module,
no new crate, no new abstraction beyond what FR-017 and the differing grid geometry force.

## Project Structure

### Documentation (this feature)

```text
specs/003-opening-centered-render-mode/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/
│   └── opening-centered-rendering.md
├── checklists/
│   └── requirements.md
├── spec.md
└── tasks.md             # Phase 2 output (/speckit-tasks — NOT created here)
```

### Source Code (repository root)

```text
src/
├── lib.rs               # export RenderMode
├── render.rs            # RenderMode; second tile table; mode-aware display + trim; to_text(mode)
├── raw_lines.rs         # second grid builder: liveness tracking, one-cell transfers
├── diagram.rs           # from_abbreviated(mode); try_ascii_print[_compact](mode)
├── rotate.rs            # unchanged — stays on the current rendering
├── moves.rs             # unchanged
├── diagram/
│   ├── tests.rs         # new insta snapshots for the opening-centered rendering
│   ├── test.rs          # unchanged (move/rotation tests stay on the current mode)
│   └── snapshots/       # existing snapshots must not change; new ones added
└── snapshots/           # existing snapshots must not change

examples/
├── ascii_print.rs       # KNOTTY_OPENING_CENTERED env var
├── samples/             # unchanged
└── knot-so-good/
    ├── README.md     # document the rendering toggle and what the retired characters do
    └── src/
        ├── main.rs      # RenderMode on Model + PersistedState, Msg, one button, call sites
        └── tests.rs     # persistence round-trip for the new field
```

**Structure Decision**: The existing single-crate layout is kept as-is. The library change is confined
to the three files that already own rendering (`render.rs`), grid construction (`raw_lines.rs`) and the
notation-to-grid entry points (`diagram.rs`). No new module is introduced: the opening-centered builder
lives beside the current one in `raw_lines.rs`, which is already the home of grid construction, and the
second tile table lives beside the first in `render.rs`. The two consumers each gain one selector — an
environment variable in the CLI example, a persisted toggle in the app.
