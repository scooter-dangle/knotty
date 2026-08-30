# Implementation Plan: Cell Boundary View in Manual Diagram Mode

**Branch**: `claude/diagram-ascii-boundary-view-pk4jjf` | **Date**: 2026-08-30 | **Spec**: [spec.md](./spec.md)

**Input**: Feature specification from `/specs/002-diagram-boundary-view/spec.md`

## Summary

Add a per-mode display toggle to the example app's manual diagram mode that draws the same picture
with cell boundary lines, so each typed character maps to one visible box.

No library capability is needed. `VerboseDiagram::display::<GRID_BORDERS>()` already renders both
ways and is already public; the app simply never calls the bordered form. The work is therefore
confined to `examples/knot-so-good/`: one `bool` on `Model`, one on `PersistedState`, one `Msg`
variant, one button, and a call-site branch on the const generic.

The one design decision with teeth is *what the app caches for the picture*. Today `Model` caches
the rendered `String`, which is what lets an invalid keystroke keep the last good picture on screen.
A rendered string cannot be re-rendered in the other view, so toggling while the picture is stale
(FR-007) would show the wrong thing. The cache therefore changes from the rendered `String` to the
last valid `VerboseDiagram`, with rendering moved to view time. That also makes the main picture and
the snapshot previews use one code path, since the previews already parse and render per view.

The only change under `src/` is test-only: an `insta` snapshot locking the bordered rendering, which
this feature makes user-visible for the first time and which no test currently covers.

## Technical Context

**Language/Version**: Rust 1.94.0 (pinned in `rust-toolchain.toml`)

**Primary Dependencies**: none added. Core crate keeps `itertools` + `regex`; the example app keeps
`yew 0.23`, `web-sys`, `js-sys`, `wasm-bindgen`, `svgbob`, `serde`, `serde_json`.

**Storage**: browser `localStorage` under the existing `knotty_state` key (example app only); one new
`bool` field.

**Testing**: `cargo test` with `insta` snapshots and `pretty_assertions`; the example app has
host-target unit tests in `examples/knot-so-good/src/tests.rs` (no browser harness).

**Target Platform**: `wasm32-unknown-unknown` (constitution Article II) plus the host target for tests.

**Project Type**: Rust library with a wasm example app consuming it.

**Performance Goals**: re-render on every keystroke and on every view pass. A bordered render is a
linear walk over the same cells, ~33% more output bytes than the plain one (4 columns per cell rather
than 3). Diagrams are tens of rows by tens of columns, so no budget is at risk.

**Constraints**:

- `GRID_BORDERS` is a **const** generic, so a runtime `bool` cannot be threaded into it. The branch
  has to happen at the call site — see [research.md](./research.md) R1.
- The bordered rendering draws each cell's top and left edge only; the picture's outer right and
  bottom edges stay open. Spec Assumptions accept this as-is; it is not to be "fixed" here.
- `ascii_diagram_to_html` panics (`unreachable!("bug!")`) on any byte outside its allow-list. `+` and
  `|` are already on it — verified, R4 — so no change is needed there, but the allow-list must not be
  narrowed.
- No new dependency in either `Cargo.toml` (constitution Article V).

**Scale/Scope**: one new persisted field, one new message, one new button; roughly 40 lines of app
changes plus tests. No new module, no new public API.

**Branch note**: as with feature 001, the assigned branch does not carry the constitution's `-story`
suffix. `test.yml` filters on the PR's *base* branch, so a PR into `main` still runs CI, and no
workflow change is needed.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Article | Status | Evidence |
|---------|--------|----------|
| I. Library-First | PASS | The capability already lives in the library: `VerboseDiagram::display::<true>()` is public and re-exported. This feature adds no library behaviour, so there is nothing that could land in the app first. The only `src/` change is an `insta` snapshot covering that existing path. |
| II. WASM-Compatible (NON-NEGOTIABLE) | PASS | App-side changes are a `bool`, a `match`, and a `serde` field — no new dependency, nothing `std`-only. Verified by `cargo check --target wasm32-unknown-unknown` at the root and for the example app. |
| III. Test-First | PASS | `insta` snapshot for `display::<true>()` in `src/render.rs` (a rendering path with no existing coverage, now user-visible); host-target tests in the app for the persisted default, the round trip, and the grid geometry the feature promises (one box per character, one box row per line). |
| IV. Notation Fidelity | PASS | Nothing about the notation, the verbose format, or the rendering changes. This is a display choice over an existing derived output. |
| V. Minimal Dependencies | PASS | Zero new entries in either `Cargo.toml`. |

**Post-Phase 1 re-check**: unchanged. The design added no dependency, no module, and no public API;
the `src/` change is a test. The one structural change — caching a `VerboseDiagram` instead of a
`String` — removes state rather than adding it, since the second rendering is derived rather than
stored.

## Project Structure

### Documentation (this feature)

```text
specs/002-diagram-boundary-view/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/
│   └── bordered-rendering.md
├── checklists/
│   └── requirements.md
├── spec.md
└── tasks.md             # Phase 2 output (/speckit-tasks — NOT created here)
```

### Source Code (repository root)

```text
src/
├── render.rs            # CHANGED (test only): insta snapshot for display::<true>()
├── snapshots/           # CHANGED: the new snapshot file
├── lib.rs               # unchanged — VerboseDiagram already re-exported, display() already pub
├── diagram.rs           # unchanged
├── raw_lines.rs         # unchanged
├── moves.rs             # unchanged
└── rotate.rs            # unchanged

examples/knot-so-good/
├── src/main.rs          # CHANGED: manual_borders on Model + PersistedState, Msg::ManualBorders,
│                        #          toggle button, render_manual takes the flag, manual_render
│                        #          caches the last valid VerboseDiagram
├── src/tests.rs         # CHANGED: persistence default/round-trip and grid-geometry tests
└── index.html           # unchanged — see research.md R6
```

**Structure Decision**: No structural change. The feature is a display option on an existing mode, so
it lives beside the state it modifies: the flag next to `compact` (the app's other display bool), the
message next to the other manual-mode messages, and the button in `manual_view` next to the snapshot
button. Splitting manual-mode view state into its own type would be the right move if a third or
fourth setting appeared; with two (`manual_diagram`, `manual_borders`) it would be abstraction ahead
of need.

## Complexity Tracking

> No constitution violations. Section intentionally empty.
