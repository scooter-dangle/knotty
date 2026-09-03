# Implementation Plan: GUI Makeover

**Branch**: `claude/speckit-gui-makeover-vj0169` | **Date**: 2026-09-03 | **Spec**: [spec.md](./spec.md)

**Input**: Feature specification from `/specs/006-gui-makeover/spec.md`

## Summary

A cosmetic makeover of the `knot-so-good` browser app, confined to its view code and stylesheet:
the four "switch to …" buttons become segmented radio pairs that show both states; the character
drawing gets a font stack and `line-height: 0.8` so strands meet across rows, with the bordered
view's grid characters drawn as CSS rules; a diagram region that always exists at a minimum height,
with a permanent message line, stops the layout jumping on empty and error states; a viewport tag
and a single-column, wrapping layout make the page usable at phone width; and the snapshot catalog
becomes a card grid. No model field, message, transition, persisted field or library API changes.

Phase 0 settled the one open technical question by measurement ([research.md](./research.md) R1):
`line-height: 0.8` on the DejaVu Sans Mono / Menlo geometry closes every vertical join, while the
bordered grid's `-` can only be made continuous by drawing it, so `+`, `-`, `|` — which the plain
drawing never uses — get CSS rules behind a transparent glyph.

## Technical Context

**Language/Version**: Rust, channel pinned in `rust-toolchain.toml` (1.94.0); the app is a
`yew` 0.23 component compiled to `wasm32-unknown-unknown` by `trunk` 0.21.4. HTML and CSS.

**Primary Dependencies**: None new. `yew`, `web-sys`, `wasm-bindgen`, `svgbob`, `serde` as today
in `examples/knot-so-good/Cargo.toml`. No web font, no CSS framework (R1, R6).

**Storage**: Browser `localStorage`, key `knotty_state` — **unchanged in every field** (FR-001;
[data-model.md](./data-model.md)).

**Testing**: `cargo test --manifest-path examples/knot-so-good/Cargo.toml` (26 host-target tests
at baseline, plus new ones for the pure helpers); the root `cargo test` as a no-change check;
`screenshots/capture.js` re-taken and compared with the baseline; `screenshots/measure-gaps.js`
for the spacing (R7, [quickstart.md](./quickstart.md)).

**Target Platform**: Browsers — desktop Chromium/Firefox/Safari and Android Chrome / iOS Safari —
served as static files from GitHub Pages by `.github/workflows/deploy.yml`.

**Project Type**: Single-page browser app that is an example inside a library repository.

**Performance Goals**: None beyond not regressing. The largest drawing in the baseline (~140 columns
× 7 rows, ~1,000 characters) gains a handful of `span`s in the bordered view only; no measurable
cost.

**Constraints**: Cosmetic only — the set of modes, settings, actions and transitions is fixed, the
picture renderer stays (spec *What "cosmetic" means here*). Everything must still compile for
`wasm32-unknown-unknown`. No changes under `src/`.

**Scale/Scope**: Three files — `examples/knot-so-good/src/main.rs` (1,004 lines; the two view
functions, `ascii_diagram_to_html`, `error_to_html`), `examples/knot-so-good/index.html` (the
inline `<style>` moves out; viewport tag in), new `examples/knot-so-good/style.css` — plus
`src/tests.rs` in the app for new helper tests, and `specs/006-gui-makeover/screenshots/capture.js`
updated to the new control names.

## Constitution Check

*GATE: must pass before Phase 0 research. Re-checked after Phase 1 design.* Both passes clean; no
entries in Complexity Tracking.

| Principle | Assessment |
|---|---|
| **I. Library-First** | Nothing lands in the GUI that belongs in the library: the feature needs no new library capability. The drawing's characters, rows and columns come from `knotty` exactly as before (FR-003); the app only styles them. `src/` is untouched, and the quickstart gate asserts it. |
| **II. WASM-Compatible** (non-negotiable) | No dependency added anywhere. The app crate's wasm check is in the gate. |
| **III. Test-First** | The constitution's rule is scoped to `src/`, which does not change. The app's own suite (26 tests) must pass unchanged — that is SC-007's evidence — and every pure function the makeover adds or alters (`ascii_diagram_to_html`'s grid classes, any state-to-label helper) gets a host-target test alongside it. View markup is checked by the capture script, since the repository has no browser test harness (R7). |
| **IV. Notation Fidelity** | Not touched: notation, verbose format and rendering are all unchanged; the app displays the same strings. |
| **V. Minimal Dependencies** | `Cargo.toml` (root and app) unchanged. R1 rejected a web font and R6 a CSS framework on this ground. One new static asset, `style.css`, which trunk already knows how to ship. |

Development workflow notes from the constitution: the spec branch is
`claude/speckit-gui-makeover-vj0169`; implementation switches to a `-story` branch per the
*Branching* rule. Conventional commits, one logical change each — the phases below are the natural
commit boundaries.

## Project Structure

### Documentation (this feature)

```text
specs/006-gui-makeover/
├── plan.md              # This file
├── spec.md              # Feature specification
├── baseline.md          # Survey of the app before the makeover, with measurements
├── research.md          # Phase 0: spacing measurements, control form, layout, testing
├── data-model.md        # Phase 1: nothing persisted changes; view-only structures
├── quickstart.md        # Phase 1: gate, build, re-capture, criterion-by-criterion checks
├── contracts/
│   └── ui.md            # Phase 1: every control by accessible name, regions, stylesheet hooks
├── checklists/
│   └── requirements.md  # From /speckit-specify
├── screenshots/
│   ├── capture.js       # Drives the app through 15 states × 2 viewports (update to new names)
│   ├── measure-gaps.js  # Counts ink gaps at the four joins for a font stack / line-height
│   ├── baseline/        # 30 captures + 2 zoom crops + measurements, before
│   ├── research/        # Trefoil at line-height normal vs 0.8; bordered grid at 0.8
│   └── after/           # Re-taken captures, after (created during implementation)
└── tasks.md             # Phase 2 output (/speckit-tasks — NOT created here)
```

### Source Code (repository root)

```text
examples/knot-so-good/
├── index.html           # viewport meta; <link data-trunk rel="css" href="style.css">;
│                        #   inline <style> removed
├── style.css            # NEW: layout, toolbar groups, segmented radio pairs, diagram
│                        #   region + message line, ascii font/line-height/padding,
│                        #   grid rules, disabled dimming, snapshot card grid, phone sizes
└── src/
    ├── main.rs          # view(): toolbar groups + radio pairs, diagram region, block
    │                    #   inputs with labels, card catalog; manual_view(): same;
    │                    #   mode_toggle() -> segmented pair helper; error_to_html removed;
    │                    #   ascii_diagram_to_html classifies + - |;
    │                    #   ascii_html_diagram built from Ok side only
    └── tests.rs         # tests for the grid classification and any new pure helper

src/                     # UNTOUCHED
Cargo.toml               # UNTOUCHED (root and app)
.github/workflows/       # UNTOUCHED — trunk picks up style.css with no workflow change
```

**Structure Decision**: The existing layout is kept exactly; one file is added (`style.css`) and
the stylesheet moves out of `index.html`. No new module: `main.rs` grows a couple of small view
helpers (a segmented-pair builder, a diagram-region builder) rather than a component tree, because
the page has five groups and two views and a component split would be more code than the views.

## Phase 0: Research — complete

See [research.md](./research.md). Decisions: R1 spacing (font stack, `line-height: 0.8`, padding,
CSS-drawn grid); R2 segmented radio pairs for all four settings; R3 the always-present diagram
region and message line, block-level inputs, opacity for disabled; R4 viewport tag and single-column
wrapping layout; R5 card grid; R6 `style.css` via trunk; R7 three-layer testing. No
NEEDS CLARIFICATION remained.

## Phase 1: Design — complete

- [data-model.md](./data-model.md): persisted state and messages unchanged; the diagram region,
  control groups and grid character classes as view-only structures.
- [contracts/ui.md](./contracts/ui.md): every control by accessible name with what it dispatches
  and when it is present; the regions in document order; the stylesheet values the success criteria
  depend on; the explicit list of what does not change.
- [quickstart.md](./quickstart.md): gate, build/serve, re-capture, and a criterion-by-criterion
  table with the numbers to compare against.

### Implementation phases (for `/speckit-tasks`)

Ordered so that each leaves the app building and the gate green, and so the user's three minimum
outcomes land first:

1. **Scaffold** — `style.css` linked via trunk; viewport meta; page container and typography
   (FR-016, FR-022, FR-023). Baseline behaviour unchanged; page merely stops being 980 px on phones.
2. **Toggles** — a segmented-pair helper; the four buttons replaced; labels for the two notation
   textareas (FR-004–FR-007). `capture.js` updated to the new names in the same commit so the
   captures keep working.
3. **Spacing** — `ascii_diagram_to_html` classifies `+ - |`; the font stack, line-height, padding
   and grid rules in CSS; tests for the classification (FR-008, FR-009).
4. **Stability** — diagram region + message line in both views; notation error shown in the
   picture display; `error_to_html` removed; block-level inputs; disabled dimming (FR-010–FR-015).
5. **Toolbar and inputs** — groups, wrapping, tap-target sizes, full-width text boxes, scrolling
   canvas (FR-017–FR-019).
6. **Catalog** — card grid with fixed preview boxes (FR-020, FR-021, FR-024).
7. **Verify** — re-take captures into `screenshots/after/`, run the quickstart table, record
   results; fix what it turns up.

## Complexity Tracking

No constitution violations; nothing to justify.
