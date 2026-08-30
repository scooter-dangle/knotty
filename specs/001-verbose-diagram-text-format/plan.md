# Implementation Plan: Verbose Diagram Text Format

**Branch**: `claude/verbose-diagram-serialization-wwphbz` | **Date**: 2026-08-30 | **Spec**: [spec.md](./spec.md)

**Input**: Feature specification from `/specs/001-verbose-diagram-text-format/spec.md`

## Summary

Add a one-character-per-cell text format for a rendered diagram, readable and writable in the core
crate, and consume it from the example app as a second, fully separate "manual diagram" mode.

The core work is small and lands entirely in `src/render.rs`: a byte ↔ `Horiz` mapping, a `FromStr`
that builds a `VerboseDiagram` from text, and a `Display` that writes the canonical padded rectangle
back out. Both directions reverse row order, because the text reads top-down while `VerboseDiagram`
stores row 0 at the bottom. Nothing else in the library changes.

The app work is larger in volume but shallow: `Model` splits into two independent per-mode states,
`PersistedState` gains three defaulted fields, and manual mode gets its own view with an ASCII-only
picture, its own snapshot list, and a stale-marked render that survives invalid input.

## Technical Context

**Language/Version**: Rust 1.94.0 (pinned in `rust-toolchain.toml`)

**Primary Dependencies**: none added. Core crate keeps `itertools` + `regex`; the example app keeps
`yew 0.23`, `web-sys`, `js-sys`, `wasm-bindgen`, `svgbob`, `serde`, `serde_json`.

**Storage**: browser `localStorage` under the existing `knotty_state` key (example app only).

**Testing**: `cargo test` with `insta` snapshots and `pretty_assertions`; the example app has
host-target unit tests in `examples/knot-so-good/src/tests.rs` (no browser harness).

**Target Platform**: `wasm32-unknown-unknown` (constitution Article II) plus the host target for tests.

**Project Type**: Rust library with a wasm example app consuming it.

**Performance Goals**: re-parse and re-render on every keystroke. Diagrams are tens of rows by tens of
columns; parsing is a single linear pass over a few hundred bytes, so no budget is at risk.

**Constraints**:

- `VerboseDiagram.0` and `VerboseLine.0` are `pub(crate)`, so parsing **must** live inside the crate.
  This is not a preference — an external consumer cannot construct a `VerboseDiagram` at all.
- `VerboseDiagram::display()` renders row 0 **last** (it iterates `.rev()`), so both parse and write
  must reverse. See [research.md](./research.md) for the trap this creates in error positions.
- No new core-crate dependency (constitution Article V).

**Scale/Scope**: 16 cell kinds; one new public trait impl pair; roughly 250 lines of app changes.

**Branch note**: the constitution asks for a `-story` suffix on feature branches. The assigned branch
does not carry one, but `test.yml` filters on the PR's *base* branch, so a PR into `main` still runs
CI. No workflow change is needed.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Article | Status | Evidence |
|---------|--------|----------|
| I. Library-First | PASS | Reading and writing land in `src/render.rs` and are usable by any consumer; the app only calls the public API. The library slice (P1+P2) is independently shippable with no app change. |
| II. WASM-Compatible (NON-NEGOTIABLE) | PASS | New code is `core`/`alloc` only — a byte match, `Vec`, and `String`. No new dependency, nothing `std`-only. Verified by `cargo check --target wasm32-unknown-unknown`. |
| III. Test-First | PASS | Unit tests for the mapping, padding, error positions, and round trips; `insta` snapshots for the new rendering path, stored under `src/snapshots/`. App persistence tests extend the existing host-target suite. |
| IV. Notation Fidelity | PASS with noted tension | See Complexity Tracking. The abbreviated notation stays the source of truth for *knots*; this format describes *pictures* and never produces notation. |
| V. Minimal Dependencies | PASS | Zero new entries in either `Cargo.toml`. |

**Post-Phase 1 re-check**: unchanged. The design added no dependency, no new crate, and no module
outside `src/render.rs`; the app changes stay inside `examples/knot-so-good/`.

## Project Structure

### Documentation (this feature)

```text
specs/001-verbose-diagram-text-format/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/
│   └── diagram-text-format.md
├── checklists/
│   └── requirements.md
├── spec.md
└── tasks.md             # Phase 2 output (/speckit-tasks — NOT created here)
```

### Source Code (repository root)

```text
src/
├── render.rs            # CHANGED: byte ↔ Horiz mapping, FromStr + Display for
│                        #          VerboseDiagram, inline #[cfg(test)] mod tests
├── snapshots/           # CHANGED: new insta snapshots for the parse→render path
├── lib.rs               # unchanged — Horiz, VerboseDiagram, VerboseLine already re-exported
├── diagram.rs           # unchanged
├── raw_lines.rs         # unchanged
├── moves.rs             # unchanged
└── rotate.rs            # unchanged

examples/knot-so-good/
├── src/main.rs          # CHANGED: Mode split, manual-mode view, per-mode state and snapshots
├── src/tests.rs         # CHANGED: persistence round-trip and back-compat tests
└── index.html           # CHANGED: styles for the stale render, symbol table, collapsed readout
```

**Structure Decision**: The existing single-crate layout is kept as-is. `src/render.rs` already owns
`Horiz`, `VerboseLine`, and `VerboseDiagram` together with their display strings, so the symbol table
belongs beside the display table it mirrors — splitting it into a new module would separate two
tables that must stay in step. Tests go inline in `src/render.rs` as `#[cfg(test)] mod tests`,
matching `src/raw_lines.rs`; the file is not large enough to warrant the `src/diagram/tests.rs`
directory pattern.

## Complexity Tracking

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| Article IV tension: a second authoring path into the renderer that is not derived from abbreviated notation | The feature exists precisely to state a *rendering* without a knot behind it — for expected-output tests and rendering bug reports. Article IV governs knots; it says the verbose format is a derived output, and this adds an input path to it. | Deriving expected renderings from notation is the status quo, and it is what makes rendering tests painful to author. Requiring the text to describe a valid knot (spec FR-014 forbids this) would reintroduce the coupling the feature removes, and would make it impossible to specify a *broken* rendering in a bug report. **Guardrail**: the conversion stays one-directional — the format never yields notation, so notation cannot be authored through it and remains the sole source of truth for knots. |
