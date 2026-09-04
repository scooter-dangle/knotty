# Implementation Plan: ASCII Print Tool Upgrade

**Branch**: `claude/ascii-print-tool-upgrade-teir79` | **Date**: 2026-09-04 | **Spec**: [spec.md](./spec.md)

**Input**: Feature specification from `specs/008-ascii-print-tool-upgrade/spec.md`

## Summary

Rebuild `examples/ascii_print.rs`'s command line on `clap` (derive style,
long options only, shell completions), and flip its defaults so a bare
`ascii_print <diagram>` invocation now prints the succinct (compact) style
placed with the precalculated-heights mode — both previously required
setting environment variables. Add a `--style full-spaced` option for the
uncompacted rendering, and a new `--input-format succinct` path that
reconstructs notation from previously-printed succinct text (reusing the
diagram-rotation feature's proven glyph scanner, `scan_row`) so it can be
re-rendered fully spaced. One small, low-risk library change is required to
make succinct text safely re-parseable: `ascii_print_compact` collapses each
blank column run to two placeholder columns instead of deleting it (see
research.md R7).

## Technical Context

**Language/Version**: Rust 1.94.0 (pinned in `rust-toolchain.toml`)

**Primary Dependencies**: `clap` 4.6.6 (`derive` feature) and `clap_complete`
4.6.9, both new `[dev-dependencies]` used only by `examples/ascii_print.rs`;
`itertools`, `regex` (existing, unaffected)

**Storage**: N/A (stateless CLI, in-memory diagram structures)

**Testing**: `cargo test` (existing `insta` snapshots + `pretty_assertions`);
new unit tests for `try_from_succinct_text` and for `scan_row` against
collapsed-to-two-column input (`src/rotate.rs`); manual CLI validation per
`quickstart.md`

**Target Platform**: native, for the example binary. The library (`src/`)
remains wasm32-compatible; the CLI's new dependencies never reach `src/` or
the wasm build (constitution II is about `src/`, not example binaries)

**Project Type**: Single Rust library crate (`knotty`) with example binaries;
this feature touches one example binary and one small library addition

**Performance Goals**: No interactive/latency requirement — a one-shot CLI
tool. `try_from_succinct_text` is linear in input size, matching
`try_rotate_90_ccw`'s existing scan cost.

**Constraints**: `--style succinct`/`full-spaced` output for encoded-diagram
input, other than the collapse-to-two width change (R7), must remain
byte-for-byte what today's `ascii_print`/`ascii_print_compact` produce — this
feature changes CLI defaults and adds capabilities, it does not change how a
diagram is rendered. All `src/` changes must keep compiling for
`wasm32-unknown-unknown`.

**Scale/Scope**: One CLI rewrite (`examples/ascii_print.rs`), one library
function addition (`AbbreviatedDiagram::try_from_succinct_text`), one library
behavior change (`ascii_print_compact`'s collapse width), two new
dev-dependencies, ~15 snapshot regenerations.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Library-First | ✅ PASS | The one behavior with reuse value beyond this CLI — `try_from_succinct_text` — lands in `src/diagram.rs`, independently useful to any consumer (e.g. `knot-so-good`) the same way `try_rotate_90_ccw` already is. Everything CLI-specific (flags, defaults, completions) stays in `examples/ascii_print.rs`, which hosts, never redefines, library behavior. |
| II. WASM-Compatible (NON-NEGOTIABLE) | ✅ PASS | `clap`/`clap_complete` are dev-dependencies reachable only from `examples/ascii_print.rs`; `cargo check --target wasm32-unknown-unknown` (library only) is unaffected. The one `src/` change (R7's collapse-to-two) is pure, dependency-free logic already in a wasm-compiled module. |
| III. Test-First | ✅ PASS | `contracts/succinct-round-trip.md` lists concrete test obligations (round-trip assertions, new `scan_row` cases, snapshot regeneration) to land alongside the implementation, not after. Regression tests required for the `ascii_print_compact` change per constitution (touches `diagram.rs`). |
| IV. Notation Fidelity | ✅ PASS | No new notation is introduced (research.md R3); the abbreviated notation stays the sole source of truth. `quickstart.md` includes a concrete example abbreviated-notation input (`(0 (2 \1 /0 \1 )2 )0`, the existing trefoil fixture) with its expected succinct/full-spaced outputs, satisfying the constitution's "specs must include example abbreviated-notation inputs and expected outputs" for this feature's artifacts. |
| V. Minimal Dependencies | ✅ PASS | Two new entries in `Cargo.toml` (`clap`, `clap_complete`), both explicitly required by the feature request (FR-010–FR-013) and scoped as dev-dependencies to one example binary — not GUI deps misplaced in the root, and not addable to `examples/knot-so-good/Cargo.toml` since that GUI has no CLI surface to serve. |

**Result**: no violations; Complexity Tracking not required.

**Post-design re-check**: still PASS. Phase 1 added one public function
(`try_from_succinct_text`) alongside an existing sibling
(`try_rotate_90_ccw`) with the same shape, and one behavior change to an
existing function's output width — no new abstractions, traits, or generics.

## Project Structure

### Documentation (this feature)

```text
specs/008-ascii-print-tool-upgrade/
├── plan.md                    # This file
├── spec.md                    # Requirements
├── research.md                # Phase 0 — R1–R9
├── data-model.md              # Phase 1 — CLI args, succinct format shape, library addition
├── quickstart.md              # Phase 1 — validation scenarios
├── contracts/
│   ├── cli-interface.md       # External contract: the ascii_print command line
│   └── succinct-round-trip.md # Internal seam: ascii_print_compact ⇄ try_from_succinct_text
├── checklists/requirements.md # Spec quality checklist
└── tasks.md                   # Phase 2 output (/speckit-tasks)
```

### Source Code (repository root)

```text
src/
├── diagram.rs       # CHANGE: try_ascii_print_compact collapses blank runs to
│                    #         2 columns instead of 0 (R7); ADD
│                    #         try_from_succinct_text (R5), mirroring
│                    #         try_rotate_90_ccw's scan loop
├── rotate.rs         # CHANGE (tests only): add scan_row cases fed
│                    #         collapsed-to-two-column input directly (R7)
├── raw_lines.rs      # unchanged — Grid::column's existing behavior is what
│                    #         R7's safety argument relies on, not a target of change
├── render.rs         # unchanged
├── moves.rs          # unchanged
└── diagram/snapshots/, snapshots/   # 15 ascii_print_compact-derived snapshots
                       #         regenerated via `cargo insta review` (R9)

examples/
├── ascii_print.rs           # REWRITE: clap derive CLI; new defaults
│                            # (--input-format, --style, --placement,
│                            # --grid-borders, --echo-diagram, --completions)
└── knot-so-good/            # unchanged (R8 — cosmetic-only effect, no code change)

Cargo.toml           # CHANGE: add clap (derive), clap_complete to [dev-dependencies]
```

**Structure Decision**: single-crate library, unchanged shape. The CLI stays
an example binary (constitution I: CLI is a consumer of the library, never a
host for new logic) — the only logic that moves into `src/` is the one
function with reuse value beyond this CLI.

## Risks

| Risk | Assessment |
|---|---|
| `scan_row` correctness against collapsed-to-two-column input (R7) | **Low, verifiable.** The per-row-constant-within-a-run invariant is proven from `Grid::column`'s existing code, not assumed; residual risk is confined to writing the new direct unit tests research.md calls for, not to the underlying logic. |
| Placement-mode provenance for succinct input (R6) | **Resolved by design**, via the trailing `# placement: ...` metadata line, reusing the codebase's existing `#`-comment convention. Residual: hand-edited succinct input missing the line falls back to the tool's default, which is a documented, acceptable behavior, not a defect. |
| Breaking change: environment variables no longer read (C10) | **Accepted, intentional.** `examples/ascii_print.rs` is a dev tool, not a published/versioned API; the feature request explicitly asks to move off ad hoc environment-variable configuration. |
| Snapshot churn (R9) | **Mechanical.** ~15 snapshots regenerate with a single-line diff shape ("wider blank run"); reviewed via `cargo insta review` as part of implementation, per constitution III. |
| `knot-so-good` GUI's compact view changes cosmetically (R8) | **Accepted, out of scope.** No functional test in that crate depends on exact compact width. |
