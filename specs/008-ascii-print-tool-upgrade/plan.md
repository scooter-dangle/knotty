# Implementation Plan: ASCII Print Tool Upgrade

**Branch**: `claude/ascii-print-tool-upgrade-teir79` | **Date**: 2026-09-04 | **Spec**: [spec.md](./spec.md)

**Input**: Feature specification from `specs/008-ascii-print-tool-upgrade/spec.md`

**Revised during implementation** (2026-09-05): the original succinct
round-trip design (R5: reuse the rotation feature's `scan_row` to reconstruct
notation from rendered text) was directly tested against the simplest
possible case and produced the wrong diagram — `scan_row`, called the way R5
proposed, computes the notation of a *rotated* diagram, not the original one;
that reversal isn't incidental preprocessing, it *is* the rotation. See
`research.md` R5 for the experiment and R10/R11 for what replaced it: reusing
`VerboseDiagram`'s existing, already-`pub`, already-tested `to_text()` /
`FromStr` lossless grid round-trip instead, embedded as a hidden trailer
behind the (otherwise completely unchanged) visible succinct art. This
version of the plan reflects that correction; the summary, technical
context, project structure and risks below are all rewritten accordingly.

## Summary

Rebuild `examples/ascii_print.rs`'s command line on `clap` (derive style,
long options only, shell completions), and flip its defaults so a bare
`ascii_print <diagram>` invocation now prints the succinct (compact) style
placed with the precalculated-heights mode — both previously required
setting environment variables. Add a `--style full-spaced` option for the
uncompacted rendering, and a new `--input-format succinct` path that expands
previously-printed succinct text back to the fully-spaced style. **No
library changes are required at all**: succinct output is today's unchanged
`ascii_print_compact` art plus a hidden trailer (`VerboseDiagram::to_text()`,
already `pub`, already lossless), and succinct input parses that trailer
straight back with `VerboseDiagram`'s existing `FromStr` — see research.md
R10.

## Technical Context

**Language/Version**: Rust 1.94.0 (pinned in `rust-toolchain.toml`)

**Primary Dependencies**: `clap` 4.6.6 (`derive` feature) and `clap_complete`
4.6.9, both new `[dev-dependencies]` used only by `examples/ascii_print.rs`;
`itertools`, `regex` (existing, unaffected)

**Storage**: N/A (stateless CLI, in-memory diagram structures)

**Testing**: `cargo test` (existing `insta` snapshots + `pretty_assertions`,
all expected to remain byte-for-byte unchanged — see Risks); manual CLI
validation per `quickstart.md`. No new `#[test]` obligations in `src/`, since
no `src/` code changes (constitution III's Test-First applies to new
behavior in `src/`; there is none here).

**Target Platform**: native, for the example binary. The library (`src/`)
is completely untouched by this feature; wasm32-compatibility is therefore
not at risk, but is still reconfirmed (constitution II, NON-NEGOTIABLE).

**Project Type**: Single Rust library crate (`knotty`) with example binaries;
this feature touches exactly one example binary and zero library files.

**Performance Goals**: No interactive/latency requirement — a one-shot CLI
tool.

**Constraints**: `--style succinct`/`full-spaced` output for encoded-diagram
input must remain byte-for-byte what today's `ascii_print`/`ascii_print_compact`
produce (now trivially true — this feature calls them unmodified). All
`src/` compiles for `wasm32-unknown-unknown` (unaffected — no `src/` change).

**Scale/Scope**: One CLI rewrite (`examples/ascii_print.rs`), two new
dev-dependencies, zero library changes, zero snapshot changes.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Library-First | ✅ PASS | This feature adds no new library behavior at all — it composes existing, already-`pub` library API (`AbbreviatedDiagram::from_abbreviated`, `ascii_print`/`ascii_print_compact`, `VerboseDiagram::to_text`/`FromStr`/`display`) entirely from the CLI. Nothing CLI-specific leaks into `src/`. |
| II. WASM-Compatible (NON-NEGOTIABLE) | ✅ PASS | `clap`/`clap_complete` are dev-dependencies reachable only from `examples/ascii_print.rs`; `src/` is untouched, so `cargo check --target wasm32-unknown-unknown` is unaffected by construction, not just by careful scoping. |
| III. Test-First | ✅ PASS (vacuously) | No new behavior lands in `src/`, so there is no new `#[test]`/`insta` obligation under Principle III. The guarantees this feature relies on (`VerboseDiagram`'s grid round-trip) are already covered by existing tests in `src/render.rs`. Manual `quickstart.md` validation covers the CLI-only logic (trailer embed/extract). |
| IV. Notation Fidelity | ✅ PASS | No new notation is introduced (research.md R3); the abbreviated notation stays the sole source of truth for encoded-diagram input. Succinct input never claims to be a notation source — it recovers a rendered grid, not notation (R11). `quickstart.md` includes a concrete example abbreviated-notation input (`(0 (2 \1 /0 \1 )2 )0`, the existing trefoil fixture) with its expected succinct/full-spaced outputs. |
| V. Minimal Dependencies | ✅ PASS | Two new entries in `Cargo.toml` (`clap`, `clap_complete`), both explicitly required by the feature request (FR-010–FR-013) and scoped as dev-dependencies to one example binary. |

**Result**: no violations; Complexity Tracking not required.

**Post-design re-check**: still PASS, and more clearly so than the original
design — the corrected approach (R10/R11) removes the one `src/` change
(the R7 collapse-to-two rework) the original design would have made,
leaving the library entirely untouched.

## Project Structure

### Documentation (this feature)

```text
specs/008-ascii-print-tool-upgrade/
├── plan.md                    # This file (revised — see note at top)
├── spec.md                    # Requirements
├── research.md                # Phase 0 — R1–R4, R8 valid as-is; R5–R7, R9
│                               #   superseded in place by R10–R12
├── data-model.md              # Phase 1 — CLI args, succinct trailer format (revised)
├── quickstart.md              # Phase 1 — validation scenarios
├── contracts/
│   ├── cli-interface.md       # External contract: the ascii_print command line
│   └── succinct-round-trip.md # Revised: trailer embed/extract, no library seam
├── checklists/requirements.md # Spec quality checklist
└── tasks.md                   # Phase 2 output (/speckit-tasks — revised)
```

### Source Code (repository root)

```text
src/                  # UNCHANGED — no file in src/ is touched by this feature

examples/
├── ascii_print.rs           # REWRITE: clap derive CLI; new defaults
│                            # (--input-format, --style, --placement,
│                            # --grid-borders, --echo-diagram, --completions);
│                            # builds/parses the succinct trailer using only
│                            # already-pub library API (research R10)
└── knot-so-good/            # unchanged — nothing in src/ changed for it to see

Cargo.toml           # CHANGE: add clap (derive), clap_complete to [dev-dependencies]
```

**Structure Decision**: single-crate library, unchanged shape. The CLI stays
an example binary (constitution I). Unlike the original plan, there is no
library-side change at all — every capability this feature needs already
existed as public API before this feature started.

## Risks

| Risk | Assessment |
|---|---|
| The corrected succinct round-trip design (R10) turns out to have its own flaw, symmetric to R5's | **Mitigated.** Unlike R5, R10 does not depend on a new, unverified transform — it reuses `VerboseDiagram`'s `to_text()`/`FromStr`, which already has passing round-trip tests in `src/render.rs` today, unmodified by this feature. The only new logic is trailer embed/extract, validated end-to-end by `quickstart.md` Scenario 3 before being considered done. |
| Breaking change: environment variables no longer read (C10) | **Accepted, intentional.** `examples/ascii_print.rs` is a dev tool, not a published/versioned API; the feature request explicitly asks to move off ad hoc environment-variable configuration. |
| Snapshot/consumer impact | **None expected**, and worth confirming rather than assuming: `ascii_print`/`ascii_print_compact` are called unmodified, so all 15 snapshots research.md originally flagged (R9, withdrawn) and `knot-so-good`'s compact view (R8, withdrawn) should be provably untouched — checked explicitly in `tasks.md`'s final gate rather than left as an assumption, given the surprise already found once in this feature. |
