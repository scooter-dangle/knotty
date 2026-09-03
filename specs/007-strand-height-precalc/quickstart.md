# Quickstart / Validation Guide: Height-Precalculated Strand Placement

This guide validates the feature end-to-end. It assumes the standard toolchain
(`rust-toolchain.toml` pins 1.94.0 with the `wasm32-unknown-unknown` target).

## Prerequisites

```sh
# from repo root
rustup show                 # confirms 1.94.0 + wasm32 target are available
```

## Build & constitution gates

```sh
cargo build
cargo check --target wasm32-unknown-unknown    # Principle II (NON-NEGOTIABLE)
```

Both must succeed with no new dependencies added to `Cargo.toml` (Principle V).

## Test gates

```sh
cargo test                 # all unit + snapshot tests
cargo insta review         # review/accept NEW PrecalculatedHeights snapshots only
```

Expected:
- **No existing snapshot changes** — `Legacy` output is unchanged (C1 / SC-004).
  If any pre-existing snapshot diffs, that is a regression, not an accept.
- **New snapshots** appear only for `PrecalculatedHeights` renders.

## Scenario 1 — Reduced transfers (US1 / SC-001)

Render the `terrace` diagram in both modes and compare:

1. Parse `(0 (2 (4 (6 )5 )3 )1 (1 (3 (5 )6 )4 )2 )0`.
2. Render with `RenderMode::Legacy` → staircase of transfer diagonals (today's output).
3. Render the same diagram `.with_mode(RenderMode::PrecalculatedHeights)` →
   previously climbing/descending strands run flat; total transfer segments are
   fewer; the figure still decodes to the same knot.

Pass: max-height render has strictly fewer transfer diagonals and represents the
same knot.

## Scenario 2 — Opt-in / operating context (US3 / FR-012)

1. Default-constructed diagram has `mode() == RenderMode::Legacy`.
2. A notation-only move (e.g. a `Swap`) yields identical `items` whether the mode
   is `Legacy` or `PrecalculatedHeights` (C9).
3. Switching `set_mode(PrecalculatedHeights)` changes only rendering and rotation.

## Scenario 3 — Crossing fidelity (US4 / FR-007 / FR-011)

1. Render `basket` and `ugly_trefoil` in `PrecalculatedHeights`.
2. Confirm each crossing connects the same two strands as the `Legacy` render and
   no crossing is drawn between non-adjacent rows.

Pass: crossings are correct and the figure decodes to the same knot.

## Scenario 4 — Rotation stability (US2 / SC-006)

1. Take a diagram whose `Legacy` render has reversed-direction transfers.
2. Rotate it (`try_rotate_90_ccw`) with `PrecalculatedHeights` active; count
   features (`items` length) before vs. after.
3. Rotate through a full four-rotation cycle.

Pass: feature count never increases vs. the original, is strictly lower than the
`Legacy`-mode rotation for such diagrams, and the final diagram represents the
same knot as the original.

## Optional — example app

If the example flag is added, `examples/ascii_print.rs` can render a diagram in
the new mode for manual inspection; `knot-so-good` can expose a toggle. These are
optional surfaces (Principle I: library-first).

## References

- Behavioral contract: [contracts/public-api.md](./contracts/public-api.md)
- Data model: [data-model.md](./data-model.md)
- Design decisions: [research.md](./research.md)
