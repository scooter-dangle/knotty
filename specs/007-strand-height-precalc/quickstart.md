# Quickstart / Validation Guide: Height-Precalculated Strand Placement

Rewritten 2026-09-03 against `origin/main` at `37b7c09`. Validates the feature
end to end. Assumes the toolchain pinned in `rust-toolchain.toml` (1.94.0 with
the `wasm32-unknown-unknown` target).

## Prerequisites

```sh
rustup show                                    # 1.94.0 + wasm32 target
cargo build
cargo check --target wasm32-unknown-unknown    # Principle II (NON-NEGOTIABLE)
```

Both must succeed with no `Cargo.toml` additions (Principle V).

## Baseline first

Before any change, record the pre-change state — every later gate compares
against it:

```sh
cargo test
ls src/snapshots/*.snap src/diagram/snapshots/*.snap | wc -l   # expect 24
```

## Test gates

```sh
cargo test
cargo insta review    # accept ONLY new PrecalculatedHeights snapshots
```

Expected:

- **No existing snapshot changes.** All 24 pre-existing snapshots must be
  byte-for-byte identical (C1 / SC-004). A diff on one of them is a regression to
  fix, never a snapshot to accept.
- **New snapshots** appear only for `PrecalculatedHeights` renders.

## Scenario 1 — Component A against fixtures (US1 / FR-001)

The five fixtures in [fixtures/](./fixtures/) supply per-strand maxima for 63
features. For each, the calculated maxima must equal the supplied maxima exactly.

Pass: all fixtures match exactly.

Also assert the two cases no supplied fixture covers, whose expected values
follow from the height rule (research R2):

| Encoding | Expected heights | Covers |
|---|---|---|
| `(0 (1 )1 (1 )1 )0` | `(0,3), (1,2), (1,2)` | sequential siblings reusing rows |
| `(0 (1 (2 )2 (3 )3 )1 )0` | `(0,7), (1,4), (2,3), (5,6)` | a sibling stacked above a divergent pair |
| `(0 (0 )0 (2 )2 )0` | `(2,3), (0,1), (4,5)` | transitive push: two strands that never coexist and never relate still do not share a height, because a third lies between them in the order. Needs 6 rows against the default's 4. |

> Passing the five fixtures alone is **not sufficient**. The natural but wrong
> gap formula — a pair's gap equals the count of strands opened between it —
> matches all 23 pairs in all five of them. The first row above is what
> distinguishes it.

## Scenario 2 — Component B against fixtures (US1, US4 / FR-002, FR-011, FR-015, FR-016)

Render each fixture's encoding against its **supplied** maxima — not against
Component A's output — and compare to the expected grid character for character.

Pass: exact match on all five. In particular
[non-adjacent-crossing](./fixtures/non-adjacent-crossing.md) must reproduce all
three midpoint crossings and their return columns.

## Scenario 3 — Integration and the seam (FR-006)

1. Feed Component A's real output to Component B and re-render each fixture.
2. Assert A's maxima equal the maxima each fixture supplies to B.

Step 2 is what proves the two independently-built halves meet. Pass: identical
grids to Scenario 2, and maxima agree.

## Scenario 4 — Opt-in and orthogonality (US3 / FR-012, FR-013, FR-014)

1. A default-constructed diagram reports `mode() == PlacementMode::IndexAligned`.
2. A notation-only move (`Swap`, `WrapAround`, `ChangeCrossing`, Reidemeister,
   `Bulge`/`Collapse`) yields identical `to_tuples()` under both modes (C9).
3. Switching to `PrecalculatedHeights` changes placement and rotation only — the
   grid mapping is untouched (C11).

## Scenario 5 — Transfer accounting (SC-002)

Count transfer segments **per glyph** and classify into three categories:
open/close displacement, boundary, crossing-alignment. Report all three per
example.

Pass: displacement segments strictly reduced wherever displacement exists, and
never increased. The **total may be unchanged or higher** — `(0 (1 )1 )0` goes
from 4 displacement to 4 boundary segments, and that is conforming.

## Scenario 6 — Rotation stability (US2 / SC-006)

1. Take a diagram whose `IndexAligned` render contains reversed-direction transfers.
2. Rotate with `PrecalculatedHeights` active; compare feature counts (`items` length).
3. Rotate through a full four-rotation cycle.

Pass: the count never increases versus the original, is strictly lower than the
`IndexAligned` rotation for such diagrams, and the final diagram represents the
same knot.

> **Rotation results legitimately differ between modes** — a cleaner grid scans
> to different but equivalent notation (research R6). Do not treat a changed
> `PrecalculatedHeights` rotation result as a regression; only `IndexAligned`
> output is frozen.

## Scenario 7 — Dimensions (research R7)

Grid dimensions change in both directions and neither is a defect:

| Fixture | Default | New mode |
|---|---|---|
| rotated-5_1 | 8 × 19 | 8 × 18 |
| square-knot | 6 × 18 | 6 × 12 |
| non-adjacent-crossing | 6 × 18 | 6 × **19** |
| little-dumb-link | 6 × 20 | **8** × 15 |
| square-knot-links-encircled | 12 × 48 | **16** × 36 |

Pass: rendered height equals `max(all maxima) + 1`, **not**
`AbbreviatedDiagram::height()`, which under-counts whenever a pair diverges.

## Optional — example surfaces

`examples/ascii_print.rs` may expose a mode flag and `knot-so-good` a toggle.
Both optional; the library owns the behavior (Principle I).

## References

- Public API: [contracts/public-api.md](./contracts/public-api.md)
- Component seam: [contracts/strand-heights.md](./contracts/strand-heights.md)
- Data model: [data-model.md](./data-model.md)
- Design decisions: [research.md](./research.md)
- Golden fixtures: [fixtures/](./fixtures/)
