# Public API Contract: Height-Precalculated Strand Placement

Rewritten 2026-09-03 against `origin/main` at `37b7c09`. This is a Rust library;
the contract is the public API surface and its behavioral guarantees. Signatures
are indicative (naming at implementer discretion); behavior is normative.

For the *internal* seam between the two components, see
[strand-heights.md](./strand-heights.md).

## New public surface

```rust
// re-exported from lib.rs
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum PlacementMode {
    #[default]
    IndexAligned,
    PrecalculatedHeights,
}

impl AbbreviatedDiagram {
    pub fn mode(&self) -> PlacementMode;
    pub fn set_mode(&mut self, mode: PlacementMode);
    pub fn with_mode(self, mode: PlacementMode) -> Self;
}
```

`PlacementMode` governs how strand heights are calculated. It is **not** a
rendering mode — the opening-centered grid mapping is a separate axis and is not
selectable (FR-014).

## Unchanged signatures (behavior now mode-dependent)

These keep their exact current signatures and remain backward compatible; with
the default `IndexAligned`, output is identical to today:

- `AbbreviatedDiagram::ascii_print::<const GRID_BORDERS: bool>(&self) -> String`
- `AbbreviatedDiagram::try_ascii_print::<const GRID_BORDERS: bool>(&self) -> Result<String, String>`
- `AbbreviatedDiagram::ascii_print_compact::<...>` / `try_ascii_print_compact::<...>`
- the free `knotty::ascii_print` / `try_ascii_print` / `*_compact` functions
- `AbbreviatedDiagram::try_rotate_90_ccw(&mut self) -> Result<(), String>`
- `AbbreviatedDiagram::try_apply(&mut self, DiagramMove) -> Result<(), String>`
- `AbbreviatedDiagram::try_apply_all(...) -> Result<(), String>`

> The free `ascii_print(knot: Vec<(u8, usize)>)` helpers build diagrams in the
> default `IndexAligned` mode. Callers wanting `PrecalculatedHeights` go through
> an `AbbreviatedDiagram` configured via `with_mode` / `set_mode`.

## Behavioral guarantees (normative)

| ID | Guarantee | Trace |
|----|-----------|-------|
| C1 | With `IndexAligned`, all rendering and rotation output is byte-for-byte identical to the pre-feature library. | FR-005, FR-013, SC-004 |
| C2 | With `PrecalculatedHeights`, each strand is placed at its precalculated maximum, and each cap, cup or crossing at the floored midpoint of the two strands it joins. | FR-001, FR-002, FR-011, FR-016 |
| C3 | A strand whose maximum does not change between its cap and its cup renders flat, with no transfer segments. | FR-003 |
| C4 | Open/close displacement segments are strictly reduced versus `IndexAligned` for any diagram exhibiting them, and never increased. **No guarantee is made about the total**, which may rise via boundary or crossing-alignment segments. | FR-004, SC-002 |
| C5 | Both modes render to the same knot for every valid diagram. | FR-006, SC-003 |
| C6 | Crossings always connect the correct partners and are never drawn between non-adjacent rows. | FR-007, FR-011 |
| C7 | Output is deterministic for a given `(items, mode)`. | FR-008, SC-005 |
| C8 | Empty and degenerate diagrams render without error, equivalent to `IndexAligned` where no divergence exists. | FR-010 |
| C9 | The mode is an operating context: rotation honors `self.mode`; notation-only moves produce identical results regardless of mode. | FR-012 |
| C10 | Rotating in `PrecalculatedHeights` never increases the scanned feature count versus the original, and is strictly lower than the `IndexAligned` rotation for diagrams with reversed-direction transfers. | SC-006 |
| C11 | Selecting either mode does not change how an already-placed diagram is mapped onto the character grid. | FR-014 |

## Consequences callers should expect

**Rotation results differ between modes.** Rotation re-derives notation by
scanning the rendered grid, so a cleaner grid yields different — but equivalent —
notation. This is intended (C9, research R6). Only `IndexAligned` output is
frozen; a changed `PrecalculatedHeights` rotation result is not a regression.

**Grid dimensions differ between modes.** Height can *increase* — a divergent
pair holds its gap open for its whole life, so the diagram may span more rows
than are ever occupied at once (research R7; the encircled fixture needs 16 rows
against the default's 12). Width usually decreases but can increase where
crossing alignment dominates. Callers that assume `AbbreviatedDiagram::height()`
bounds the rendered row count are correct only under `IndexAligned`.

## Reference inputs

The five golden fixtures in [../fixtures/](../fixtures/) are the authority on
expected `PrecalculatedHeights` output. They supply per-strand maxima and exact
rendered grids for 63 features, covering divergent caps and cups, crossing
alignment with returns, odd and even separations, nesting to depth 6, and a
separation of 14.

`IndexAligned` output is pinned by the 24 pre-existing `insta` snapshots, which
must not change.
