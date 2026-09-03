# Public API Contract: Height-Precalculated Strand Placement

This is a Rust library; the "contract" is the public API surface and its
behavioral guarantees. Signatures are indicative (final naming at implementer
discretion); behavior is normative and traces to the spec.

## New public surface

```rust
// re-exported from lib.rs
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum RenderMode {
    #[default]
    Legacy,
    PrecalculatedHeights,
}

impl AbbreviatedDiagram {
    pub fn mode(&self) -> RenderMode;
    pub fn set_mode(&mut self, mode: RenderMode);
    pub fn with_mode(self, mode: RenderMode) -> Self;
}
```

## Unchanged signatures (behavior now mode-dependent)

These keep their exact current signatures and remain backward compatible
(default mode `Legacy` ⇒ identical output):

- `AbbreviatedDiagram::ascii_print::<const GRID_BORDERS: bool>(&self) -> String`
- `AbbreviatedDiagram::try_ascii_print::<const GRID_BORDERS: bool>(&self) -> Result<String, String>`
- `AbbreviatedDiagram::ascii_print_compact::<...>` / `try_ascii_print_compact::<...>`
- the free `knotty::ascii_print` / `try_ascii_print` / `*_compact` functions
- `AbbreviatedDiagram::try_rotate_90_ccw(&mut self) -> Result<(), String>`
- `AbbreviatedDiagram::try_apply(&mut self, DiagramMove) -> Result<(), String>`
- `AbbreviatedDiagram::try_apply_all(...) -> Result<(), String>`

> The free `ascii_print(knot: Vec<(u8, usize)>)` helpers build a diagram with
> the default `Legacy` mode. Callers that want `PrecalculatedHeights` go through
> an `AbbreviatedDiagram` configured via `with_mode`/`set_mode`.

## Behavioral guarantees (normative)

| ID | Guarantee | Trace |
|----|-----------|-------|
| C1 | With `RenderMode::Legacy`, all rendering and rotation output is byte-for-byte identical to the pre-feature library. | FR-005, FR-013, SC-004 |
| C2 | With `RenderMode::PrecalculatedHeights`, each opening is rendered at its precalculated maximum row. | FR-001, FR-002 |
| C3 | A strand that does not change rows between open and close renders flat (no transfer) where placement allows. | FR-003 |
| C4 | Open/close displacement (reversed-direction) transfers are strictly reduced vs. `Legacy` for any diagram exhibiting them; never increased for crossing-free diagrams. | FR-004, SC-002 |
| C5 | Both modes render to the same knot for every valid diagram. | FR-006, SC-003 |
| C6 | Crossings always connect the correct partners and are never drawn between non-adjacent rows; crossing-alignment transfers are inserted as needed. | FR-007, FR-011 |
| C7 | Output is deterministic for a given `(items, mode)`. | FR-008, SC-005 |
| C8 | Empty/degenerate diagrams render without error; equivalent to `Legacy` where no avoidable movement exists. | FR-010 |
| C9 | The mode is an operating context: rotation honors `self.mode`; notation-only moves produce identical results regardless of mode. | FR-012 |
| C10 | Rotating in `PrecalculatedHeights` never increases the scanned feature count vs. the original, and strictly reduces it vs. `Legacy` rotation for diagrams with reversed-direction transfers. | SC-006 |

## Example notation (fidelity reference)

- `terrace` = `(0 (2 (4 (6 )5 )3 )1 (1 (3 (5 )6 )4 )2 )0` — primary
  reduced-transfer demonstration (US1, SC-001).
- `basket`, `ugly_trefoil` — crossing-bearing diagrams for C6.

Expected `PrecalculatedHeights` ASCII outputs are captured as `insta` snapshots
during implementation (Test-First) and reviewed before commit; `Legacy`
snapshots must be unchanged.
