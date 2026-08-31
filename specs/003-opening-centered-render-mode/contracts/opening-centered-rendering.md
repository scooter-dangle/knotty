# Contract: Opening-Centered Rendering

The interfaces this feature exposes, and the rules a consumer must hold to. Three surfaces: the
library API, the CLI example's environment, and the app's toggle.

## Library API

### New export

```rust
pub use render::RenderMode;   // src/lib.rs
```

### Changed signatures

```rust
impl Horiz {
    pub const fn display(&self, mode: RenderMode) -> [&'static str; DISPLAY_LINES];
    pub const fn display_with_borders(&self, mode: RenderMode) -> [&'static str; DISPLAY_WITH_BORDERS_LINES];
}

impl VerboseLine {
    pub fn display<const GRID_BORDERS: bool>(&self, mode: RenderMode) -> impl 'static + Iterator<Item = String>;
}

impl VerboseDiagram {
    pub fn from_abbreviated(knot: &AbbreviatedDiagram, mode: RenderMode) -> Result<Self, String>;
    pub fn display<'a, const GRID_BORDERS: bool>(&'a self, mode: RenderMode) -> impl 'a + Iterator<Item = String>;
}

impl AbbreviatedDiagram {
    pub fn try_ascii_print<const GRID_BORDERS: bool>(&self, mode: RenderMode) -> Result<String, String>;
    pub fn ascii_print<const GRID_BORDERS: bool>(&self, mode: RenderMode) -> String;
    pub fn try_ascii_print_compact<const GRID_BORDERS: bool>(&self, mode: RenderMode) -> Result<String, String>;
    pub fn ascii_print_compact<const GRID_BORDERS: bool>(&self, mode: RenderMode) -> String;
}

// free functions in src/diagram.rs, same treatment
pub fn try_ascii_print<const GRID_BORDERS: bool>(tuples: Vec<(u8, usize)>, mode: RenderMode) -> Result<String, String>;
pub fn ascii_print<const GRID_BORDERS: bool>(knot: Vec<(u8, usize)>, mode: RenderMode) -> String;
pub fn try_ascii_print_compact<const GRID_BORDERS: bool>(tuples: Vec<(u8, usize)>, mode: RenderMode) -> Result<String, String>;
pub fn ascii_print_compact<const GRID_BORDERS: bool>(knot: Vec<(u8, usize)>, mode: RenderMode) -> String;
```

This is a breaking change to a `0.1.0` crate with one in-tree consumer. Passing
`RenderMode::Standard` — or `RenderMode::default()` — reproduces today's output exactly.

### New method

```rust
impl VerboseDiagram {
    /// Canonical text for `mode`. Under `OpeningCentered` the eight retired
    /// characters are written as `_`; under `Standard` this equals `to_string()`.
    pub fn to_text(&self, mode: RenderMode) -> String;
}
```

### Unchanged

`Horiz::as_byte`, `Horiz::from_byte`, `Horiz::subsequent`, `impl FromStr for VerboseDiagram`,
`impl Display for VerboseDiagram`, and every move and rotation entry point.

## Rules

1. **Pairing.** A grid produced by `from_abbreviated(knot, m)` is only meaningful when drawn with
   `display::<_>(m)`. There is no runtime check; mismatching modes draws a disconnected picture. A
   grid parsed from diagram text (`FromStr`) carries no mode and may be drawn either way — that is
   the manual-mode case, and the same text drawing differently in the two modes is expected.
2. **Default preserves behaviour.** Every existing snapshot must be reproduced byte for byte by the
   `RenderMode::Standard` path. This is the regression guard for FR-008.
3. **Transfer-free equality.** For any knot whose `Standard` grid contains no transfer cell,
   `ascii_print::<B>(Standard)` and `ascii_print::<B>(OpeningCentered)` are equal, for both `B` and
   for the compact variants (FR-007).
4. **Retired characters.** Under `OpeningCentered` the eight retired variants draw nothing and
   `to_text` writes them as `_`; `to_text(m)` reaches a fixed point in one pass (FR-016).
5. **Never emitted.** `from_abbreviated(_, OpeningCentered)` never produces a retired variant (FR-005).
6. **One cell per level.** Under `OpeningCentered` a strand moving one level occupies exactly one cell
   and one column; no column is spent starting or finishing the movement (FR-017).
7. **Rotation is mode-independent.** `try_rotate_90_ccw` and `full_render_lines` always use
   `RenderMode::Standard`, whatever the caller has selected (R9).

## CLI example (`examples/ascii_print.rs`)

| variable | values | default | effect |
|---|---|---|---|
| `KNOTTY_OPENING_CENTERED` | `true` / anything else | unset | selects `RenderMode::OpeningCentered` |

Joins the existing `KNOTTY_GRID`, `KNOTTY_COMPACT` and `KNOTTY_PRINT_ABBREV`, read the same way. All
eight combinations of the three display variables must be producible (FR-010, SC-007). Because the
mode is a runtime value it is passed through the existing four-arm `match` rather than doubling it.

## App (`examples/knot-so-good`)

| element | behaviour |
|---|---|
| toggle | One button, rendered in **both** notation mode and manual diagram mode, reading "switch to opening-centered view" / "switch to standard view". |
| state | One `render_mode` on `Model`, one on `PersistedState`; both app modes read and write the same one (FR-012). |
| persistence | Survives reload under the existing `knotty_state` key; an unrecognised stored value falls back to `Standard` (FR-013). |
| notation mode | `update_modified` selects the mode when calling `try_ascii_print[_compact]`; `compact_text` builds the mode's grid and calls `to_text(mode)`. |
| manual mode | `render_manual(diagram, mode, borders)`; the cached `Option<VerboseDiagram>` is re-drawn at view time, so toggling while the text is invalid keeps the stale picture and redraws it in the selected mode once valid. |
| unchanged | `ascii_diagram_to_html`'s allow-list and the svgbob path — the new tiles use the same glyphs (R10). |
