# Contract: Bordered Rendering (`VerboseDiagram::display::<true>()`)

This feature adds no library API. It makes an existing, already-public rendering visible in the app
for the first time, so this document states what that rendering guarantees — the properties the app's
promises (spec FR-003, SC-001) rest on, and what the new `insta` snapshot locks down.

**Signature** (unchanged, `src/render.rs`):

```rust
impl VerboseDiagram {
    pub fn display<'a, const GRID_BORDERS: bool>(&'a self) -> impl 'a + Iterator<Item = String>
}
```

Each yielded `String` is one output line and ends with `\n`. `GRID_BORDERS = false` is what the app
draws today; `GRID_BORDERS = true` is what the boundary view draws.

## Cell geometry

| | Plain (`false`) | Bordered (`true`) |
|---|---|---|
| Columns per cell | 3 | 4 (`+---` / `\|   `) |
| Lines per row | 3 | 4 |
| Bottom row emits | 1 line | 2 lines (its border line and its first content line) |

Each cell contributes its **top** and **left** edge only. Consequences, both intended and accepted
(spec Assumptions, research R3):

- Boundary lines fall *between* cells; the picture's rightmost column and bottom row have no closing
  outer edge.
- The bordered view's content is a superset of the plain view's: for the bottom row it emits the same
  content line plus a border above it.

## Line arithmetic

For a diagram of `rows` rows (`rows` = the number of lines the text describes, spec FR-013 included):

| Property | Plain | Bordered |
|----------|-------|----------|
| Total lines | `3 × rows − 2` | `4 × rows − 2` |
| Border lines (`+---…`) | 0 | `rows` — exactly one per row |
| `+---` groups per border line | — | the row's cell count |
| Empty output | `rows == 0` | `rows == 0` |

Verified at `rows` ∈ {0, 1, 2, 4}; see the table in [../research.md](../research.md) R8.

Two consequences the app relies on:

- **One box per character** (SC-001): rows are padded to the diagram's width when parsed (feature 001
  FR-010), so every border line has one `+---` group per character of the widest text line, and there
  are as many border lines as text lines.
- **Both views are empty together**: `display::<false>()` and `display::<true>()` yield no lines for
  exactly the same diagrams. So the app's single "is there a picture?" test is view-independent, and
  the boundary view never draws an empty grid (FR-008).

## Character set

A bordered render contains only `' '`, `'+'`, `'|'`, and the characters a plain render already
contains (`( ) / \ _ -`). This matters because `ascii_diagram_to_html` panics on any byte outside its
allow-list; `+` and `|` are already on it (research R4). **The allow-list must not be narrowed.**

## Degenerate rows

A row with zero cells (a blank line in the text, spec FR-013) contributes a border line that is the
empty string rather than `+---…`, since a border line is built per cell. It is still one line, so the
arithmetic above holds. The app draws it as a blank line, which is what a row of no cells looks like.

## Stability

The snapshot added in `src/render.rs` pins this rendering. Changing it — for instance to close the
outer edge — is a library behaviour change that also changes `KNOTTY_GRID=true` output from
`examples/ascii_print.rs`, and is out of scope here.
