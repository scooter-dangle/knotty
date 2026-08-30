# Phase 0 Research: Cell Boundary View in Manual Diagram Mode

All findings below were checked against the code and, where marked *verified*, against actual output
from a scratch binary run at `1.94.0` and deleted afterwards.

## R1 — A runtime toggle over a const generic

**Decision**: Branch at the call site.

```rust
fn render_manual(diagram: &knotty::VerboseDiagram, borders: bool) -> String {
    if borders {
        diagram.display::<true>().collect()
    } else {
        diagram.display::<false>().collect()
    }
}
```

**Rationale**: `VerboseDiagram::display<const GRID_BORDERS: bool>` takes a *const* generic. A runtime
`bool` cannot be threaded into it, so the branch has to exist somewhere; the only question is where.
Putting it in the one helper the app already has means exactly one branch in the whole feature, and
both call sites (the main picture, the snapshot previews) go through it unchanged in shape.

**Alternatives considered**:

- *Add a runtime-dispatch method to the library* (`display_with(borders: bool)`). Rejected: it is a
  new public API to avoid a three-line `if` in one consumer, and Article I asks for library API that
  is independently useful — this would exist solely for the app's convenience.
- *Make the app store a function pointer* (`fn(&VerboseDiagram) -> String`) chosen once. Rejected:
  the same branch, moved somewhere less obvious, plus a field that cannot be serialized or compared.

## R2 — What the app caches so a stale picture can change view

**Decision**: `Model.manual_render` changes from `Option<String>` (a rendered picture) to
`Option<knotty::VerboseDiagram>` (the last valid diagram). Rendering moves to view time.

**Rationale**: FR-006 keeps the last valid picture on screen when the text goes bad, and FR-007 says
toggling the view while it is stale must redraw *that same picture* in the other view. A cached
`String` is already rendered — it cannot be re-rendered the other way — so a cached string would
force either a second cached string or a re-parse of text that is currently invalid. Caching the
diagram makes both views derivable at any moment and makes the toggle a pure view concern that
touches no update logic at all.

Rendering per view pass costs a linear walk over a few hundred cells. The snapshot catalog already
*parses and renders* every snapshot on every view pass, so this is strictly cheaper than code that
already ships.

**Verified**: for every input tried (a four-row trefoil, a two-row unknot, `""`, `"\n"`, `"(\n"`),
`display::<false>()` and `display::<true>()` are empty for exactly the same inputs. So the existing
"is there a picture at all?" test keeps its meaning regardless of view, and can be written without
allocating as `diagram.display::<false>().next().is_some()`.

**Alternatives considered**:

- *Cache both rendered strings.* Rejected: two fields that must be kept in step, to avoid one cheap
  render — the redundancy the current single-string cache already demonstrates the cost of.
- *Re-parse `manual_diagram` on toggle.* Rejected: it is invalid exactly when this matters. The
  fallback would be an `unwrap` on text known to be bad, or silently blanking the picture — which
  FR-006 forbids.

## R3 — The open outer edge

**Decision**: Use the bordered rendering exactly as it is. Do not close the picture's right or bottom
outer edge.

**Rationale**: Each cell's `display_with_borders()` supplies a top edge and a left edge (`+---` and
`|   `), so boundaries appear *between* cells and the rightmost column and bottom row are left open.
The spec accepts this in Assumptions and calls it out as an edge case. Closing it would mean changing
the library's per-cell strings and the emission loop, which changes `KNOTTY_GRID=true` output for the
command-line example and any snapshot of it — a library behaviour change for a cosmetic gain, on a
feature whose whole point is that no library behaviour changes.

**Verified**: the bottom row is emitted with two of its four lines (its top border and its first
content line), mirroring the plain view, which emits one of three. The bordered view therefore shows
strictly more than the plain view and never less.

## R4 — HTML rendering of the border characters

**Decision**: No change to `ascii_diagram_to_html`.

**Rationale**: it maps each byte and calls `unreachable!("bug!")` on anything outside its allow-list,
so an unexpected character is a panic in the browser, not a rendering glitch. Its allow-list is
`' ' '(' ')' '/' '\\' '_' '-' '+' '|' '0'..='9'` — `+` and `|`, the only two characters the bordered
view introduces, are already there. **Verified** by reading the match arm; a bordered render of the
trefoil contains no byte outside that set.

Implementation note for tasks: the allow-list must not be narrowed as a "cleanup" — it is load-bearing
for this feature.

## R5 — Persisting the setting

**Decision**: `#[serde(default)] manual_borders: bool` on `PersistedState`.

**Rationale**: `bool` defaults to `false`, which delivers FR-002 (off by default) and FR-014 (state
saved before this feature loads with the view off) with no code. This is exactly how the existing
`compact` field behaves, so the two display bools stay symmetric.

**Alternatives considered**:

- *A `PersistedManualView` enum with `#[serde(other)]`*, mirroring `PersistedMode` and
  `PersistedDisplayMode`. Rejected: those enums exist because their fields have three or more
  meaningful states and needed a forward-compatible unknown case. A two-state view has nothing to be
  forward-compatible *with*, and `serde` already treats an unknown value of a `bool` field as an
  error the existing storage-error path reports.
- *Store it per snapshot.* Rejected outright by FR-011: the snapshot records text, and the view is a
  preference, not a property of a picture.

## R6 — The control, and whether `index.html` needs styles

**Decision**: A plain `<button>` in `manual_view`, labelled in the app's existing
`switch to {other}` idiom and placed next to the `snapshot` button. No CSS change.

**Rationale**: notation mode already toggles display with `switch to compact display` /
`switch to full display` buttons whose label names the view you would move *to*, which satisfies
FR-017 (clear effect, current state readable) without inventing a second control idiom for one
checkbox. The picture keeps its existing `.manual-render` class in both views, so the stale styling
(FR-006, FR-007) applies unchanged and no new selector is needed.

A bordered picture is wider than a plain one — four columns per cell rather than three. It is drawn
in the same `<pre>` as before, so it overflows exactly the way a wide plain picture already does,
which is what the spec's edge case asks for.

## R7 — Snapshot previews

**Decision**: previews render through the same `render_manual(diagram, self.manual_borders)` call as
the main picture.

**Rationale**: FR-010 wants the catalog and the main picture to agree. The preview code already parses
each snapshot's text per view pass and calls the render helper, so this is a one-argument change with
no new state and nothing new persisted (FR-011).

## R8 — Test strategy

**Decision**:

1. `src/render.rs` — an `insta` snapshot of `display::<true>()` for a parsed diagram. **Verified**
   that no test anywhere exercises `VerboseDiagram::display::<true>()` today: the only `::<true>` uses
   in the tree are in `examples/ascii_print.rs`, driven by the `KNOTTY_GRID` environment variable.
   This feature makes that rendering user-visible, and Article III asks for snapshot coverage of
   rendering paths.
2. `examples/knot-so-good/src/tests.rs` — the persisted field defaults to `false` when absent from
   saved JSON (FR-002, FR-014) and round-trips when `true` (FR-012).
3. `examples/knot-so-good/src/tests.rs` — the geometry the feature promises (SC-001), asserted on the
   library output rather than the DOM, since the app has no browser harness.

**Geometry invariants**, verified over the trefoil (4×7), the unknot (2×2), `""`, `"\n"`, and `"(\n"`,
and written up in [contracts/bordered-rendering.md](./contracts/bordered-rendering.md):

| Rows of text | Plain lines | Bordered lines | Border lines | `+---` groups per border line |
|--------------|-------------|----------------|--------------|-------------------------------|
| 0            | 0           | 0              | 0            | —                             |
| 1            | 1           | 2              | 1            | width                         |
| 2            | 4           | 6              | 2            | width                         |
| 4            | 10          | 14             | 4            | width                         |

That is `3 × rows − 2` plain and `4 × rows − 2` bordered, with one border line per row and one
`+---` group per cell — the one-box-per-character correspondence SC-001 states.

**Not tested**: the toggle's effect on the DOM, and `localStorage` itself. The app has no browser
harness and feature 001 deliberately left that glue untested as too thin; nothing here changes that
trade-off. FR-003 through FR-010 are covered by the geometry and persistence tests plus the manual
walkthrough in [quickstart.md](./quickstart.md).
