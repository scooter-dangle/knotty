# Phase 1 Data Model: Cell Boundary View in Manual Diagram Mode

No library types change. Everything below is in `examples/knot-so-good/src/main.rs`.

## Application state (`Model`)

| Field | Before | After | Notes |
|-------|--------|-------|-------|
| `manual_borders` | — | `bool` | New. The boundary view setting (spec: *Boundary view setting*). `false` = plain, `true` = bordered. Lives beside `compact`, the app's other display bool. |
| `manual_render` | `Option<String>` | `Option<knotty::VerboseDiagram>` | Changed. Was the rendered picture; becomes the last valid diagram, so either view can be produced from it at any moment (research R2). `None` means "no picture to show" — either nothing valid has been entered yet, or the valid text describes an empty diagram. |
| `manual_diagram` | `String` | unchanged | The entered text. Never touched by the toggle (FR-004). |
| `manual_error` | `Option<String>` | unchanged | `Some` ⟹ the picture, if any, is stale. Unchanged by the toggle (FR-007). |
| `manual_snapshots` | `Vec<PersistedManualSnapshot>` | unchanged | FR-011: no per-snapshot view. |

**Invariant (unchanged in spirit, restated for the new type)**: `manual_render` is only ever written
from *valid* text. A parse failure sets `manual_error` and leaves `manual_render` alone — that is what
keeps the stale picture on screen (FR-006).

**Derived, not stored**: the picture itself. `render_manual(&diagram, self.manual_borders)` is called
at view time for the main picture and for each snapshot preview. There is no cached rendering to
invalidate, so the toggle needs no update-side logic beyond setting the flag.

## Messages (`Msg`)

| Variant | Payload | Behaviour |
|---------|---------|-----------|
| `ManualBorders` | `bool` | New. No-op returning `false` when the value is unchanged (matching `Compact` and `DisplayMode`); otherwise sets `manual_borders` and returns `true`, which re-renders and saves. It must **not** call `update_manual()` — the text and its parse result are unaffected (FR-004). |

## Persisted state (`PersistedState`)

| Field | Type | Serde | Meaning |
|-------|------|-------|---------|
| `manual_borders` | `bool` | `#[serde(default)]` | New. Absent in state written before this feature ⟹ `false` ⟹ boundary view off (FR-002, FR-014). |

`PersistedState::from_model` copies `model.manual_borders`; `Model::create` copies it back. Both are
straight assignments — no enum mapping, unlike `mode` and `display_mode` (research R5).

`PersistedManualSnapshot` is **unchanged**: `{ diagram: String }`. The view is not recorded, and
restoring a snapshot does not change it (FR-011).

## State transitions

```text
                         toggle (Msg::ManualBorders)
        plain view  <───────────────────────────────>  bordered view
             │                                              │
             │  both views derive from manual_render;        │
             │  neither reads or writes manual_diagram,      │
             │  manual_error, or manual_snapshots            │
             ▼                                              ▼
        render_manual(diagram, false)              render_manual(diagram, true)
```

Orthogonal to the picture's staleness:

| `manual_render` | `manual_error` | Shown |
|-----------------|----------------|-------|
| `Some(d)` | `None` | picture in the current view, normal |
| `Some(d)` | `Some(e)` | picture in the current view, marked stale, error alongside (FR-006, FR-007) |
| `None` | `Some(e)` | error alone; no grid drawn (FR-008, spec edge case) |
| `None` | `None` | nothing (empty text) |

Toggling moves left-to-right in the diagram above and never up or down in this table.

## Rendering helper

```rust
fn render_manual(diagram: &knotty::VerboseDiagram, borders: bool) -> String
```

Replaces the current one-argument `render_manual`. The `borders` branch is the feature's only
dispatch over the library's `GRID_BORDERS` const generic (research R1). Its output contract — line
counts, border lines, one box per cell — is in
[contracts/bordered-rendering.md](./contracts/bordered-rendering.md).
