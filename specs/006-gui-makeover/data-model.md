# Phase 1 Data Model: GUI Makeover

**Feature**: [spec.md](./spec.md) | **Research**: [research.md](./research.md)

This feature changes no data. Every persisted type, every message and every model field in the app
keeps its name, shape and meaning (spec FR-001). What follows records that, and names the
presentation-only structures the view gains.

## Unchanged: `PersistedState` and its parts

```rust
struct PersistedState {            // examples/knot-so-good/src/main.rs, serialized to localStorage
    diagram: String,
    moves: String,
    display_mode: PersistedDisplayMode,   // svg | ascii | other
    compact: bool,
    snapshots: Vec<PersistedSnapshot>,
    mode: PersistedMode,                  // notation | manual | other
    manual_diagram: String,
    manual_snapshots: Vec<PersistedManualSnapshot>,
    manual_borders: bool,
}
```

No field is added, removed or renamed; `STORAGE_KEY` stays `"knotty_state"`. State written by the
current app loads into the new one and yields the same settings (spec edge case; SC-007). The
existing app tests that round-trip this struct are the guard.

## Unchanged: `Model`, `Msg`, `Mode`, `DisplayMode`

The four two-state settings map to the same model fields and the same messages as today. The new
controls dispatch exactly the message the button they replace dispatched:

| Setting | Model field | Message dispatched | Values |
|---|---|---|---|
| notation / manual | `mode: Mode` | `Msg::SetMode(Mode)` | `Notation`, `Manual` |
| picture / characters | `display_mode: DisplayMode` | `Msg::DisplayMode(DisplayMode)` | `Svg`, `Ascii` |
| full / compact | `compact: bool` | `Msg::Compact(bool)` | `false`, `true` |
| plain / bordered | `manual_borders: bool` | `Msg::ManualBorders(bool)` | `false`, `true` |

Every one of these handlers already returns `false` (no re-render, no save) when the requested state
equals the current one, which is what a radio pair needs: selecting the already-checked segment is
not possible, and a stray duplicate event is harmless.

The error strings the view now shows in a fixed place already exist:

| Where shown | Source field | Type |
|---|---|---|
| notation-mode message line | `ascii_modified_diagram` | `Result<String, String>` — `Err` is the message |
| manual-mode message line | `manual_error` | `Option<String>` |
| page-top notice | `storage_error` | `Option<String>` |

`error_to_html` (which folds the notation error into the characters display's HTML) is removed;
`ascii_html_diagram` is built only from the `Ok` side. That is a view change: the field's type and
contents do not change.

## New, view-only: the diagram region

Not a type — a fixed arrangement of elements the view emits in both modes, in every state:

```text
section.diagram                 min-height floor; grows for tall drawings, never collapses
├── div.canvas                  overflow-x: auto; holds either
│   ├── pre.ascii               the character drawing (or nothing)
│   └── div.picture             the rendered picture (notation mode, picture display)
└── p.message[role=status]      always present; empty, or the error text
```

Invariant (FR-010–FR-012): the vertical position of everything below `section.diagram` depends
only on the *height of the drawing*, never on whether the drawing is present, empty or erroneous.

## New, view-only: control groups

The toolbar's children are groups, each a fixed set of controls in a fixed order:

| Group | Contents | Modes |
|---|---|---|
| mode | segmented pair: notation / manual | both |
| presets | buttons: unknot, trefoil, square knot, knot 5_1 | notation |
| display | segmented pair: picture / characters; segmented pair: full / compact | notation |
| view | segmented pair: plain / bordered | manual |
| actions | snapshot button | both |

Group order and membership are part of the UI contract ([contracts/ui.md](./contracts/ui.md)).

## New, view-only: character classes in the drawing

`ascii_diagram_to_html` maps each byte of a drawing to HTML. It gains one distinction:

| Byte | Emitted as | Class | Why |
|---|---|---|---|
| `+` | `<span class="grid grid-cross">+</span>` | grid | bordered view only; drawn as a CSS cross |
| `-` | `<span class="grid grid-h">-</span>` | grid | bordered view only; drawn as a CSS rule |
| `\|` | `<span class="grid grid-v">\|</span>` | grid | bordered view only; drawn as a CSS rule |
| ` ` `(` `)` `/` `\` `_` `0`–`9` | text | — | the diagram itself; unchanged |
| `\n` | `<br/>` | — | unchanged |

The set of accepted bytes is unchanged and the `unreachable!` on any other byte stays. One
character still produces exactly one cell (FR-003).
