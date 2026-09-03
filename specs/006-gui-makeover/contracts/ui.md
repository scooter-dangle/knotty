# Contract: the app's user interface after the makeover

**Feature**: [spec.md](../spec.md) | **Consumers**: people using the app; `screenshots/capture.js`, which drives it by these names

The app has no API; its contract is what a user (or a script driving a browser) can find on the
page, by what name, and what it does. This lists every control, region and class the makeover
commits to. The capture script is rewritten against these names and is the executable check.

## Controls, by accessible name

Names are what assistive technology announces and what `getByRole` / `getByLabel` find.

### Two-state settings (radio pairs)

Each is a radio group with two labelled radios; exactly one is checked. Selecting the unchecked one
dispatches the message shown and the group re-renders with it checked.

| Group name | Options (radio labels) | Checked when | Dispatches | Present in |
|---|---|---|---|---|
| `mode` | `notation` / `manual` | `mode == Notation` / `Manual` | `Msg::SetMode(_)` | both modes |
| `display` | `picture` / `characters` | `display_mode == Svg` / `Ascii` | `Msg::DisplayMode(_)` | notation |
| `drawing` | `full` / `compact` | `!compact` / `compact` | `Msg::Compact(_)` | notation |
| `view` | `plain` / `bordered` | `!manual_borders` / `manual_borders` | `Msg::ManualBorders(_)` | manual |

Persistence is unchanged: the message handlers save state exactly as before.

### Buttons

| Name | Behaviour | Disabled when | Present in |
|---|---|---|---|
| `unknot`, `trefoil`, `square knot`, `knot 5_1` | load the preset (unchanged) | never | notation |
| `snapshot` | save a snapshot (unchanged) | as today: at the limit, or on an error | both |
| `rotate 90° CCW` | append the rotation move (unchanged) | moves unparseable | notation |
| `restore`, `delete` (per snapshot card) | unchanged | never | both |
| `Dismiss` (storage notice) | unchanged | — | both |
| `Download SVG` (link) | unchanged | — | notation |

Disabled controls keep their size and position and are dimmed (reduced opacity), not repainted;
placeholder text stays legible.

### Text inputs

| Accessible label | Element | Present in |
|---|---|---|
| `knot notation` | textarea, block-level, full width | notation |
| `moves` | textarea, block-level, full width | notation |
| `select a simplifying move`, `select a reärranging move`, `select a complecting move`, `select a changing move` | input + datalist (unchanged placeholders) | notation |
| `diagram text` | textarea (`textarea.manual-input`), block-level, full width | manual |

The two notation textareas gain visible labels (they have none today); their behaviour is unchanged.

## Regions

In document order:

1. **Storage notice** — `aside.notice` — only when `storage_error` is set; contains the message and
   `Dismiss`. Above everything else, as today.
2. **Toolbar** — `nav.toolbar` — wrapping row of `div.group` elements in the order: mode, presets,
   display, view, actions (groups absent in a mode are omitted, not hidden).
3. **Diagram region** — `section.diagram`, always present, `min-height` floor —
   - `div.canvas` — `overflow-x: auto`; contains `pre.ascii` (character drawing) or `div.picture`
     (rendered picture) or nothing.
   - `p.message` with `role="status"` — always present; the notation or manual error text, else
     empty.
4. **Encoding line** (notation) — `pre.encoding` — the modified diagram's notation, as today.
5. **Disclosures** — `details.compact-text` ("diagram text", notation) and
   `details.symbol-table` ("character reference", manual), as today.
6. **Inputs** — the textareas and move pickers listed above, stacked, block-level.
7. **Snapshot catalog** — `section.snapshot-catalog` — CSS grid of `article.snapshot-entry`
   cards, each: `div.snapshot-preview` (fixed height), the encoding `pre` (notation only), and
   `div.snapshot-actions` holding `restore` and `delete`.

## Stylesheet contract

`examples/knot-so-good/style.css`, linked from `index.html` with `<link data-trunk rel="css">`.
The classes above are the stable hooks. Values the spec's success criteria depend on:

| Selector | Property | Value | Spec |
|---|---|---|---|
| `pre.ascii`, `.snapshot-preview pre` | `font-family` | `"DejaVu Sans Mono", Menlo, Consolas, "Liberation Mono", monospace` | FR-008 |
| same | `line-height` | `0.8` | FR-008, SC-003 |
| same | `padding-block` | about `0.3em` | overhang, R1 |
| `.grid-h::after`, `.grid-v::after`, `.grid-cross::after` | drawn rule(s) | full cell width / height | FR-009 |
| `.grid` | `color` | `transparent` | glyph kept for copy, hidden from view |
| `section.diagram` | `min-height` | a fixed floor (≥ the trefoil's height) | FR-010 |
| `p.message` | `min-height` | one line | FR-011 |
| `.toolbar button`, `.toolbar label` | `min-height` | `2.5rem` | SC-005 |
| `textarea` | `width` / `max-width` | `100%` / about `40rem` | FR-018 |
| `div.canvas` | `overflow-x` | `auto` | FR-017 |
| `.snapshot-catalog` | `grid-template-columns` | `repeat(auto-fill, minmax(11rem, 1fr))` | FR-020 |
| `button:disabled`, `input:disabled` | `opacity` | about `0.5` (no background repaint) | FR-015 |

`index.html` gains `<meta name="viewport" content="width=device-width, initial-scale=1">` (FR-016).

## What does not change

- `PersistedState` and every persisted field; the storage key.
- The set of messages and their handlers; the mode/display/compact/borders transitions.
- The character drawing's text: same characters, rows and columns (FR-003).
- The picture: same renderer, same output; only its container is sized and scrolled (FR-002).
- Snapshot contents, limit, restore and delete; the "unreadable snapshot" notice (FR-021).
- The move pickers' placeholders and datalists; the Enter-key handling for Android Chrome.
