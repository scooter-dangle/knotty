# Baseline: how the app looks before the makeover

**Captured**: 2026-09-03, from the app as built at `644bafd`, in a Chromium browser.

**Screenshots**: [`screenshots/baseline/`](./screenshots/baseline/). Every state was captured twice:

| Prefix | Viewport | Notes |
|---|---|---|
| `desktop-` | 1280 × 800 | Full-page captures; taller pages are captured in full. |
| `mobile-` | 390 × 844, 2× pixel density, touch | A phone-sized viewport. Because the page declares no viewport, the browser lays it out **980 px wide and scales it down**, so every mobile capture is 980 CSS px across. |

Two extra `-zoom` captures crop just the drawn diagram at 4× density, to show the character
spacing problem up close. The `*-measurements.json` files record the page size for each state and
the vertical position of the main text box in the valid, empty and error states — the numbers behind
the layout-shift findings below.

The captures can be re-taken after the makeover, for a before/after comparison, with
[`screenshots/capture.js`](./screenshots/capture.js): build the app, serve the build directory over
plain HTTP, then run the script with `BASE_URL` pointing at the served page and `OUT_DIR` at the
directory to write into. It needs only a browser-automation library that the repository already
relies on for nothing else; it is a capture aid, not part of the app.

## The states captured

| State | What it shows |
|---|---|
| `notation-empty` | Fresh load, no diagram, nothing typed. |
| `notation-trefoil-svg` | Trefoil, drawn as a picture. |
| `notation-trefoil-ascii` | Trefoil, drawn as characters. |
| `notation-trefoil-ascii-compact` | Same, in the compact drawing. |
| `notation-large-ascii` | Knot 5_1 with six bulges added — a wide diagram, as characters. |
| `notation-large-svg` | The same wide diagram, as a picture. |
| `notation-diagram-error` | Notation that does not parse (`(0 (2 xx`), picture display selected. |
| `notation-moves-error` | Valid notation with an unparseable line in the moves box. |
| `notation-many-snapshots` | Nine notation snapshots, the maximum. |
| `manual-empty` | Manual diagram mode with nothing typed. |
| `manual-trefoil` | Manual trefoil. |
| `manual-trefoil-bordered` | Manual trefoil with cell borders shown. |
| `manual-error` | Manual trefoil plus an unrecognized character. |
| `manual-many-snapshots` | Nine manual snapshots, the maximum. |
| `storage-error` | Load with corrupt saved state; the recovery banner is showing. |

## Findings

### Controls

- **Every two-state control is a plain button labelled with the *other* state.** "switch to Ascii
  display", "switch to compact display", "switch to manual diagram mode", "switch to bordered view".
  Nothing on screen says which display, density, mode or view is *current*; the reader has to
  invert the label in their head. Four controls work this way: mode, picture/characters, full/compact,
  plain/bordered.
- **Disabled controls are painted a heavy grey** (`#a0a0a0`) with the placeholder text nearly the
  same shade as the background, so "select a simplifying move" in the disabled state is close to
  unreadable (`desktop-notation-empty`, `desktop-notation-moves-error`). The disabled "snapshot"
  button gets the same treatment.
- The toolbar is a single unbroken row of default-styled browser buttons with no grouping: the mode
  switch, the four preset knots, the two display toggles and the snapshot action sit side by side.
- The page is set in the browser's default serif face at default sizes; the "Download SVG" link is
  set at 8 px.

### Layout shift on empty and error states

Measured as the vertical position of the first text box, in CSS px, at the desktop viewport:

| Mode | Valid diagram | Empty / error | Shift |
|---|---|---|---|
| Notation | 305 | 133 (notation error) | **172 px up** |
| Manual | 211 | 8 (empty) | **203 px up** |
| Manual | 211 | 245 (error) | 34 px down |

- **Notation mode, picture display, bad notation**: the picture simply vanishes and nothing replaces
  it. No error text is shown anywhere (`desktop-notation-diagram-error`) — the error message exists
  but is only rendered in the characters display. The whole form below jumps up into the space.
- **Notation mode, empty**: same collapse; the "diagram text" disclosure and the inputs float up under
  the toolbar with a blank band above them (`desktop-notation-empty`).
- **Manual mode, empty**: with no picture to draw, the text box has no block above it and, being an
  inline element, it sits *on the same line as the toolbar buttons*, to their right, at the very top
  of the page (`desktop-manual-empty`). Typing the first valid line makes the picture appear above it
  and the box drops ~200 px.
- **Manual mode, error**: the last good picture stays (dimmed to 40%) but the red error line is
  inserted between it and the text box, pushing the box down 34 px on every keystroke that toggles
  between valid and invalid.
- **Bad moves**: the four move pickers and the rotate button all flip to the grey disabled paint at
  once.
- **Storage error**: the recovery banner is inserted above the toolbar, pushing everything down by
  its height.

### Character-drawn diagrams

The diagram is drawn in the browser's default `monospace` at 13 px with `line-height: normal`.
Zoomed (`desktop-notation-trefoil-ascii-zoom`, `desktop-manual-trefoil-bordered-zoom`):

- `/` and `\` do not reach the top or bottom of their line box, so a diagonal strand that continues
  across rows is drawn as a run of short dashes with a visible gap at every row boundary.
- `_` sits on the baseline, so a horizontal strand has empty space above it up to the next row and
  does not meet the `/` or `\` that continues it at the row above.
- `(` and `)` are shorter than the row, leaving gaps where they join the strands above and below.
- In the bordered view, `|` is likewise broken at each row and `+` does not meet the `-` on either
  side, so the cell grid reads as dashed rather than ruled.

### Size and overflow

- The picture is drawn at its natural size with no bound. The wide knot is 1,070 px across in the
  picture display and pushes the page wider than the viewport on the phone
  (`mobile-notation-large-svg`: 1,088 px page width against a 980 px layout), so the phone shows a
  horizontal scrollbar for the whole page.
- Snapshot entries are full-width bordered boxes stacked one per row, each holding a 150 × 150 px
  preview (or the character picture) with `restore`/`delete` under it. Nine notation snapshots make
  the page 2,566 px tall; nine manual snapshots make it 2,750 px. Most of each box is empty.
- Snapshot previews in manual mode are drawn at the same size as the main picture, with no
  indication of which one is current.

### Mobile

- No viewport declaration, so the phone renders a 980 px desktop layout scaled to 40%: the
  toolbar buttons are roughly 4 px tall on screen, text boxes are unreadably small, and the user
  must pinch-zoom to do anything (`mobile-notation-trefoil-svg`).
- The text boxes keep their default 20-column width, so knot notation and moves wrap after a
  handful of tokens.
