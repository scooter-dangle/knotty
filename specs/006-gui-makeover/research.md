# Phase 0 Research: GUI Makeover

**Feature**: [spec.md](./spec.md) | **Date**: 2026-09-03

All experiments ran against the app as built at `644bafd`, in the same Chromium the baseline
screenshots came from. The scripts and images referenced here live under
[`screenshots/`](./screenshots/).

## R1 — How to make character-drawn strands continuous (spec FR-008, FR-009, SC-003)

**Decision**: Keep the drawing as text. Set the diagram `pre` to a monospace stack led by the
DejaVu Sans Mono geometry — `"DejaVu Sans Mono", Menlo, Consolas, "Liberation Mono", monospace` —
with `line-height: 0.8` and vertical padding of about `0.3em` so the overhanging ink is not clipped
by a scrolling container. Draw the three characters that appear only in the bordered view — `+`,
`-`, `|` — as CSS rules spanning their whole cell, with the glyph itself kept in the DOM (so text
selection and copy still work) but made transparent.

**Rationale**: The gaps have two causes with two different fixes.

*Vertical gaps between rows* come from the line box being taller than the glyph ink: at the
default `line-height: normal` DejaVu Sans Mono's line box is 19 px at 16 px type, while `/` and
`\` ink spans roughly 13 px, so every row boundary opens a gap. Shrinking the line box below the
ink height makes adjacent glyphs overlap and the strokes meet. `measure-gaps.js` renders four test
joins — a three-row `\` diagonal, a `|` column, a `___` run above a `\` (the Line→TransferDown
join), and `/ ( \` stacked as in the opening tile — as real DOM in Chromium at 4× density and
counts the ink-free pixel rows between the first and last ink row of each:

| Font (16 px) | line-height | diagonal | `\|` column | line→diag | paren join | line box |
|---|---|---|---|---|---|---|
| DejaVu Sans Mono | normal | 2 gaps, 11.5 px | 2, 6.0 | 2, 9.3 | 2, 10.3 | 19.0 px |
| DejaVu Sans Mono | 1 | 2, 5.5 | 0 | 2, 3.3 | 2, 4.3 | 16.0 |
| DejaVu Sans Mono | 0.9 | 2, 2.5 | 0 | 1, 1.8 | 2, 1.3 | 14.4 |
| DejaVu Sans Mono | 0.85 | 1, 0.8 | 0 | 0 | 1, 0.3 | 13.6 |
| **DejaVu Sans Mono** | **0.8** | **0** | **0** | **0** | **0** | **12.8** |
| Liberation Mono | 0.8 | 2, 2.5 | 0 | 1, 1.3 | 1, 1.3 | 12.8 |
| Liberation Mono | 0.75 | 2, 0.5 | 0 | 1, 0.3 | 1, 0.3 | 12.0 |
| FreeMono | 0.75 | 0 | 2, 0.5 | 0 | 1, 1.0 | 12.0 |
| Unifont | 0.75 | 2, 4.0 | 0 | 2, 3.0 | 2, 2.0 | 12.0 |

At 0.8 every join closes in DejaVu Sans Mono. Menlo, the macOS default monospace, is a DejaVu
derivative with the same vertical metrics, so the same value is expected to hold there; Consolas
on Windows was not measurable here and is covered by the spec's "to the extent the typeface
allows" (the edge case on fallback fonts). Going lower than 0.8 buys nothing on DejaVu and starts
to visibly squash the picture, so 0.8 is the value.

Rendering the real trefoil with that setting confirms it visually —
[`research/research-trefoil-lh-0.8.png`](./screenshots/research/research-trefoil-lh-0.8.png)
against [`research-trefoil-lh-normal.png`](./screenshots/research/research-trefoil-lh-normal.png):
the diagonals run unbroken through three rows, `(` and `)` meet the strands above and below, and
the `___` runs sit at the bottom of their row where the strand below picks them up. The `___`-then-
`/` join on the same row was already fine at baseline, because both glyphs sit on the baseline.

*Horizontal gaps in the bordered grid* have a different cause: the `-` glyph does not span its
advance width in any of the measured fonts, so `+---+` stays dashed at every line-height
([`research-bordered-lh-0.8.png`](./screenshots/research/research-bordered-lh-0.8.png) shows the
vertical rules joined and the horizontal ones still dashed). No line-height fixes this. `+`, `-`
and `|` are emitted only by the bordered view (`Horiz::display_with_borders`); the plain drawing
never uses them. The app already turns the drawing into HTML one character at a time
(`ascii_diagram_to_html`), so wrapping those three in a span with a class costs nothing, and CSS
can draw a full-cell horizontal rule, vertical rule, or cross behind a transparent glyph. That makes
the grid ruled regardless of typeface and leaves the diagram characters untouched (FR-003: still one
character per cell).

*Overhang*: with the line box shorter than the ink, the first and last rows' glyphs extend outside
the `pre`'s box. Inside an `overflow-x: auto` wrapper (needed for FR-017) that overflow would be
clipped or would spawn a vertical scrollbar. Vertical padding on the `pre` of roughly `0.3em` (the
difference between ink height and line box, split top and bottom) keeps the ink inside the box.

**Alternatives considered**:

- *Ship a web font designed for box drawing* (a bitmap-derived terminal face where `/`, `\`, `|`
  and `_` fill the cell). Rejected: it adds a binary asset and a network fetch to a page that has
  neither, and constitution V's spirit is against it when a CSS setting does the job on the two
  most common default monospace faces.
- *Draw the diagonals with CSS too* (a gradient in a `1ch × 1lh` cell). Rejected: at that point
  the drawing is no longer text, which is what the user asked to keep; it would be a second
  renderer next to the one the user asked to retain.
- *`transform: scaleY` on the whole `pre`*. Equivalent to line-height for the row pitch but also
  squashes every glyph; line-height keeps the glyph shapes.

## R2 — What form the two-state controls take (spec FR-004 to FR-007, SC-001)

**Decision**: All four two-state settings become **segmented radio pairs**: a `fieldset`/group with
a visually hidden `<input type="radio">` per state and a `<label>` per state, styled as one
two-segment control where the checked segment is filled. The radio's `onchange` dispatches the
message the old button dispatched (`SetMode`, `DisplayMode`, `Compact`, `ManualBorders`), each of
which already carries the target state and already no-ops when the state is unchanged.

**Rationale**: Native radios give the spec's accessibility requirements for free — keyboard
operation (arrow keys within the group, Tab between groups), a role and checked state announced by
assistive technology (FR-006) — with no script beyond the existing message. Both states are named
on screen and the active one is marked by the fill (FR-004, SC-001). Using the same form for all
four keeps the toolbar uniform; the spec's assumption allowed a switch for the on/off pairs, but
"full / compact" and "plain / bordered" read better as named pairs than as an "on" that has to be
decoded.

**Alternatives considered**:

- *`<button aria-pressed>`* per state — needs manual keyboard grouping and does not convey
  exclusivity.
- *`<input type="checkbox" role="switch">`* — names only one state; would need the second name
  written beside it anyway.
- *A `<select>`* — hides the inactive state until opened.

## R3 — How to stop the layout shifting (spec FR-010 to FR-015, SC-002, SC-004)

**Decision**: Introduce a **diagram region**: a block that always renders, in both modes, with a
`min-height`, containing a scrolling *canvas* wrapper for the picture or characters and a
*message* line that always exists (`min-height: 1lh` equivalent, empty when there is nothing to
say). The manual-mode text box and the notation-mode inputs become block-level, full-width
elements below the region. The notation error is shown in the message line in both displays; the
existing `error_to_html`, which folds the message into the characters display's HTML, is replaced
by reading the `Err` side of `ascii_modified_diagram` directly in the view. Disabled controls are
dimmed with reduced opacity instead of repainted grey, so their size, position and placeholder
legibility do not change.

**Rationale**: The baseline shifts come from three things, all in the view: the picture's block
disappearing (notation: 172 px, manual: 203 px), the error paragraph being inserted (manual: 34 px),
and the text box being inline so that with nothing above it it joins the toolbar's line. Reserving
the region and the message line fixes the first two; making the box block-level fixes the third.
The model already holds the error strings (`ascii_modified_diagram: Result<String, String>`,
`manual_error: Option<String>`); the view just needs to show them in the same place. Nothing is
added to the model, so the state machine is untouched (FR-001).

`min-height` is a floor, not a ceiling: a tall diagram grows the region (spec edge case). A
diagram wider than the region scrolls inside the canvas wrapper (FR-017).

**Alternatives considered**:

- *Retain the last valid notation-mode picture during an error* (as manual mode does). Rejected as
  not required by the spec and needing a new model field; the reserved region alone meets FR-010.
- *Position the message absolutely over the picture*. Rejected: a multi-line message would cover
  the drawing; the spec's edge case wants it to grow downward.

## R4 — Phone layout (spec FR-016 to FR-019, SC-005)

**Decision**: Add `<meta name="viewport" content="width=device-width, initial-scale=1">`. Lay the
page out as a single column of stacked blocks with a `max-width` of about 64 rem and side padding;
the toolbar is a wrapping flex row whose children are *groups* (mode; presets; display settings;
actions), each group itself a flex row with `flex-wrap: nowrap`-by-default so groups wrap as units
(FR-019). Buttons and segmented controls get a `min-height` of 2.5 rem (40 px) for tap targets.
Text boxes are `width: 100%` with a `max-width` of about 40 rem (FR-018). The canvas wrapper is
`overflow-x: auto` and `max-width: 100%` so a wide drawing scrolls inside it (FR-017).

**Rationale**: The missing viewport tag is the whole cause of the 980 px scaled layout; everything
else is ordinary responsive layout. Nothing needs a media query at this scale except, optionally,
tightening padding below ~480 px.

**Alternatives considered**: A two-column desktop layout (controls beside the picture). Rejected
for a "basic makeover": a single column already reads well at both sizes and does not need a
breakpoint to reflow.

## R5 — Snapshot catalog (spec FR-020, FR-021, SC-006)

**Decision**: The catalog becomes a CSS grid — `repeat(auto-fill, minmax(11rem, 1fr))` — of cards.
Each card has a fixed-height preview box (the existing 150 × 150 px scalable SVG in notation mode;
in manual mode the character drawing at a reduced type size inside a box of the same height that
scrolls if the drawing is larger), the encoding line where one exists today, and a row with the
restore and delete buttons. The markup order and the messages (`Restore*`, `Delete*`) are
unchanged; only classes and wrappers are added.

**Rationale**: Cards of equal height in an auto-fill grid halve the page at desktop width with nine
snapshots (baseline 2,566 / 2,750 px) and stack to one column on a phone without a breakpoint.

## R6 — Where the styles live and how they ship

**Decision**: Move the inline `<style>` out of `index.html` into `examples/knot-so-good/style.css`,
referenced with `<link data-trunk rel="css" href="style.css">`. Trunk 0.21.4 (the pinned version)
hashes and copies it into `dist/` and rewrites the link, so the GitHub Pages deploy with
`--public-url` needs no change.

**Rationale**: The stylesheet is about to grow from 40 lines to a few hundred; a file is easier to
read and diff than an inline block. No new build tooling, no new dependency.

**Alternatives considered**: Keeping it inline (works, but crowds the HTML shell); a CSS framework
(adds a dependency and a download for a page with ten controls).

## R7 — Testing a cosmetic change

**Decision**: Three layers.

1. **Host-target unit tests** in the app crate (`cargo test --manifest-path
   examples/knot-so-good/Cargo.toml`) for every pure function the makeover adds or changes:
   `ascii_diagram_to_html` classifying grid characters, and any helper that derives a control's
   state or label. The existing 26 tests must keep passing unchanged, which is SC-007's evidence
   that the persisted state and behaviour are untouched.
2. **The capture script** (`screenshots/capture.js`), updated to drive the new controls by their
   accessible names, re-taken into `screenshots/after/` and compared against the baseline —
   SC-002 (the script already records the text box's position in the valid, empty and error
   states), SC-005 (page width per state) and SC-006 (page height with nine snapshots) are read off
   its measurements file; SC-001, SC-003, SC-004 and SC-008 are read off the images.
3. **`measure-gaps.js`** re-run against the shipped stylesheet's values as the check for SC-003.

**Rationale**: The app's view code has never had browser tests and the repository has no browser
test harness; the capture script already exists and is the cheapest way to assert layout facts.
Making it a CI step is out of scope (it needs a served build and a browser).
