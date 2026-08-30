# Feature Specification: Cell Boundary View in Manual Diagram Mode

**Feature Branch**: `claude/diagram-ascii-boundary-view-pk4jjf`

**Created**: 2026-08-30

**Status**: Draft

**Input**: User description: "Add optional view mode to manual diagram entry mode (spec 001) in GUI where the ASCII boundary lines are included"

## Overview

Manual diagram mode (spec 001) lets someone type one character per cell and watch the picture
appear. The picture it shows is unbroken ASCII art: the cells that the typed characters name are
drawn edge to edge with no seam between them. That is exactly right for judging what the diagram
looks like, and exactly wrong for answering the question a person actually has while typing —
*which character drew that bit?*

A cell is three characters wide and three lines tall, arcs run across cell edges, and a mistyped
character shows up somewhere the eye cannot easily trace back to the text. Today the only way to
find it is to count characters and lines by hand — the same counting the compact format was
introduced to eliminate.

This feature adds an optional view of the same picture with the cell boundaries drawn in: a ruled
grid in which every typed character owns exactly one visible box. Nothing about the format, the
text, or the diagram changes — only how the rendering is displayed while the view is on.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - See which character drew which part of the picture (Priority: P1)

Someone typing compact diagram text in manual mode sees a cell in the picture that is not what they
meant. They turn on the boundary view, and the picture is redrawn with every cell boxed. They count
boxes across and down — or simply look, because the boxes line up one-for-one with the characters
they typed — find the offending cell, and fix that one character.

**Why this priority**: This is the entire feature. Everything else is persistence and consistency
around it.

**Independent Test**: Open the app in manual mode, enter the compact text for a known diagram, turn
the boundary view on, and confirm the picture gains cell boundaries with exactly one box per typed
character and one row of boxes per line of text. Delivers value with no other change.

**Acceptance Scenarios**:

1. **Given** manual mode with valid diagram text and the boundary view off, **When** the user turns
   the boundary view on, **Then** the same picture is shown with cell boundaries drawn, and the
   entered text is unchanged.
2. **Given** manual mode with the boundary view on, **When** the user turns it off, **Then** the
   picture returns to the seamless ASCII rendering shown before.
3. **Given** manual mode with the boundary view on, **When** the user edits the text, **Then** the
   boundary-view picture updates on every edit exactly as the plain picture does, with no further
   action.
4. **Given** text of N characters per line and M lines, **When** it is shown in the boundary view,
   **Then** the picture contains M rows of N boxes — one box per typed character, in the same
   arrangement as the text.
5. **Given** ragged text whose rows are shorter than the longest row, **When** it is shown in the
   boundary view, **Then** the inferred trailing empty cells appear as boxes like any other cell, so
   the grid stays rectangular.
6. **Given** an empty text box, **When** the boundary view is on, **Then** no picture is shown, the
   same as with the view off.

---

### User Story 2 - The view survives mistakes and reloads (Priority: P2)

Someone works in the boundary view over a session: they mistype a character, correct it, switch to
notation mode and back, and reload the page. The view they chose is the view they keep.

**Why this priority**: Without it the feature is still fully usable, just annoying — the setting
would silently reset and have to be re-chosen. It is the difference between a toggle and a
preference.

**Independent Test**: Turn the boundary view on, reload the page, and confirm manual mode comes back
with the boundary view still on and the same text.

**Acceptance Scenarios**:

1. **Given** manual mode with the boundary view on and a valid picture on screen, **When** the user
   types an unrecognized character, **Then** the last valid picture stays on screen in the boundary
   view, marked stale, with the error alongside it — the same behaviour as with the view off.
2. **Given** manual mode with the boundary view on, **When** the user switches to notation mode and
   back, **Then** the boundary view is still on and the text is unchanged.
3. **Given** manual mode with the boundary view on, **When** the user reloads the page, **Then** the
   mode, the text, and the boundary view setting are all restored.
4. **Given** app state saved before this feature existed, **When** it is loaded, **Then** it loads
   as it did before with the boundary view off.
5. **Given** notation mode, **When** the user looks at the controls, **Then** no boundary view
   control is present — the setting belongs to manual mode only.

---

### User Story 3 - Snapshots look like what is on screen (Priority: P3)

Someone with several manual-mode snapshots turns the boundary view on. The snapshot previews are
drawn the same way as the main picture, so the catalog reads as one consistent set of pictures
rather than a mix of two renderings.

**Why this priority**: Purely a consistency polish on an existing surface; the feature is complete
and useful without it.

**Independent Test**: Take two manual-mode snapshots, toggle the boundary view, and confirm every
preview in the catalog is drawn in the currently selected view.

**Acceptance Scenarios**:

1. **Given** manual mode with snapshots and the boundary view on, **When** the catalog is shown,
   **Then** every snapshot preview is drawn with cell boundaries.
2. **Given** manual mode with snapshots and the boundary view off, **When** the catalog is shown,
   **Then** every snapshot preview is drawn without cell boundaries.
3. **Given** a snapshot taken while the boundary view was on, **When** it is restored, **Then** the
   text it restores is exactly the text it was taken from, unaffected by which view was selected
   when it was taken or restored.

---

### Edge Cases

- **Empty diagram**: nothing is drawn in either view; the boundary view does not draw an empty grid
  for text that describes no diagram.
- **Invalid text with a prior picture**: the last valid picture is kept and marked stale, in
  whichever view is selected. Toggling the view while the picture is stale re-draws that same stale
  picture in the other view; it never revives it as current and never clears the error.
- **Invalid text with no prior picture**: the error is shown alone. The boundary view has nothing to
  draw and draws nothing — it does not show an empty grid.
- **Single-character text**: a picture of one cell shows one box.
- **Trailing blank lines**: rows of empty cells are rows of boxes like any other, so the grid height
  matches the number of described rows, not the number of non-empty ones.
- **Rightmost column and bottom row**: their outer edge is open, since boundary lines are drawn
  between cells rather than around the picture. The cells are still separated from one another and
  countable; this is a deliberate scope decision, not a defect.
- **Wide diagrams**: a bordered picture is wider than the same picture unbordered, so it may need
  scrolling or scaling where the plain view did not. It is displayed the same way any other picture
  too wide for the display area already is.
- **Toggling with unsaved edits in flight**: toggling the view never alters, reformats, or
  re-normalises the text in the box.

### Worked Example

The trefoil, written in the compact format from spec 001 as four lines of seven characters:

```
_(---)_
_./-/,_
(-A\A-)
.--a--,
```

renders in the default view as a seamless picture, and in the boundary view as the same picture with
each of the twenty-eight characters given its own box:

```
+---+---+---+---+---+---+---
|   |   |___|___|___|   |
|   |  /|   |   |   |\  |
|   | ( |   |   |   | ) |
+---+---+---+---+---+---+---
|   |  \|   |___|   |/  |
|   |   |\ /|   |\ /|   |
|   |   | / |   | / |   |
+---+---+---+---+---+---+---
|   |___|/ \|   |/ \|___|
|  /|   |   |\ /|   |   |\
| ( |   |   | \ |   |   | )
+---+---+---+---+---+---+---
|  \|___|___|/ \|___|___|/
```

Four rows of boxes for four lines of text; seven boxes across for seven characters per line. The
`(` in the third line of text is the box in the third row, first column — the one drawn `( ` with an
arc opening upward out of it.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Manual diagram mode MUST offer a control that turns the cell boundary view on and off.
- **FR-002**: The boundary view MUST be off by default, so the picture shown to a user who never
  touches the control is the seamless rendering manual mode shows today.
- **FR-003**: With the boundary view on, the displayed picture MUST include boundary lines that
  separate every cell from its neighbours, such that one delimited box corresponds to exactly one
  character of the entered text, and one row of boxes to exactly one line of it. The outermost right
  and bottom edges of the picture need not be closed (see Assumptions).
- **FR-004**: Turning the boundary view on or off MUST NOT change the entered text, the diagram it
  describes, or the validity of either. It is a display choice only.
- **FR-005**: With the boundary view on, the picture MUST update on every text edit with no further
  action, exactly as it does with the view off.
- **FR-006**: With the boundary view on, invalid text MUST behave exactly as it does with the view
  off: the last valid picture stays on screen in the boundary view, marked stale, with the error
  alongside it; and where there is no prior picture, the error is shown alone with no grid drawn.
- **FR-007**: Toggling the boundary view while the displayed picture is stale MUST redraw that same
  stale picture in the newly selected view, leaving its stale marking and the error message in place.
- **FR-008**: Text that describes no diagram MUST draw nothing in the boundary view, not an empty
  grid.
- **FR-009**: Ragged rows MUST appear in the boundary view with their inferred trailing empty cells
  drawn as boxes, so the grid is rectangular even while the text is not.
- **FR-010**: Manual-mode snapshot previews MUST be drawn in the currently selected view, so the
  catalog and the main picture always agree.
- **FR-011**: A snapshot MUST continue to record only the diagram text it was taken from. The
  selected view MUST NOT be recorded in a snapshot, and restoring a snapshot MUST NOT change the
  selected view.
- **FR-012**: The app MUST remember the boundary view setting across a page reload, alongside the
  mode, the manual diagram text, and the manual-mode snapshots it already remembers.
- **FR-013**: The app MUST keep the boundary view setting while the user is in notation mode and
  restore it unchanged on return to manual mode.
- **FR-014**: The app MUST continue to load state saved before this feature existed, treating it as
  having the boundary view off.
- **FR-015**: The boundary view control MUST be present only in manual diagram mode. Notation mode
  MUST be unchanged by this feature.
- **FR-016**: The boundary view MUST NOT reintroduce any control that manual mode excludes. The
  drawn (SVG) display, the compact display toggle, and the SVG download MUST remain unavailable in
  manual mode in both views.
- **FR-017**: The control MUST be labelled so that its effect is clear without trying it, and MUST
  show which of the two views is currently selected.

### Key Entities

- **Boundary view setting**: a two-state display preference belonging to manual diagram mode,
  remembered across reloads, recorded in no snapshot, and affecting nothing but how a picture is
  drawn.
- **Cell box**: one enclosed region of the boundary-view picture, corresponding one-to-one with one
  character of the entered text.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: In the boundary view, the number of cell boxes across equals the number of characters
  in the longest line of text, and the number of rows of boxes equals the number of lines described
  — a one-to-one correspondence with no counting of characters or lines required.
- **SC-002**: A user can locate the character responsible for any given part of the picture in a
  single step — turn the view on and read off the box position — without consulting the character
  reference, the source, or any external tool.
- **SC-003**: Switching between the two views takes exactly one action and leaves the entered text
  byte-for-byte identical, in 100% of cases.
- **SC-004**: The picture keeps pace with typing at normal speed in the boundary view on diagrams up
  to the size the app already displays, matching the responsiveness of the plain view.
- **SC-005**: The boundary view setting survives a page reload and a round trip through notation
  mode in 100% of cases.
- **SC-006**: A user who never turns the view on sees manual mode behave exactly as it did before
  this feature, in 100% of cases.

## Assumptions

- The bordered rendering already exists in the core library and is exercised today only by the
  command-line example. This feature exposes that existing rendering through the app rather than
  inventing a second one, so the two surfaces cannot drift apart. No new library capability is
  assumed to be needed, and per the minimal-dependency principle any such need would have to be
  justified at planning time.
- The existing bordered rendering draws each cell's top and left edges, which leaves the rightmost
  column and the bottom row of the picture without an outer closing edge. That is accepted as-is:
  every cell is still delimited and countable, and changing it would alter the command-line output
  and existing snapshots for a purely cosmetic gain. Closing the outer edge is explicitly out of
  scope.
- The setting is a view preference, not a property of a diagram. It is therefore stored once for
  manual mode rather than per snapshot, and it never travels with a snapshot or with the text.
- Notation mode is deliberately left alone. It already offers ASCII, drawn, and compact displays,
  and its user is reading a knot rather than typing cells, so a boundary view there would add a
  fourth display choice for no corresponding need. Extending it later is not precluded.
- The boundary view is an addition to manual mode's ASCII display, not a relaxation of the
  constraint from spec 001 that manual mode shows ASCII only. Both views are ASCII; neither is a
  drawn image or a compact rendering.
- Snapshot previews follow the current view rather than the view in effect when each snapshot was
  taken, because a catalog drawn two different ways would be harder to read than either way alone,
  and because a snapshot stores text rather than a picture.
- The app's existing saved state is extended with one more remembered setting rather than replaced,
  and state saved before this feature still loads, with the view off.
