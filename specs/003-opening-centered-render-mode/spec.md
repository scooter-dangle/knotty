# Feature Specification: Opening-Centered Rendering Mode

**Feature Branch**: `claude/opening-centered-diagram-mode-d150sm`

**Created**: 2026-08-31

**Status**: Draft

**Input**: User description: "Another diagram rendering mode variation: an 'opening-centered' form where the opening and closing paren characters are centered vertically within the 3 by 3 character tile. It's meant to be swappable (including in the GUI) with the current rendering mode so that it can be easily examined for regressions. It will reduce complexity in some ways by removing cases in the current mode where the features are split across multiple tiles. It will break, though, a shortcut in the current mode where 'filler' tiles can be determined by examining the tile immediately to their left." (followed by a table of cell shapes, reproduced in full under *The cell table* below)

## Overview

A rendered diagram is a grid of cells, each three characters wide and three lines tall, each named
by exactly one character of diagram text (spec 001) and boxed by exactly one rectangle in the
boundary view (spec 002). In the rendering the library draws today, that one-character-to-one-cell
promise is true of the *text* but false of the *picture*: almost every feature of a knot is drawn
across two vertically adjacent cells.

A crossing is half `\` and half `A`. An opening is half `(` and half `.`. A closing is half `)` and
half `,`. A strand climbing to the next level is three cells wide, `j` then `i` then `r`. Eight of
the sixteen cell characters exist only to hold the missing half of a feature that lives one row
away. Reading the picture therefore means holding two rows in mind at once, and a defect in half of
a feature surfaces in a box the eye does not associate with the character that drew it — which is
precisely the question the boundary view was added to answer.

This feature adds a second rendering of the same diagrams, *opening-centered*, in which the opening
and closing parentheses sit on the middle line of their cell and each feature is drawn whole inside
a single cell. A crossing becomes a complete X in one box. An opening carries both of its arms. A
strand climbs one level per cell. The eight partner-half characters are never drawn.

The two renderings are swappable — in the library, in the example program, and in the app — so the
same knot can be drawn both ways and the pictures compared. That comparison is the point: for every
diagram that the current rendering draws without transfer cells, the two modes are expected to
produce the identical picture, so any difference at all is a defect in one of them. Diagrams that do
contain transfers are the one intended exception: a strand that needs three columns to climb a level
today climbs it in one, so those pictures come out narrower and steeper by design rather than by
accident.

Nothing about the abbreviated knot notation, the diagram text format, or the diagram itself changes.
Only how a diagram is drawn changes, and only when the new mode is selected.

### The cell table

The user description gives the shape of every cell in the new rendering. It is reproduced here in
full, as the authority on what the mode draws (`s` in the description marks a space):

```
character  current cell     opening-centered cell    change
---------  ---------------  -----------------------  ---------------------------------
_          "   "            "   "                    no change (empty)
           "   "            "   "
           "   "            "   "

-          "___"            "   "                    underscores move to the bottom line
           "   "            "   "
           "   "            "___"

\          "   "            "\ /"                    whole crossing in one cell
           "\ /"            " \ "
           " \ "            "/ \"

/          "   "            "\ /"                    whole crossing in one cell
           "\ /"            " / "
           " / "            "/ \"

A          "/ \"            (never drawn)            goes away
           "   "
           "   "

a          "/ \"            (never drawn)            goes away
           "   "
           "   "

(          "   "            "  /"                    whole opening in one cell,
           "  /"            " ( "                    parenthesis on the middle line
           " ( "            "  \"

.          "  \"            (never drawn)            goes away
           "   "
           "   "

)          "   "            "\  "                    whole closing in one cell,
           "\  "            " ) "                    parenthesis on the middle line
           " ) "            "/  "

,          "/  "            (never drawn)            goes away
           "   "
           "   "

j          "__/"            (never drawn)            goes away
           "   "
           "   "

i          "  /"            "  /"                    stays the same
           " / "            " / "
           "/  "            "/  "

r          "  _"            (never drawn)            goes away
           " / "
           "/  "

2          "_  "            (never drawn)            goes away
           " \ "
           "  \"

k          "\  "            "\  "                    stays the same
           " \ "            " \ "
           "  \"            "  \"

L          "\__"            (never drawn)            goes away
           "   "
           "   "
```

Eight characters survive — `_`, `-`, `\`, `/`, `(`, `)`, `i`, `k` — and eight go away. Under this
mode the eight that go away are read as the empty cell `_` rather than as distinct values (see
*Clarifications*).

## Clarifications

### Session 2026-08-31

- Q: How far does the synonymy between the eight retired characters and `_` reach — display only, or through parsing and serialization? → A: Normalizing synonym. Under opening-centered rendering the eight are read as `_`, so canonical text writes `_` for them; canonical text is therefore mode-dependent, and the byte-for-byte round trip holds in that mode for the eight surviving characters. The current rendering keeps spec 001's round trip for all sixteen.
- Q: Where in the app is the rendering mode selectable, and is it one setting or one per app mode? → A: Both notation mode and manual diagram mode, sharing a single persisted choice. The compact view stays notation-only and the boundary view manual-only, so the app offers four option combinations; all eight remain reachable from the library and the example program.
- Q: How many columns should a strand transfer occupy in the new mode, given that `i` and `k` now climb a whole level unaided? → A: One column per level climbed. Transfers become steeper and narrower than today, so pictures containing transfers legitimately differ between the two modes; the identical-picture expectation stays scoped to transfer-free diagrams.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Draw a knot with every feature in one cell (Priority: P1)

Someone working on the rendering asks the library for the opening-centered picture of a knot given in
abbreviated notation. They get back a picture of the same knot in which each crossing, each opening,
each closing, and each single level of vertical strand movement is drawn entirely inside one cell.

**Why this priority**: This is the feature. Everything else is a way of reaching it and comparing it
against what exists.

**Independent Test**: Render a handful of known knots — the unknot, the trefoil — both ways and
confirm the opening-centered picture depicts the same knot, with the parentheses on the middle line
of their cells. Delivers value with no app or command-line change at all.

**Acceptance Scenarios**:

1. **Given** a knot in abbreviated notation, **When** it is rendered opening-centered, **Then** a
   picture of that knot is produced in which every opening and closing parenthesis sits on the
   middle line of its cell.
2. **Given** a knot whose current rendering uses no transfer cells (`j`, `i`, `r`, `2`, `k`, `L`),
   **When** it is rendered both ways, **Then** the two pictures are identical.
3. **Given** any knot, **When** it is rendered opening-centered, **Then** none of the eight
   characters that go away contributes any mark to the picture.
4. **Given** the empty diagram, **When** it is rendered opening-centered, **Then** the picture is
   empty, as it is in the current rendering.
5. **Given** any knot, **When** it is rendered opening-centered, **Then** the picture has no leading
   or trailing all-blank lines that the current rendering of the same knot does not also have.
6. **Given** diagram text naming one of the eight characters that go away, **When** it is read and
   written back under opening-centered rendering, **Then** the text is accepted, that cell is drawn
   blank, and the character is written back as `_`.
7. **Given** a knot whose picture contains a strand climbing or descending between levels, **When** it
   is rendered opening-centered, **Then** each level of movement is drawn in a single cell, and the
   picture is correspondingly narrower than the current rendering of the same knot.

---

### User Story 2 - Swap renderings in the app to hunt regressions (Priority: P1)

Someone looking at a diagram in the app switches the rendering mode and the same diagram is redrawn
the other way. They flip back and forth, comparing the two pictures, and any place where the new
rendering disagrees with the old one is immediately visible. The mode they picked is still selected
when they come back to the app tomorrow.

**Why this priority**: The user asked for the two modes to be swappable specifically so the new one
can be examined for regressions, and the app is where a person actually looks at a diagram. Without
this the rendering exists but nobody can compare the two by eye.

**Independent Test**: Open the app, enter a knot, switch the rendering mode, and confirm the picture
is redrawn in the other mode with the notation text untouched; reload and confirm the mode stuck.

**Acceptance Scenarios**:

1. **Given** a diagram on screen in the current rendering, **When** the user switches to
   opening-centered, **Then** the same diagram is redrawn opening-centered with no other action, and
   the notation text and diagram text are unchanged.
2. **Given** a diagram on screen opening-centered, **When** the user switches back, **Then** the
   picture returns to exactly the rendering shown before the switch.
3. **Given** either rendering mode, **When** the user turns on the compact view in notation mode or
   the boundary view (spec 002) in manual mode, **Then** that option applies to whichever rendering
   is selected.
4. **Given** the opening-centered rendering with the boundary view on, **When** the user looks at any
   crossing, opening, or closing, **Then** it is drawn entirely inside one box.
5. **Given** a rendering mode chosen by the user, **When** the page is reloaded, **Then** the app
   comes back with the same mode selected and the same diagram shown.
6. **Given** manual diagram mode (spec 001), **When** the user switches rendering mode, **Then** the
   typed text is drawn under the newly selected mode's cell shapes, and the text itself is unchanged.
7. **Given** a rendering mode selected in one app mode, **When** the user switches between notation
   mode and manual diagram mode, **Then** the same rendering mode is still selected — the two app
   modes share one choice rather than remembering their own.

---

### User Story 3 - Pick the rendering outside the app (Priority: P2)

Someone rendering a diagram from the command line, or capturing a rendering for a test, chooses which
of the two renderings they want, the same way they already choose the compact and boundary views.

**Why this priority**: The library and the app cover the feature; command-line selection is what
makes it convenient to capture, diff, and paste renderings, but the work is usable without it.

**Independent Test**: Run the example program over a sample diagram once in each mode and confirm the
two outputs, plus every combination with the existing compact and boundary options.

**Acceptance Scenarios**:

1. **Given** a diagram file, **When** the example program is run with the opening-centered rendering
   selected, **Then** it prints the opening-centered picture.
2. **Given** a diagram file and no rendering mode selected, **When** the example program is run,
   **Then** it prints the current rendering, byte for byte as it does today.
3. **Given** any combination of rendering mode with the existing compact and boundary options,
   **When** the example program is run, **Then** it prints that combination.

---

### Edge Cases

- **A diagram text containing a character that goes away.** Diagram text (spec 001) may name any of
  the sixteen cells regardless of which rendering is selected. Under opening-centered rendering the
  eight that go away are synonyms of `_` — accepted without error, drawn blank, and written back as
  `_` — so such a text is not stable under a round trip in that mode: it normalizes once, then holds.
- **Text that draws well in one mode and badly in the other.** The two modes place a feature's marks
  in different cells, so a text hand-written for one mode generally does not draw the same picture in
  the other. Switching modes reinterprets the text; it never rewrites it.
- **Empty and near-empty diagrams.** An empty diagram, a diagram of one opening/closing pair, and a
  diagram with blank rows all render in both modes without error.
- **Ragged diagram text.** Trailing cells inferred as empty (spec 001) behave in the new mode exactly
  as they do in the current one.
- **Diagrams containing transfers.** These are the one case where the two renderings are expected to
  disagree, because a level of strand movement costs one column instead of three. The pictures depict
  the same knot; a reviewer comparing them should read the difference as intended, not as a defect.
- **Compaction.** Columns the compact view strips are decided by what is actually drawn, so a column
  that is strippable in one mode may not be in the other; each mode strips its own picture.
- **Stale picture.** In manual mode, while the text contains an unrecognized character the last valid
  picture stays on screen (spec 001). Switching rendering mode at that moment leaves the stale picture
  in place; it is redrawn in the newly selected mode as soon as the text is valid again.
- **Snapshots captured in the other mode.** A saved snapshot records a diagram, not a picture, so it
  is drawn in whichever mode is selected when it is viewed.

### Worked Example

The unknot, `(0 )0`, in the current rendering and opening-centered — identical:

```
    
 /\ 
(  )
 \/ 
```

The trefoil, `(0 (2 /1 \0 /1 )2 )0`, in the current rendering and opening-centered — also identical:

```
    _________    
   /         \   
  (           )  
   \   ___   /   
    \ /   \ /    
     /     /     
  __/ \   / \__  
 /     \ /     \ 
(       \       )
 \_____/ \_____/ 
```

The pictures match; what changes is which cell owns which mark. With cell boundaries drawn (spec
002), the same trefoil in the current rendering — every opening, closing and crossing straddles a
boundary line:

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
|   |   |   |   |   |   |   
|   |   |   |   |   |   |   
```

and opening-centered — every one of them is whole inside a single box:

```
+---+---+---+---+---+---+---
|   |   |   |   |   |   |   
|   |   |   |   |   |   |   
|   |   |___|___|___|   |   
+---+---+---+---+---+---+---
|   |  /|   |   |   |\  |   
|   | ( |   |   |   | ) |   
|   |  \|   |___|   |/  |   
+---+---+---+---+---+---+---
|   |   |\ /|   |\ /|   |   
|   |   | / |   | / |   |   
|   |___|/ \|   |/ \|___|   
+---+---+---+---+---+---+---
|  /|   |   |\ /|   |   |\  
| ( |   |   | \ |   |   | ) 
|  \|___|___|/ \|___|___|/  
```

Note the consequence the user description calls out. In the current rendering, the cell to the right
of any cell is decided by that cell alone — a filler cell is whatever its left-hand neighbour implies.
Opening-centered, the cell above an opening is empty while the cell above and to the right of it
carries a strand, so the filler cell to the right of an empty cell is no longer determined by the
empty cell. Whatever produces the new rendering must decide filler cells some other way.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The library MUST be able to render a diagram in either of two rendering modes — the
  current one and opening-centered — chosen per rendering.
- **FR-002**: Opening-centered rendering MUST draw each cell exactly as *The cell table* above
  specifies, including which of the cell's three lines each mark falls on.
- **FR-003**: In opening-centered rendering, an opening parenthesis and a closing parenthesis MUST
  each be drawn on the middle line of its cell.
- **FR-004**: In opening-centered rendering, a crossing, an opening, a closing, and each single level
  of vertical strand movement MUST each be drawn entirely within one cell; no feature may be split
  across cells.
- **FR-005**: Under opening-centered rendering, the eight characters listed as going away (`A`, `a`,
  `.`, `,`, `j`, `r`, `2`, `L`) MUST be synonyms of the empty cell `_`: accepted without error, drawn
  blank, and written as `_` when diagram text is serialized.
- **FR-006**: Both renderings MUST depict the same knot for the same input — the same crossings with
  the same over/under, joined the same way.
- **FR-007**: For any diagram whose current rendering contains no transfer cells (`j`, `i`, `r`, `2`,
  `k`, `L`), the two renderings MUST produce identical pictures.
- **FR-008**: The current rendering MUST remain the default everywhere and MUST be unchanged,
  byte for byte, by this feature.
- **FR-009**: Selecting a rendering mode MUST NOT change the diagram, the abbreviated notation, or the
  diagram text; it MUST affect only what is drawn.
- **FR-010**: Both existing display options — the compact view and the cell boundary view (spec 002) —
  MUST be available with either rendering mode. In the library and the example program every
  combination of the three MUST be reachable; in the app each keeps the app mode it lives in today,
  so the app offers rendering mode with compact in notation mode and rendering mode with the boundary
  view in manual mode.
- **FR-011**: In the boundary view under opening-centered rendering, each character of diagram text
  MUST still own exactly one box, unchanged from spec 002.
- **FR-012**: Users MUST be able to switch rendering mode in the app, in both notation mode and manual
  diagram mode, and see the picture redrawn immediately with no further action. The two app modes MUST
  share a single rendering-mode choice, not one each.
- **FR-013**: The app MUST persist the selected rendering mode across reloads, as it persists its other
  view settings, as one setting covering both app modes.
- **FR-014**: The example command-line program MUST allow either rendering mode to be selected, by the
  same mechanism it already uses to select its other display options, defaulting to the current
  rendering.
- **FR-015**: Opening-centered rendering MUST NOT introduce leading or trailing all-blank lines that
  the current rendering of the same diagram does not also have.
- **FR-016**: Canonical diagram text MUST be byte-for-byte stable under opening-centered rendering for
  text using only the eight surviving characters (`_`, `-`, `\`, `/`, `(`, `)`, `i`, `k`), and MUST
  reach that stable form in a single pass for text naming a retired character. Under the current
  rendering, spec 001's byte-for-byte round trip MUST continue to hold for all sixteen characters.
- **FR-017**: In opening-centered rendering, a strand moving one level up or down MUST occupy exactly
  one cell and therefore one column; climbing N levels MUST take N columns, with no additional columns
  spent on starting or finishing the movement.

### Key Entities

- **Rendering mode**: which of the two cell vocabularies a picture is drawn with — the current one or
  opening-centered. Two values; the current one is the default. A display choice only: it carries no
  information about the knot.
- **Cell**: one character of diagram text, drawn as three characters across and three lines down in
  both modes. What a given character draws inside its cell is what the rendering mode decides.
- **Feature**: a crossing, an opening, a closing, a horizontal strand, or one level of vertical strand
  movement. In the current rendering a feature may span two cells; opening-centered, never more than
  one.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All eight cell characters that opening-centered rendering keeps draw the exact shape
  given in *The cell table*, confirmed cell by cell.
- **SC-002**: Every existing rendering the project has recorded is unchanged — 100% of them — when
  the current rendering mode is used.
- **SC-003**: For every sample knot in the project whose current rendering uses no transfer cells,
  the two rendering modes produce identical pictures.
- **SC-004**: With cell boundaries drawn, 100% of crossings, openings and closings in an
  opening-centered picture are contained in a single box, against 0% in the current rendering.
- **SC-005**: A person comparing the two renderings of a diagram in the app can switch between them
  in one action and see the result without reloading, re-entering the diagram, or losing any other
  view setting.
- **SC-006**: The rendering mode a person selects is still selected after a page reload.
- **SC-007**: All eight combinations of rendering mode, compact view, and boundary view can be produced
  from the library and from the example program; the app reaches the four its existing layout allows
  (rendering mode with compact in notation mode, rendering mode with the boundary view in manual mode).
- **SC-008**: Diagram text using only the eight surviving characters round-trips byte for byte under
  opening-centered rendering; text naming a retired character reaches its canonical form in one pass
  and is unchanged by every pass after that. All sixteen characters still round-trip byte for byte
  under the current rendering.
- **SC-009**: A strand climbing one level occupies one column in opening-centered rendering against
  three in the current rendering, and both renderings still depict the same knot.

## Assumptions

- The eight characters that go away remain readable in diagram text under either rendering mode. What
  differs is that opening-centered rendering treats them as the empty cell rather than as distinct
  values, so canonical text is mode-dependent (see *Clarifications*).
- Switching rendering mode reinterprets diagram text under the newly selected mode's cell shapes; the
  text is never translated or rewritten. Two texts that draw the same picture in the two modes are in
  general different texts.
- Rendering mode is a display choice only. Diagram manipulation — moves, rotation, bulge detection,
  snapshots — continues to operate on the existing representation and is unaffected by which mode is
  selected.
- The current rendering is kept indefinitely as the default. This feature adds a mode; it does not
  deprecate or replace anything.
- Both modes keep the three-wide, three-line cell, so the compact view and the boundary view need no
  change in what they mean.
- "Opening-centered" is the name shown to the user, and the app's toggle reads as a switch between the
  two renderings rather than as a property of either app mode.
- The example program selects the mode the way it selects its existing display options.
- Per the project constitution, the mode lands in the core library first; the app and the example
  program are surfaces over it.
