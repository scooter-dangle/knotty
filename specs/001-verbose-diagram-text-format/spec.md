# Feature Specification: Verbose Diagram Text Format

**Feature Branch**: `claude/verbose-diagram-serialization-wwphbz`

**Created**: 2026-08-30

**Status**: Draft

**Input**: User description: "A succinct, direct serialization of the `VerboseDiagram` type so that it's fast and simple for a human to specify the rendered diagram independent of a knot diagram encoding. E.g., for specifying what the expected rendering of a diagram should be without having to painstakingly position all the ASCII art. Each line of the format represents a `VerboseLine` with each byte representing a `Horiz` variant (except for the newline, which indicates the end of the `VerboseLine`)."

## Overview

Today a rendered diagram can only be produced by writing abbreviated knot notation and letting the
library derive the rendering. There is no way to say "this is the picture I expect" directly. Anyone
writing a test, filing a rendering bug, or exploring what a layout looks like has to either work
backwards from notation or hand-position multi-line ASCII art in which a single misplaced space is
invisible and wrong.

This feature introduces a compact, one-character-per-cell text format for the rendered diagram
itself. One text line describes one row of the diagram; each character in that line names one cell.
The format is independent of knot notation: it describes a picture, not a knot.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Specify an expected rendering directly (Priority: P1)

A contributor writing or fixing a rendering test wants to state exactly what picture the library
should produce. Instead of pasting eleven lines of ASCII art whose alignment they must count by
hand, they write four short lines of one character per cell and get back the rendered diagram.

**Why this priority**: This is the core value of the feature and everything else builds on it. On
its own it already replaces the most error-prone part of authoring rendering tests and bug reports.

**Independent Test**: Write the compact text for a known diagram (e.g. the trefoil), render it, and
confirm the resulting picture matches the picture the equivalent knot notation produces. Delivers
value with no GUI changes at all.

**Acceptance Scenarios**:

1. **Given** the compact text for a trefoil rendering, **When** it is parsed and rendered, **Then**
   the resulting picture is byte-for-byte identical to the picture produced from the abbreviated
   notation `(0 (2 /1 \0 /1 )2 )0`.
2. **Given** a text block whose rows have different lengths, **When** it is parsed, **Then** the
   shorter rows are treated as if padded on the right with empty cells and the diagram renders
   without error.
3. **Given** a text block whose first line describes an outer arc, **When** it is rendered, **Then**
   that arc appears across the top of the picture, not the bottom.
4. **Given** a text block containing a character that is not in the symbol table, **When** it is
   parsed, **Then** parsing fails with a message naming the offending character and its row and
   column.
5. **Given** empty text, **When** it is parsed, **Then** the result is an empty diagram rather than
   an error.

---

### User Story 2 - Read an existing rendering back out as compact text (Priority: P2)

A contributor looking at a diagram produced from knot notation wants its compact form — to paste
into a test, to compare two renderings at a glance, or to use as the starting point for a
hand-edited variation.

**Why this priority**: Turns the format into a two-way tool. Authoring (P1) is usable without it,
but writing the first version of a complex expected rendering by hand is much easier when you can
start from a real one and edit it.

**Independent Test**: Take any diagram the library can render, emit its compact text, parse that
text back, and confirm the two renderings are identical.

**Acceptance Scenarios**:

1. **Given** any diagram the library can render, **When** its compact text is produced and parsed
   back, **Then** the round trip reproduces the identical diagram.
2. **Given** a rendering, **When** its compact text is produced, **Then** every row is the same
   number of characters and each character is drawn from the documented symbol table.

---

### User Story 3 - Manual diagram mode in the example app (Priority: P3)

Someone exploring the example app wants to sketch a rendering directly rather than derive it from
knot notation. They switch the app into manual diagram mode, type the compact text into a text box,
and watch the picture update as they type.

**Why this priority**: This is the most visible and most fun surface, but it depends entirely on
P1. The format is already useful to contributors without any app change.

**Independent Test**: Open the app, switch to manual mode, type compact text, and confirm the
picture appears and updates on every keystroke without pressing a button.

**Acceptance Scenarios**:

1. **Given** the app in its normal notation mode, **When** the user switches to manual diagram mode,
   **Then** a text box for the compact format is shown and the notation-driven inputs are no longer
   in play.
2. **Given** the app in manual diagram mode, **When** the user types or edits a character, **Then**
   the displayed diagram updates to match the new text without any further action.
3. **Given** the app in manual diagram mode, **When** the text is not valid, **Then** an error
   message describing the problem is shown in place of a diagram, and correcting the text restores
   the diagram.
4. **Given** the app in manual diagram mode with text entered, **When** the user reloads the page,
   **Then** the mode and the entered text are restored.
5. **Given** the app in manual diagram mode, **When** the user switches back to notation mode,
   **Then** the previously entered notation and moves are still there, unchanged.

---

### Edge Cases

- **Unrecognized character**: reported as an error identifying the row, the column, and the
  character — never silently ignored or treated as empty.
- **Ragged rows**: rows shorter than the widest row are padded on the right with empty cells.
- **Empty input**: yields an empty diagram, which renders as nothing.
- **Blank line inside the text**: describes a row of entirely empty cells, not the end of the input.
- **Trailing newline**: optional; text with and without one describes the same diagram.
- **Carriage returns**: text saved with Windows line endings describes the same diagram as the same
  text with Unix line endings.
- **Pictorially inconsistent input**: text that is well-formed but describes a picture no knot could
  produce (an opening with no matching close, a crossing with nothing crossing it) still renders —
  the format describes a picture, and judging whether the picture is a valid knot is out of scope.
- **Non-ASCII characters**: reported as an error the same way any other unrecognized character is.
- **Very large input**: a diagram larger than the display area renders and can be scrolled or scaled
  the same way any other large diagram in the app is.

### Worked Example

The trefoil, written in abbreviated notation as `(0 (2 /1 \0 /1 )2 )0`, renders as this picture:

```
      _________
     /         \
    (           )
     \   ___   /
      \ /   \ /
       /     /
   ___/ \   / \___
  /      \ /      \
 (        \        )
  \______/ \______/
```

The same picture, written directly in this format, is four lines of seven characters:

```
_(---)_
_./-/,_
(-A\A-)
.--a--,
```

Reading the first line: an empty cell, an opening arc, three horizontal lines, a closing arc, an
empty cell — the outer loop across the top of the picture. Nothing about the knot itself had to be
worked out to write it, and nothing had to be aligned by counting spaces.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The system MUST define a text format in which one line of text describes one row of a
  rendered diagram and one character describes one cell of that row.
- **FR-002**: The system MUST use exactly this character-to-cell mapping, with no aliases and no
  additional accepted characters:

  | Char | Cell               | Char | Cell                 |
  |------|--------------------|------|----------------------|
  | `_`  | Empty              | `,`  | ClosedAbove          |
  | `-`  | Line               | `j`  | TransferUpStart      |
  | `\`  | CrossDownOver      | `i`  | TransferUp           |
  | `/`  | CrossDownUnder     | `r`  | TransferUpFinish     |
  | `A`  | CrossUpOver        | `2`  | TransferDownStart    |
  | `a`  | CrossUpUnder       | `k`  | TransferDown         |
  | `(`  | OpenedBelow        | `L`  | TransferDownFinish   |
  | `.`  | OpenedAbove        |      |                      |
  | `)`  | ClosedBelow        |      |                      |

- **FR-003**: The mapping MUST be case-sensitive — `A` and `a` denote different cells, as do `L` and
  `l` (the latter being unrecognized).
- **FR-004**: A newline MUST end a row. The first line of text MUST describe the topmost row of the
  rendered picture and each subsequent line the row below it, so that the text block reads in the
  same visual order as the picture it describes.
- **FR-005**: The system MUST accept text in this format and produce the corresponding rendered
  diagram, using the existing rendering, with no knot notation involved.
- **FR-006**: The system MUST produce this text format from any diagram it can render, emitting rows
  of equal length.
- **FR-007**: Text produced by the system, when read back, MUST yield an identical diagram; and a
  diagram read from text, when written back out, MUST yield text describing the identical diagram.
- **FR-008**: The system MUST reject unrecognized characters with an error message that identifies
  the character and its row and column position.
- **FR-009**: The system MUST treat rows shorter than the widest row as if padded on the right with
  empty cells.
- **FR-010**: The system MUST treat empty input as an empty diagram rather than an error.
- **FR-011**: The system MUST accept a trailing newline and both Unix and Windows line endings
  without changing the described diagram.
- **FR-012**: The system MUST NOT require the described picture to correspond to a valid knot; it
  renders whatever the text describes.
- **FR-013**: The example app MUST offer a manual diagram mode, reachable and reversible from its
  existing controls, alongside its existing notation-driven mode.
- **FR-014**: In manual diagram mode the app MUST present a text box for this format and update the
  displayed diagram on every edit, with no button press or other confirmation step.
- **FR-015**: In manual diagram mode, when the text is invalid, the app MUST show the error message
  in place of the diagram, and MUST restore the diagram once the text becomes valid again.
- **FR-016**: The app MUST retain the notation, moves, and other existing state while in manual mode
  and restore it unchanged when the user switches back.
- **FR-017**: The app MUST remember the selected mode and the manual text across a page reload,
  alongside the state it already remembers.
- **FR-018**: The app MUST support both its existing picture styles (ASCII and drawn) in manual mode.
- **FR-019**: The documented symbol table MUST be discoverable from the manual mode surface, so a
  user can look up a character without leaving the app.

### Key Entities

- **Diagram text**: a block of lines describing one rendered diagram; the unit a user writes, pastes,
  or stores.
- **Row line**: one line of that block, describing one horizontal row of the picture.
- **Cell character**: one character of a row line, naming one of the sixteen renderable cell kinds.
- **Symbol table**: the fixed, one-to-one correspondence between cell characters and cell kinds; the
  single source of truth shared by reading, writing, and the in-app reference.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: A rendering that takes 10 lines of hand-aligned ASCII art to write out can be specified
  in 4 lines of 7 characters, a reduction of over 80% in characters typed.
- **SC-002**: A contributor can specify an expected rendering and see it drawn without writing any
  knot notation, in a single step.
- **SC-003**: Every diagram the library can render survives a write-then-read round trip unchanged —
  100% of cases, verified across the existing set of example diagrams.
- **SC-004**: Every invalid input is reported with the exact row and column of the first offending
  character, so the author can fix it without hunting.
- **SC-005**: In the example app, an edit to the manual text is reflected in the picture with no
  intermediate action by the user, and with no perceptible lag on diagrams of the size the app
  already displays.
- **SC-006**: Switching into manual mode and back leaves the user's existing notation and moves
  exactly as they were, in 100% of cases.
- **SC-007**: A user unfamiliar with the format can determine what any character means from within
  the app, without consulting source code.

## Assumptions

- The format describes the *rendered picture*, not a knot. Abbreviated notation remains the source
  of truth for knots; this format is a way to state a rendering directly, as the constitution's
  notation-fidelity principle requires.
- Reading and writing this format both belong in the core library, so tests and any other consumer
  can use them; the example app is only a consumer, per the library-first principle.
- Rows are written in visual order — first line is the top of the picture — so that the text reads
  the way the diagram looks. This is the opposite of the order the rendering structure stores rows
  in, so reading and writing both reverse; that reversal is an implementation concern, invisible to
  anyone writing the text.
- The format is one byte per cell over the characters listed; multi-byte characters and multi-
  character cell names are out of scope.
- Rows shorter than the widest row are padded rather than rejected, because trailing empty cells are
  exactly the tedium the format exists to remove.
- Manual mode is for *viewing* a specified rendering. Diagram moves, the rotate action, and the move
  pickers all operate on knot notation and are therefore not available while in manual mode.
- Snapshots taken in manual mode, if offered, store the manual text rather than knot notation; this
  is a detail for planning, not a requirement here.
- The app's existing local-storage state is extended rather than replaced, and existing saved state
  from before this feature still loads.
- No new core-crate dependency is needed; per the minimal-dependency principle any such need would
  have to be justified at planning time.
