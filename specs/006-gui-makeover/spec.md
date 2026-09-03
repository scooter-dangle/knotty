# Feature Specification: GUI Makeover

**Feature Branch**: `claude/speckit-gui-makeover-vj0169`

**Created**: 2026-09-03

**Status**: Draft

**Input**: User description: "Give the GUI app a basic makeover. Start by grabbing screenshots of its current, rather dismal appearance in different app states (e.g., empty manual diagram, many snapshots, large knot diagram). Screenshots should include mobile browser sizing, not just desktop. At a minimum: 1. buttons that switch between two exclusive states should be changed to toggles 2. spacing of ASCII diagrams should be tweaked to minimize discontinuity between characters 3. layout shouldn't shift so jarringly upon error or empty states. Out of scope: 1. changes to the overall state machine of the GUI — changes should only be cosmetic 2. svgbob as diagram renderer. keep it for now."

## Overview

The browser app is, by its own README's description, "ugly" and "snowflakey". It has grown a
feature at a time — a second mode, a bordered view, snapshots, move pickers — and each was bolted
onto a page that has never had a layout: default browser buttons in one long row, a default serif
face, inline text boxes that land wherever the flow leaves them, and a diagram that appears and
disappears and takes the rest of the page with it.

This feature is a **cosmetic makeover**. It changes how the app looks and how its controls read; it
does not change what the app does. Every mode, setting, action, message and saved-state shape the
app has today survives unchanged, and a user who knew the old page finds every capability in the
new one. The user set three minimum outcomes and the baseline survey added a fourth that the user's
own request for phone-sized screenshots implies:

1. **Two-state controls become toggles.** Four controls flip between exactly two exclusive states —
   notation mode / manual mode, picture / characters, full / compact, plain / bordered — and every
   one of them is today a button labelled with the *other* state, so nothing on screen says which
   state is current. Each becomes a control that shows both states and marks the active one.
2. **Character-drawn diagrams stop looking dashed.** The pictures the app draws out of `/`, `\`, `_`,
   `(`, `)` and `|` are set in a font and spacing where the strokes do not reach their cell edges,
   so a strand that runs across several rows is drawn as separated fragments. The spacing is tuned
   so adjacent characters meet.
3. **The layout stops jumping.** Today an error or an empty diagram collapses the picture area and
   everything below it leaps up by 170–200 px; in manual mode the empty state even puts the text box
   on the toolbar row. The picture area and the message area keep their place, so the inputs a user
   is typing into stay under their fingers.
4. **The page works at phone size.** The app is used from phones — the code already carries a
   workaround for Enter on Android Chrome — but it declares nothing about viewports, so a phone
   renders a 980 px desktop layout at 40% scale. The page lays out to the width it is given.

The evidence for all of this is in [baseline.md](./baseline.md): thirty full-page screenshots of
the app in fifteen states at a desktop and a phone viewport, two zoomed crops of the character
drawing, and the measured layout-shift figures. The same captures, re-taken after the makeover, are
how the outcome is judged.

### What "cosmetic" means here

The user drew the boundary: no changes to the app's state machine, and the current picture renderer
stays. Concretely, the set of modes, settings and actions is fixed; every transition between them
is fixed; what is persisted, and its shape, is fixed; and the pictures themselves — the character
grid the library produces and the picture derived from it — are drawn from the same source with the
same renderer. What may change is everything about how those things are presented: arrangement,
size, spacing, type, colour, control shape, labels, and what occupies a region of the page when the
thing that usually occupies it is absent.

One consequence deserves stating up front. In the notation mode with the picture display selected,
a notation error today shows *nothing* — the message exists but is only ever rendered in the
characters display. Showing that message in the picture display too is a presentation change, not a
behavioural one, and it is required (FR-014), because a stable message area with nothing in it
would be worse than the current collapse.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - See which state each two-way setting is in (Priority: P1)

A user opens the app and wants to know, without clicking anything, whether they are in notation
mode or manual mode, whether the diagram is shown as a picture or as characters, whether the
compact or full drawing is on, and (in manual mode) whether cell borders are on. Each of these
settings is presented as a single control that names both of its states and visibly marks the one
that is active; activating the control flips it to the other state and the marking follows.

**Why this priority**: It is the first of the user's three minimum outcomes, and it is the one a
user meets on every visit. A label that names the state you are *not* in is a small puzzle on every
glance; four of them on one toolbar is the app's most persistent irritation.

**Independent Test**: Load the app with saved settings in each of the sixteen combinations and
confirm, from a screenshot alone, which state each of the four settings is in. Then flip each one
and confirm the marking moved and the diagram or mode changed exactly as it does today.

**Acceptance Scenarios**:

1. **Given** the app in notation mode with the picture display and the full drawing selected,
   **When** the user looks at the toolbar, **Then** the mode control shows "notation" as active and
   "manual" as available, the display control shows the picture as active and characters as
   available, and the density control shows full as active and compact as available.
2. **Given** the display control shows the picture as active, **When** the user activates the
   characters option, **Then** the diagram is redrawn as characters, the control now marks
   characters as active, and reloading the page keeps that choice — exactly as the old button did.
3. **Given** manual mode, **When** the user looks at the toolbar, **Then** the plain/bordered
   control is present and marks the active view, and the display and density controls that apply
   only to notation mode are not shown.
4. **Given** any of the four controls, **When** a user reaches it with the keyboard, **Then** it can
   be focused and flipped without a pointer, and assistive technology announces it as a two-state
   control with its current state.

---

### User Story 2 - Read a character-drawn diagram as continuous strands (Priority: P1)

A user viewing the diagram as characters — the characters display in notation mode, every picture
in manual mode, and every manual snapshot preview — sees each strand as a continuous line: a
diagonal that runs across rows meets itself at every row boundary, a horizontal run meets the
diagonal that continues it, and an opening or closing parenthesis meets the strands above and
below it. In the bordered view the cell grid reads as ruled lines, not dashes.

**Why this priority**: It is the user's second minimum outcome and it is what manual mode is *for*:
the whole point of the mode is to look at the picture the characters make. Today the picture is
visibly broken at every row (see the zoomed baseline crops), which makes it harder to judge whether
the diagram is right.

**Independent Test**: Draw the trefoil as characters, zoom in, and compare against the baseline
zoom crop: the diagonal `\` `/` runs, the `_` runs and the `(` `)` joins that are broken in the
baseline are continuous or as near continuous as the chosen typeface permits.

**Acceptance Scenarios**:

1. **Given** the trefoil in the characters display, **When** the user inspects a diagonal strand
   that spans three rows, **Then** there is no visible gap between the stroke in one row and the
   stroke in the next.
2. **Given** a horizontal run of `_` cells with a `/` continuing it in the row above, **When** the
   user inspects the join, **Then** the horizontal stroke and the diagonal stroke meet.
3. **Given** the bordered view in manual mode, **When** the user inspects a cell boundary, **Then**
   the vertical `|` strokes form a continuous rule down the column and the `+` corners meet the
   `-` strokes on either side.
4. **Given** the same diagram in the characters display and in the picture display, **When** the
   user compares them, **Then** each cell of the character drawing still corresponds to one
   character — the spacing change does not alter the one-character-one-cell correspondence, and the
   column count and row count of the drawing are unchanged.

---

### User Story 3 - Keep typing when the diagram goes empty or bad (Priority: P1)

A user is editing notation, moves or a manual diagram. Mid-edit the text is briefly invalid, or
briefly empty, and then valid again. Throughout, the text box they are typing into stays where it
is, the region where the diagram is drawn stays the same size and place, and the error message
appears in a place reserved for it — without pushing anything else around. Nothing about *when*
an error is shown or *what* it says changes.

**Why this priority**: It is the user's third minimum outcome. Layout shift under the cursor is the
most disruptive thing the current page does: the baseline measures a 172 px jump in notation mode
and a 203 px jump in manual mode, on every transition between valid and invalid text.

**Independent Test**: Record the position of every input and control, then walk the text through
valid → invalid → empty → valid in both modes and confirm no input or control moved.

**Acceptance Scenarios**:

1. **Given** a valid trefoil in notation mode, **When** the user makes the notation unparseable,
   **Then** the notation box, the moves box, the move pickers and the rotate control stay at the
   same position, the diagram region keeps its size, and the error message is shown in the reserved
   message area — in the picture display as well as the characters display.
2. **Given** the notation box is emptied, **When** the user looks at the page, **Then** the
   diagram region is still present at its reserved size, empty, and the controls have not moved.
3. **Given** manual mode with nothing typed, **When** the user looks at the page, **Then** the text
   box sits in its normal place below the (empty) diagram region, not beside the toolbar.
4. **Given** a valid manual diagram, **When** the user types an unrecognized character, **Then**
   the last good picture stays showing, dimmed, exactly as today, the error appears in the reserved
   message area, and the text box does not move; **When** the user deletes the character, **Then**
   the message area clears and again nothing moves.
5. **Given** valid notation and an unparseable moves line, **When** the user looks at the move
   pickers and the rotate control, **Then** they are shown as unavailable in a way that keeps their
   placeholder text legible, and they occupy the same space as when available.
6. **Given** the saved-state recovery notice is showing, **When** the user dismisses it, **Then**
   the notice's space is released without the toolbar or inputs changing position by more than the
   notice's own height — a one-time settle on dismissal, not a shift during editing.

---

### User Story 4 - Use the app on a phone (Priority: P2)

A user opens the app on a phone. The page lays out to the phone's width: the toolbar wraps into
readable, tappable controls; text boxes span the width; a diagram wider than the screen scrolls
within its own region rather than widening the whole page; and the snapshot catalog stacks
sensibly. Nothing requires pinch-zooming to read or to tap.

**Why this priority**: The user asked for phone-sized screenshots, which only matter if the phone
layout is expected to be good. It ranks below the three minimum outcomes because it is implied
rather than stated.

**Independent Test**: Re-take the `mobile-*` baseline captures and confirm the page width equals
the viewport width in every state, every control is at least a comfortable tap target, and no
state needs horizontal scrolling of the page itself.

**Acceptance Scenarios**:

1. **Given** a phone-width viewport, **When** the app loads in any of the fifteen baseline states,
   **Then** the page is no wider than the viewport and the layout is not a scaled-down desktop page.
2. **Given** the wide knot from the baseline at phone width, **When** it is drawn as a picture or as
   characters, **Then** the drawing scrolls horizontally inside its own region and the toolbar and
   inputs stay fully on screen.
3. **Given** the toolbar at phone width, **When** the user looks at it, **Then** every control is
   readable and tappable without zooming, and related controls remain grouped when the row wraps.

---

### User Story 5 - Scan the snapshot catalog (Priority: P3)

A user who has saved several snapshots sees them as a compact catalog of like-sized cards, each
with its preview and its restore/delete actions, arranged to use the available width, so that nine
snapshots fit in far less than the 2,500 px they take today. Snapshot behaviour — what is saved,
the nine-snapshot limit, restore, delete — is unchanged.

**Why this priority**: Purely tidiness; it is the largest visual waste on the page but does not
impede any task.

**Independent Test**: Save nine snapshots in each mode and compare page height and card layout
against the baseline `*-many-snapshots` captures.

**Acceptance Scenarios**:

1. **Given** nine notation snapshots at desktop width, **When** the user views the catalog,
   **Then** the cards are uniform in size, arranged in more than one column, and the page is
   markedly shorter than the baseline.
2. **Given** a snapshot card, **When** the user looks at it, **Then** its preview, its encoding
   (notation mode) and its restore and delete actions are all present and legible.
3. **Given** a manual snapshot whose saved text no longer parses, **When** the catalog is shown,
   **Then** the "unreadable snapshot" notice is shown in the card in place of a preview, as today.

---

### Edge Cases

- **A diagram taller than the reserved region.** The diagram region has a minimum size, not a
  maximum: a tall or wide diagram grows the region (and scrolls within it horizontally), it is never
  clipped, and growing is not "shift" in the sense of FR-010 because it happens on a change of
  diagram, not on a change of validity.
- **Error message longer than one line.** The message area must be able to hold the longest message
  the app produces today without overlapping neighbours; it may grow, but only downwards, and only
  the message area's own contents change.
- **Both a storage-recovery notice and a diagram error.** Both are shown; the notice stays above the
  toolbar as today, the diagram error in its area.
- **Toggle activated while its action is a no-op.** Flipping a control to the state it is already in
  is not possible with a two-state control; the app's existing "no change, no re-render" behaviour
  is unaffected.
- **Saved state from before the makeover.** Every persisted field keeps its name and meaning, so a
  state saved by the current app loads into the new one and produces the same settings.
- **Very narrow viewports (below 360 px).** The layout should degrade by wrapping and scrolling
  regions, never by widening the page.
- **Character drawing in the browser's fallback font.** The spacing tuning should not depend on a
  single typeface being installed; where the preferred face is unavailable the drawing must still be
  monospaced and no worse than the baseline.

## Requirements *(mandatory)*

### Functional Requirements

**Scope guard**

- **FR-001**: The makeover MUST NOT add, remove or rename any mode, setting, action, message or
  persisted field, and MUST NOT change any transition between them. The user-visible capability set
  and the saved-state format after the makeover are identical to before.
- **FR-002**: The picture display MUST continue to derive its picture from the character drawing
  with the renderer in use today; the makeover may change how the picture is sized and placed, not
  how it is produced.
- **FR-003**: The character drawing MUST keep its one-character-one-cell correspondence: the
  makeover changes the spacing and typeface of cells, never the characters, rows or columns.

**Two-state controls**

- **FR-004**: Each of the four two-state settings — notation/manual mode, picture/characters
  display, full/compact drawing, plain/bordered view — MUST be presented as a single control that
  names both states and visibly distinguishes the active state from the inactive one.
- **FR-005**: Activating a two-state control MUST switch to its other state, with the same effect
  and the same persistence as the button it replaces.
- **FR-006**: Two-state controls MUST be operable by keyboard and MUST expose their current state to
  assistive technology.
- **FR-007**: The plain/bordered control MUST appear only in manual mode, and the display and
  density controls only in notation mode, matching where the buttons they replace appear today.

**Character spacing**

- **FR-008**: In every place the app draws a diagram as characters (the characters display, the
  manual-mode picture, manual snapshot previews), the row spacing and typeface MUST be chosen so
  that `/`, `\` and `|` strokes in vertically adjacent cells meet, `_` strokes meet the strokes
  that continue them in the adjacent row, and `(` and `)` meet the strokes above and below them,
  to the extent the typeface allows.
- **FR-009**: The bordered view's `+`, `-` and `|` MUST read as continuous ruled lines under the
  same spacing.

**Layout stability**

- **FR-010**: Transitions of the diagram text between valid, empty and unparseable MUST NOT move any
  input, control or toolbar element. The diagram region MUST keep at least a fixed minimum size in
  all three states.
- **FR-011**: Error messages for the diagram or the moves MUST be shown in a message area whose
  position is fixed relative to the diagram region, and the area MUST exist (occupying its space)
  whether or not a message is showing.
- **FR-012**: In manual mode the text box MUST occupy the same place below the diagram region
  whether the diagram is empty, valid or invalid.
- **FR-013**: The retained, dimmed last-good picture on a manual-mode error MUST be kept.
- **FR-014**: A notation error MUST be shown in the picture display as well as the characters
  display, with the same message text.
- **FR-015**: Controls made unavailable (the move pickers and rotate control on a moves error, the
  snapshot control at the limit or on an error) MUST remain the same size and position when
  unavailable, and their labels or placeholders MUST remain legible in the unavailable state.

**Viewport and sizing**

- **FR-016**: The page MUST lay out to the viewport width: at phone widths it MUST NOT render as a
  scaled-down desktop layout, and the page MUST NOT become wider than the viewport in any baseline
  state.
- **FR-017**: A diagram wider than the available width MUST scroll horizontally within its own
  region, leaving the toolbar and inputs in place.
- **FR-018**: Text boxes for notation, moves and the manual diagram MUST span the available width
  up to a sensible maximum, rather than the default twenty columns.
- **FR-019**: Toolbar controls MUST be grouped by purpose (mode; preset knots; display settings;
  actions), and groups MUST wrap as units at narrow widths.

**Snapshot catalog**

- **FR-020**: Snapshot entries MUST be presented as uniformly sized cards arranged across the
  available width, each containing the preview, the encoding where one is shown today, and the
  restore and delete actions.
- **FR-021**: Snapshot behaviour — contents, limit, restore, delete, the "unreadable snapshot"
  notice — MUST be unchanged.

**General presentation**

- **FR-022**: The app MUST use one consistent typeface for interface text and one monospaced
  typeface for notation, moves and character drawings, at sizes readable without zooming on both
  desktop and phone.
- **FR-023**: The "Download SVG" link MUST be presented at the same size as other secondary
  controls, not at 8 px.
- **FR-024**: The saved-state recovery notice MUST be visually distinguished as a notice and keep
  its dismiss action.

### Key Entities

No new data. The entities the app already has — the persisted state, its snapshots, the two modes
and the four settings — are unchanged in name, shape and meaning (FR-001). The one new *concept* is
the **diagram region**: the reserved area of the page in which the diagram is drawn, together with
its adjoining message area, which exists in every state rather than only when there is something
to draw.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: In a screenshot of any of the sixteen setting combinations, a reader who has not used
  the app can state which state each of the four two-state settings is in, without clicking.
- **SC-002**: Walking the notation text through valid → invalid → empty → valid, and the manual
  text through the same, moves no input or control by more than 0 px (baseline: 172 px and 203 px).
- **SC-003**: In a zoomed capture of the trefoil drawn as characters, the number of visible breaks
  along a diagonal strand that spans three rows is 0 (baseline: a break at every row boundary).
- **SC-004**: A notation error is visible as text on screen in both displays (baseline: invisible
  in the picture display).
- **SC-005**: At a 390 px-wide viewport, the page width equals the viewport width in all fifteen
  baseline states (baseline: 980–1,088 px), and every control's tap target is at least 40 px tall.
- **SC-006**: With nine snapshots, page height at desktop width is at most half the baseline
  (baseline: 2,566 px notation, 2,750 px manual).
- **SC-007**: Every action reachable in the baseline is reachable after the makeover, and state
  saved by the baseline app loads into the new one with identical settings — verified by the
  existing app tests continuing to pass unchanged in what they assert about behaviour.
- **SC-008**: The re-taken `capture.js` screenshots, viewed side by side with the baseline, show
  every finding in [baseline.md](./baseline.md) addressed or explicitly deferred.

## Assumptions

- **Toggle form.** "Toggle" is taken to mean any two-state control that shows both states and marks
  the active one; a segmented pair of options (both labels visible, active one highlighted) is the
  expected form for named pairs like notation/manual and picture/characters, and a labelled switch
  is acceptable for on/off properties like compact and bordered. The choice is left to planning as
  long as FR-004 to FR-006 hold.
- **Stability over retention.** FR-010 is met by reserving space, not by holding on to the last
  valid notation-mode picture during an error. Retaining the last picture in notation mode, as
  manual mode already does, would need the app to remember something it does not remember today,
  which is closer to a behavioural change than a cosmetic one; it is not required. Manual mode's
  existing retention stays (FR-013).
- **Visual direction.** A light, neutral, unbranded look with system typefaces is assumed. No dark
  theme, no colour scheme beyond what is needed to distinguish active from inactive, available from
  unavailable, and notices from content.
- **Phone target.** A 390 px-wide viewport with touch is the reference phone size, matching the
  baseline captures; wider phones and tablets fall between it and the desktop capture.
- **Tests.** The app's existing tests assert behaviour, not markup, and are expected to pass
  unchanged; any test that pins a button label may need its label updated, which is not a change
  in what is asserted.
- **Documentation.** The app's README describes the manual-mode character set and a rendering
  toggle from before spec 005 and is already stale; refreshing it is outside this feature, though
  the toolbar labels it names will change and it will drift further.
- **Renderer.** The picture renderer stays; the makeover may scale or scroll its output but does
  not tune it. Line weight and picture geometry are as today.
