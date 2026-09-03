# Feature Specification: Retire the Split-Cell Rendering

**Feature Branch**: `claude/speckit-rendering-mode-migration-cbdxrp`

**Created**: 2026-09-02

**Status**: Draft

**Input**: User description: "remove the previous rendering mode (the one that split features across more than one rendered tile). do this in careful phases, verifying first that the new rendering mode can replace functionality and tests where the older mode is still used."

## Overview

The project draws a diagram two ways. The older way — the one that has been the default since before
there was a choice — splits almost every feature of a knot across two vertically adjacent cells: a
crossing is half `\` in one cell and half `A` in the cell above, an opening is half `(` and half `.`,
a strand climbing two levels is spread over three columns. Call it the **split-cell rendering**.
Spec 003 added the **opening-centered rendering**, in which every feature is drawn whole inside a
single cell, and made the two swappable specifically so the newer one could be compared against the
older one and examined for regressions.

That comparison has now been done. This feature retires the split-cell rendering, leaving one
rendering and no choice to make. The value is subtraction: eight of the sixteen cell kinds exist only
to hold the missing half of a feature that lives one row away, every drawing path is written twice,
every recorded picture is recorded twice, and every caller — the library's own tests, the example
command-line program, the app — has to say which rendering it wants before it can ask for a picture.
Removing the older rendering removes all of that, and it makes the one-character-to-one-cell promise
of the diagram text format true of the picture as well as of the text.

Nothing about the abbreviated knot notation, the meaning of a diagram, or what the surviving
rendering draws changes. The pictures the project produces after this feature are exactly the
pictures the opening-centered rendering produces today. One thing does narrow: the last phase
removes the eight cell kinds that only the older rendering ever drew, so the eight characters that
named them stop naming anything and diagram text using one is rejected. That is a deliberate
breaking change to the diagram text format, and it is why that phase stands on its own.

### Why phases, and why verification first

The user asked for this to be done in careful phases, verifying first that the surviving rendering
can replace the functionality and the tests where the older one is still used. That ordering is a
requirement of this feature, not a suggestion, because the split-cell rendering is not only a
display choice today:

- **Rotation depends on it.** Rotating a diagram works by drawing the diagram, reading the picture
  back with patterns that encode the split-cell tile shapes, and recovering notation from what it
  read. It is pinned to the split-cell rendering for exactly that reason. Whether it could simply be
  pointed at the other one was the feature's open question; Phase 0 research answered yes, but the
  answer had to be established by evidence before anything moved (FR-008a).
- **Recorded pictures are split-cell pictures.** Roughly half the project's recorded renderings, and
  the rendered knot in the project's own README, were captured in the split-cell rendering.
- **Several tests exist only to compare the two.** Tests that assert the two renderings agree on
  transfer-free knots, that a climb costs two columns where the older rendering spends three, and
  that the newer rendering never emits a retired cell, all lose their subject when one rendering
  goes. Some of them are the only place a real invariant is checked, and those invariants have to be
  restated in absolute terms before the comparison they ride on is deleted.

So the first phase deletes nothing. It establishes, use by use, that the surviving rendering already
covers — or can be made to cover — everything the older one is used for, and it lands that coverage
while both renderings still exist and every existing test still passes. Only then does anything come
out.

## Clarifications

### Session 2026-09-02

- Q: Rotation recovers notation by reading back a split-cell picture with patterns that encode the split-cell tile shapes. Is it acceptable for those shapes to survive as a private detail of rotation that no caller can select? → A: No — port the read-back. Rotation keeps its draw-then-read approach, but reads the surviving rendering's pictures instead, with patterns derived against the surviving rendering's tile shapes. The split-cell vocabulary is therefore gone from the project entirely, not merely hidden. Because the surviving rendering draws each feature whole in one cell but steps a strand transfer differently, the patterns are a genuine re-derivation rather than a substitution, and the requirement that rotation produce identical results (FR-006) is what proves the re-derivation right.
- Q: What happens to the eight cell kinds that only the split-cell rendering ever drew (`A`, `a`, `'`, `,`, `j`, `r`, `2`, `L`), which the surviving rendering already treats as synonyms of the empty cell? → A: Removed outright. They stop being cell kinds, their characters stop naming a cell, and diagram text using one is rejected with the same unrecognised-character message any other unknown character gets, naming the character and its row and column. This is a deliberate breaking change to the diagram text format: text that parses today stops parsing. It supersedes spec 003's treatment of the eight as normalizing synonyms of the empty cell, which existed only because the split-cell rendering still needed them.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Prove the surviving rendering covers every remaining use, before deleting anything (Priority: P1)

Someone preparing the removal walks every place the split-cell rendering is still used — the
library's drawing paths, its tests, its recorded pictures, the rotation machinery, the example
command-line program, the app, and the project's documentation — and for each one establishes what
would replace it. Where an equivalent under the surviving rendering does not exist yet, they create
it and land it now, while both renderings are still present and every existing test still passes.
Where no equivalent is possible, they say so plainly rather than deleting and hoping.

**Why this priority**: The user asked for verification first, and this phase is that verification.
It is also the phase that finds out whether the removal is safe at all — the rotation dependency and
the comparison-only tests are exactly the sort of thing a straight deletion would silently lose.
Landed on its own it delivers value even if the removal never happens: the surviving rendering ends
up with test and snapshot coverage matching the older one's.

**Independent Test**: Read the audit, confirm every remaining use of the split-cell rendering appears
in it with a named replacement, then build and test the project and confirm both renderings still
work, the split-cell rendering is still the default, and nothing that passed before now fails.

**Acceptance Scenarios**:

1. **Given** the project as it stands, **When** the audit is produced, **Then** it lists every
   remaining use of the split-cell rendering — in the library, in its tests, in its recorded
   pictures, in rotation, in the example program, in the app, and in project documentation — and for
   each names either the equivalent that already covers it, the equivalent to be created, or the
   reason no equivalent is possible.
2. **Given** a recorded picture that exists only for the split-cell rendering, **When** this phase
   completes, **Then** a recorded picture of the same diagram under the surviving rendering exists
   and passes.
3. **Given** a behaviour that only a split-cell test asserts, **When** this phase completes,
   **Then** a test asserting the same behaviour under the surviving rendering exists and passes.
4. **Given** an invariant stated as a comparison between the two renderings, **When** this phase
   completes, **Then** the same invariant is also stated in absolute terms, without reference to the
   split-cell rendering, and that absolute statement passes.
5. **Given** this phase is complete, **When** the project is built and tested, **Then** both
   renderings still exist, the split-cell rendering is still the default, and every test that passed
   before this phase still passes.
6. **Given** a use of the split-cell rendering for which no equivalent can be produced, **When** the
   audit is produced, **Then** it is named explicitly along with what is missing, and no later phase
   deletes anything that depends on it until it is resolved.

---

### User Story 2 - Rotate a diagram without the split-cell rendering (Priority: P1)

Someone rotates a diagram 90° counter-clockwise and gets the same result they get today. What has
changed is underneath: rotation no longer needs the split-cell rendering to do its work, so nothing
stands in the way of that rendering going.

**Why this priority**: This is the one hard dependency. Every other use of the split-cell rendering
is a picture or a test that can be re-recorded or restated; rotation is behaviour that would break.
Until rotation is free of it, the removal cannot proceed, so this phase gates the rest.

**Independent Test**: Rotate each knot in the project's rotation tests and confirm the resulting
notation is identical to what the same rotation produces today, then confirm rotation no longer
reaches for the split-cell rendering.

**Acceptance Scenarios**:

1. **Given** any knot the project can rotate today, **When** it is rotated after this phase,
   **Then** the resulting notation is identical, item for item, to what rotation produces today.
2. **Given** the project's existing rotation tests, including the regression tests for past rotation
   defects, **When** they are run after this phase, **Then** they pass with their expectations
   unchanged.
3. **Given** a knot rotated four times, **When** the result is compared with the original, **Then**
   it matches, as it does today.
4. **Given** a knot whose rotation fails today with a specific error, **When** it is rotated after
   this phase, **Then** it fails the same way rather than succeeding or failing differently.
5. **Given** rotation after this phase, **When** the split-cell rendering is made unavailable,
   **Then** rotation still produces correct results.

---

### User Story 3 - Draw a diagram with no rendering to choose (Priority: P1)

Someone asking the library, the example program, or the app for a picture of a diagram simply gets
one. There is no rendering mode to pass, no toggle in the app, no environment variable on the command
line, and no way to ask for the split-cell picture, because there is only one rendering left.

**Why this priority**: This is the feature the user asked for. It cannot land before the two phases
above, but once they are done it is the whole point of the exercise.

**Independent Test**: Ask the library for a picture without naming a rendering; open the app and
confirm there is no rendering control and the picture is the opening-centered one; run the example
program and confirm it prints the opening-centered picture with every combination of its remaining
display options.

**Acceptance Scenarios**:

1. **Given** a diagram, **When** a picture of it is requested from the library, **Then** no rendering
   mode is named in the request and the picture returned is byte-for-byte the picture the
   opening-centered rendering produces today.
2. **Given** the app, **When** it is opened in either notation mode or manual diagram mode,
   **Then** there is no rendering-mode control, and diagrams are drawn opening-centered.
3. **Given** a saved app state that names the split-cell rendering, or names no rendering at all,
   **When** the app loads it, **Then** it loads without error, keeps every other saved setting, and
   draws opening-centered.
4. **Given** the example command-line program, **When** it is run, **Then** it prints the
   opening-centered picture, and every combination of its compact and cell-boundary options is still
   reachable.
5. **Given** the project's recorded pictures, **When** this phase completes, **Then** the split-cell
   recordings are gone, the opening-centered recordings remain, and each surviving recording is named
   for what it records rather than for the rendering that produced it.
6. **Given** a test that existed only to compare the two renderings, **When** this phase completes,
   **Then** it is gone, and every invariant it was the only check for is asserted by a test that
   survives.
7. **Given** the project's documentation, including the rendered knot in the README, **When** this
   phase completes, **Then** every picture shown is one the project actually produces now.

---

### User Story 4 - Stop carrying the half-cells (Priority: P2)

Someone reading the list of cell kinds sees only cells that are actually drawn. The eight that
existed solely to hold the missing half of a feature one row away are gone from the vocabulary, from
the diagram text format's symbol table, and from the table the app shows.

**Why this priority**: This is the cleanup the removal makes possible rather than the removal itself.
The rendering is already gone by this point and the project is already correct without it; carrying
eight cells that draw nothing is untidy rather than broken. It is also the only phase that narrows
what diagram text the project accepts — text naming one of the eight stops parsing — so it is worth
landing on its own, where that breaking change is visible rather than buried in a larger removal.

**Independent Test**: Ask for the list of cell kinds and confirm only drawn cells appear; write
diagram text using a retired character and confirm it behaves as specified; open the app's symbol
table and confirm it matches.

**Acceptance Scenarios**:

1. **Given** the set of cell kinds after this phase, **When** it is enumerated, **Then** it contains
   only cells the surviving rendering draws, and none of the eight half-cells.
2. **Given** the app's symbol table, **When** it is opened, **Then** every row corresponds to a cell
   that can actually be drawn, and there are no rows for retired cells.
3. **Given** diagram text using one of the eight retired characters, **When** it is read, **Then**
   it is rejected with the same message any other unrecognised character produces, naming the
   offending character and its row and column.
4. **Given** diagram text using only surviving characters, **When** it is read and written back,
   **Then** it round-trips byte for byte, as it does today.
5. **Given** an app snapshot saved before this phase whose diagram text uses a retired character,
   **When** it is restored, **Then** the app reports the text as invalid rather than discarding the
   snapshot silently, and the rest of the app keeps working.

---

### Edge Cases

- **A saved app state naming the split-cell rendering.** People have used the toggle. Their saved
  state names a rendering that no longer exists; it must load without error and without discarding
  their other settings, exactly as the app already tolerates a rendering name it does not recognise.
- **App snapshots holding diagram text.** A snapshot records diagram text, not a picture, so it is
  drawn under whichever rendering exists when it is viewed. A snapshot whose text uses one of the
  eight retired characters becomes invalid at the last phase: the app must say so and carry on,
  rather than discarding the snapshot silently or failing to load the rest of its saved state.
- **Text that parses today and stops parsing.** Removing the eight cell kinds narrows the diagram
  text format. Any fixture, documented example, or saved text in the project that names one of the
  eight has to be found and updated in the same phase that removes them, or it becomes a failure
  with no obvious cause.
- **Invariants stated as comparisons.** Several existing checks say "the same as the split-cell
  rendering" or "two columns where the split-cell rendering spends three". Deleting one rendering
  makes them vacuous or meaningless. Each has to be restated absolutely first — a climb costs one
  column per level; the picture has no leading or trailing all-blank lines; a diagram of a given
  width renders to a picture of a given width — or the invariant is lost without anything failing.
- **Recorded pictures that pass vacuously.** A comparison test that skips every case still passes.
  Any surviving test that filters its inputs must assert it actually checked something, as the
  existing comparison test already does.
- **The rendered knot in the README.** It is a split-cell picture. Under the surviving rendering the
  square knot it shows contains strand transfers, so the picture genuinely differs. It has to be
  re-captured, not assumed unchanged.
- **Diagrams with transfers.** These are the diagrams whose two renderings legitimately differ.
  Every recorded picture of such a diagram changes when the split-cell recording goes away, and a
  reviewer comparing the before and after must be able to tell that difference from a regression —
  so the surviving recordings must already exist and be reviewed in phase one, not created during
  the deletion.
- **The empty diagram, and near-empty diagrams.** A diagram with no items, a single opening/closing
  pair, and a diagram with blank rows must all still render without error and without gaining or
  losing blank lines.
- **Ragged diagram text.** Trailing cells inferred as empty behave exactly as they do today; nothing
  about the format's rules changes here.
- **A phase landing on its own.** Each phase must leave the project building, passing its tests, and
  usable. No phase may depend on a later one to restore correctness.

## Requirements *(mandatory)*

### Functional Requirements

#### Phase ordering

- **FR-001**: The verification phase MUST land before any deletion. Its completion state MUST have
  both renderings present, the split-cell rendering still the default, and every test that passed
  beforehand still passing.
- **FR-002**: The verification phase MUST produce a written audit covering every remaining use of the
  split-cell rendering — in the library's drawing paths, its tests, its recorded pictures, rotation,
  the example program, the app, and project documentation — naming for each the replacement that
  covers it or the reason none is possible.
- **FR-003**: For every behaviour, invariant, or recorded picture that only the split-cell rendering
  covers today, an equivalent under the surviving rendering MUST exist and pass before the
  split-cell version is deleted.
- **FR-004**: Rotation MUST be free of the split-cell rendering before any phase that removes it.
- **FR-005**: Each phase MUST leave the project building, passing its tests, and usable on its own;
  no phase may rely on a later one to restore correctness.

#### Rotation

- **FR-006**: Rotating a diagram MUST produce the same result after this feature as before it, for
  every diagram the project can rotate today — the same notation, item for item.
- **FR-007**: A diagram that fails to rotate today MUST fail the same way afterwards, with an
  equivalent message.
- **FR-008**: Rotation MUST recover notation by reading back a picture drawn in the surviving
  rendering, using patterns derived against that rendering's tile shapes. It MUST NOT retain the
  split-cell tile shapes, publicly or privately.
- **FR-008a**: The read-back MUST be shown to produce identical results against the surviving
  rendering before it is re-pointed at it. That evidence MUST be a differential check run while both
  renderings still exist, over a corpus far wider than the project's rotation tests, and MUST include
  diagrams containing strand transfers — the case where the two renderings legitimately differ — and
  MUST establish that the two agree on real results rather than by failing alike.

  *Amended after Phase 0 research (see [research.md](./research.md), R1).* This requirement
  originally mandated re-deriving the read-back patterns against the surviving rendering's tile
  shapes, on the assumption that the different transfer stepping would demand it. A differential run
  over every valid diagram of length 2–8 and height ≤ 8 — 175,536 diagrams, 170,928 of them
  containing transfers, all rotating successfully under both renderings — found the two agree on
  100% of them, so the existing patterns need no change. The requirement now asks for that evidence
  instead of for the rewrite. What the user decided at clarification time is unchanged: the read-back
  is pointed at the surviving rendering, and the split-cell tile shapes survive nowhere, publicly or
  privately.

#### One rendering

- **FR-009**: The library MUST draw a diagram exactly one way, and that way MUST produce byte-for-byte
  the pictures the opening-centered rendering produces today.
- **FR-010**: Requesting a picture MUST NOT require naming a rendering — in the library, in the
  example program, or in the app.
- **FR-011**: The app MUST NOT offer a rendering-mode control, and MUST NOT persist a rendering-mode
  choice.
- **FR-012**: The app MUST load a previously saved state that names the split-cell rendering, or names
  no rendering, without error, preserving every other saved setting.
- **FR-013**: The example command-line program MUST NOT offer a way to select a rendering, and every
  combination of its remaining display options — the compact view and the cell-boundary view — MUST
  remain reachable.
- **FR-014**: Both existing display options — the compact view and the cell-boundary view — MUST
  behave exactly as they do today under the surviving rendering.
- **FR-015**: Every cell-boundary picture MUST continue to give each character of diagram text exactly
  one box, and MUST continue to draw every crossing, opening and closing whole inside a single box.

#### Tests and recorded pictures

- **FR-016**: Recorded pictures captured under the split-cell rendering MUST be removed, and the
  surviving recordings MUST be named for what they record rather than for the rendering that produced
  them.
- **FR-017**: Tests whose only purpose was to compare the two renderings MUST be removed, and every
  invariant such a test was the sole check for MUST be asserted by a surviving test stated without
  reference to the split-cell rendering.
- **FR-018**: A surviving test that filters its inputs MUST assert that it checked at least one case,
  so that it cannot pass vacuously.
- **FR-019**: Every picture shown in project documentation MUST be one the project actually produces
  after this feature.

#### Cell vocabulary and diagram text

- **FR-020**: The eight cell kinds that only the split-cell rendering drew — cross up over, cross up
  under, opened above, closed above, transfer up start, transfer up finish, transfer down start,
  transfer down finish — MUST be removed outright. They MUST cease to be cell kinds, and their
  characters MUST cease to name a cell.
- **FR-020a**: Diagram text containing one of those eight characters MUST be rejected with the same
  unrecognised-character message any other unknown character produces, naming the offending
  character and its row and column. This supersedes spec 003's treatment of the eight as normalizing
  synonyms of the empty cell.
- **FR-020b**: Where the project or the app holds diagram text saved before this feature that uses
  one of the eight characters — an app snapshot, a fixture, a documented example — that text MUST
  be reported as invalid rather than silently discarded, and MUST NOT prevent the rest of the app or
  the rest of the test suite from working.
- **FR-021**: Diagram text using only cells the surviving rendering draws MUST round-trip byte for
  byte, as it does today.
- **FR-022**: The symbol table the app shows MUST list exactly the cells the format accepts, with no
  entry for a cell that cannot be drawn.
- **FR-023**: The abbreviated knot notation, the meaning of a diagram, and every diagram manipulation
  other than rotation — moves, bulge detection, snapshots — MUST be unchanged by this feature.
- **FR-024**: A strand moving one level up or down MUST occupy exactly one cell and one column, and
  climbing N levels MUST take N columns — the invariant that today is stated only as a ratio against
  the split-cell rendering.

### Key Entities

- **Split-cell rendering**: the older of the two renderings, in which a feature may be drawn across
  two vertically adjacent cells and a climb of two levels spans three columns. The thing this feature
  removes. Currently the default and currently the rendering rotation reads.
- **Surviving rendering**: the opening-centered rendering added by spec 003, in which every feature is
  drawn whole inside one cell. After this feature, the only rendering, and no longer named as a choice.
- **Half-cell**: one of the eight cell kinds that exist only to carry the missing half of a feature
  that lives one row away. Never drawn by the surviving rendering, and removed outright by the last
  phase — after which its character no longer names a cell.
- **Audit**: the record produced by the verification phase — every remaining use of the split-cell
  rendering paired with what replaces it. The gate the later phases pass through.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: After the feature, zero places in the project — library, tests, example program, app,
  documentation — name or select a rendering; there is nothing to choose.
- **SC-002**: 100% of the pictures the project produces after the feature are byte-for-byte identical
  to the pictures the opening-centered rendering produces before it.
- **SC-003**: 100% of the knots covered by the project's rotation tests rotate to the same notation
  before and after the feature.
- **SC-004**: Every remaining use of the split-cell rendering identified in the audit is resolved
  before the deletion that depends on it lands — no deletion lands against an open audit entry.
- **SC-005**: The number of behaviours and invariants under test does not drop: every assertion the
  split-cell tests made is made by a surviving test, and no surviving test can pass without checking
  at least one case.
- **SC-006**: After the verification phase and before any deletion, the project builds and 100% of
  previously passing tests still pass, with both renderings present.
- **SC-007**: Each phase, taken on its own, leaves the project building and its tests passing.
- **SC-008**: An app state saved before the feature loads afterwards without error and without losing
  any setting other than the retired rendering choice — 100% of previously savable states.
- **SC-009**: Both display options — compact and cell boundaries — remain reachable in all four
  combinations from the library and the example program, and in the places the app offers them today.
- **SC-010**: Diagram text using cells the surviving rendering draws round-trips byte for byte, in
  100% of cases that round-trip today under the opening-centered rendering.
- **SC-011**: The count of cell kinds the project carries drops to the number the surviving rendering
  actually draws, with no cell that draws nothing.
- **SC-012**: Diagram text naming any of the eight retired characters is rejected in 100% of cases,
  with a message naming the character and its position, and no such text remains anywhere in the
  project's own fixtures, examples, or documentation.
- **SC-013**: Rotation reads back only the surviving rendering; the split-cell tile shapes appear
  nowhere in the project, in any form a caller or an internal path can reach.

## Assumptions

- The comparison the two renderings existed to enable has been done, and the opening-centered
  rendering is trusted. This feature is the removal that comparison was meant to lead to; it does not
  re-open the question of which rendering is correct.
- The pictures the project produces will change where the two renderings legitimately differ — every
  diagram containing a strand transfer, including the knot rendered in the README. This is the
  intended consequence of removing the older rendering, not a regression.
- Rotation is the only behaviour, as opposed to display, that depends on the split-cell rendering.
  The audit in phase one exists partly to confirm that; if it finds another, that dependency joins
  phase two's scope.
- Rotation keeps working by drawing a diagram and reading the picture back, rather than being
  reworked to read the diagram's own representation. That larger change is out of scope here; this
  feature only re-points the read-back at the surviving rendering.
- Narrowing the diagram text format is acceptable. Text naming one of the eight retired characters
  is rare — the eight were only ever emitted by the rendering being removed — and rejecting it
  outright is preferred to carrying eight names for cells that can never be drawn.
- The app's persisted state already tolerates a rendering name it does not recognise, so a saved
  split-cell choice degrades to the surviving rendering without special handling. Phase three
  confirms this rather than assuming it.
- Diagram text remains one character per cell, case-sensitive, whitespace-rejecting, with ragged rows
  padded — every rule of the format other than which characters name a cell is untouched.
- The example program keeps selecting its remaining display options the way it does today; only the
  rendering selection goes.
- Per the project constitution, each phase lands in the core library first, with the app and the
  example program following as surfaces over it.
- No new dependency is needed for any phase.
