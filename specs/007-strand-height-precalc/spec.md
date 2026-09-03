# Feature Specification: Height-Precalculated Strand Placement (Rendering Mode)

**Feature Branch**: `claude/diagram-strand-height-precalc-p4l2lo`

**Created**: 2026-06-18

**Status**: Draft

**Input**: User description: "Diagram rendering mode that precalculates diagram max heights for strands so that it can place the opening features for those strand openings at a diagram height that reduces the need for strands to be moved up and down via diagonals as other strands are opened and closed, respectively, below them in the diagram"

## Clarifications

### Session 2026-06-18

- Q: Should the new mode apply max-row placement unconditionally, or weigh crossing-alignment cost against displacement savings? → A: Unconditional max-row placement — always open each strand at its precalculated maximum row, accepting that crossing-heavy diagrams may net out with equal-or-more total transfers (open/close displacement is still strictly reduced).
- Q: How should the rotation benefit be stated, given that not every transfer inflates scanned features? → A: Only transfers later reversed by an opposite-direction transfer (the avoidable up-then-down open/close displacement) inflate the scanned feature count; many transfers scan to nothing, and the crossing-alignment transfers the new mode adds are scanned but do not increase the feature count. Therefore rotating in the new mode never increases scanned features and strictly reduces them for diagrams containing such reversed-direction displacement, with knot equivalence always preserved.
- Q: Which operations carry the rendering mode, and how is it supplied? → A: The user selects a single rendering mode to work in and all operations run under that active mode (mode is an operating context/state, not a per-call argument). Only rotation's produced notation actually changes with the mode; notation-only moves (swap, wrap-around, change-crossing, Reidemeister, bulge/collapse) yield identical results regardless. The existing mode stays the default for now (it is trusted as correct); the new mode is opt-in and is expected to become the default later once proven — no immediate migration.

## User Scenarios & Testing *(mandatory)*

<!--
  Background (domain): A knot diagram is rendered as horizontal "strands". An
  opening feature introduces a new strand pair; a closing feature retires one.
  When a strand pair opens *beneath* strands that are already present, those
  existing strands are pushed up a row; when a pair closes beneath them, they
  are pulled back down. Today the renderer places every opening at the lowest
  free row, so a strand that is only "passing through" gets shoved up and later
  pulled back down, drawing diagonal "transfer" segments (a staircase) instead
  of a straight horizontal line. This feature adds a rendering mode that opens
  each strand directly at the highest row it will ever need, so it can run flat.

  Primary motivation (diagram rotation): the rotation move is performed by
  scanning the *rendered* diagram grid and re-deriving the notation from it.
  Not every transfer diagonal becomes an extra feature when scanned — many
  scan to nothing. The transfers that DO inflate the scanned feature count are
  (especially, perhaps only) those where a strand transfers one direction and
  is later transferred back the opposite direction: the avoidable up-then-down
  open/close displacement this feature removes. Because those reversed-direction
  transfers re-encode as extra features, today's renderer makes a diagram grow
  progressively more complicated the more it is rotated, even though the knot is
  unchanged. Removing them keeps the scanned feature count stable across
  rotations. (Crossing-alignment transfers the new mode adds are scanned but do
  not increase the resulting feature count.)
-->

### User Story 1 - Render with reduced up-and-down strand movement (Priority: P1)

A person rendering a knot diagram chooses the height-precalculated rendering mode. For each opening, the renderer first determines the maximum vertical row the resulting strand will occupy at any point before it closes, and opens the strand directly at (or near) that row. Strands that previously zig-zagged up and then back down as other strands opened and closed beneath them instead run as straight horizontal lines.

**Why this priority**: This is the core value of the feature — cleaner, easier-to-read diagrams. Without it, nothing else matters. It is independently shippable as a new rendering path.

**Independent Test**: Render a diagram known to produce avoidable up-then-down movement (for example the `terrace` diagram, abbreviated notation `(0 (2 (4 (6 )5 )3 )1 (1 (3 (5 )6 )4 )2 )0`) in the new mode and confirm that strands which previously climbed and descended now render with no intermediate vertical movement between their opening and closing, while the diagram still represents the same knot.

**Acceptance Scenarios**:

1. **Given** a diagram in which a strand opens, is passed over by one or more openings/closings beneath it, and then closes at the same row it opened, **When** rendered in the height-precalculated mode, **Then** that strand is drawn flat (no diagonal transfer segments) for the portion where it was previously displaced up and back down.
2. **Given** the `terrace` diagram, **When** rendered in the height-precalculated mode, **Then** the total number of diagonal transfer segments is lower than in the default mode and the rendered figure decodes to the same knot.
3. **Given** any valid diagram, **When** rendered in the height-precalculated mode, **Then** the figure represents the same knot as the default-mode rendering of that diagram.

---

### User Story 2 - Keep diagram complexity stable under repeated rotation (Priority: P2)

A person repeatedly applies the rotation move to a diagram. Because rotation re-derives the diagram by scanning the rendered grid, transfers that reverse direction (a strand pushed up then later pulled back down) are re-encoded as additional diagram features and accumulate with each rotation, making the diagram progressively more complicated even though the knot is unchanged. (Many transfers scan to nothing and do not inflate the count.) Using the height-precalculated rendering mode for the scan removes those avoidable reversed-direction transfers, so the scanned feature count stays stable across rotations.

**Why this priority**: This is the motivating use case for the feature. Without it, repeated rotation inflates the feature count and degrades usability of the rotation move; it is the primary real-world payoff of reduced transfers. It is independently testable against the existing rotation move.

**Independent Test**: Take a diagram, render/scan it for rotation in the new mode, rotate it through a full cycle (e.g., four 90° rotations back to the original orientation), and confirm the number of scanned diagram features does not grow across rotations and the final diagram represents the same knot as the original.

**Acceptance Scenarios**:

1. **Given** a diagram whose default rendering contains reversed-direction (up-then-down) transfers, **When** it is rotated using the height-precalculated rendering for the scan, **Then** the resulting diagram has fewer scanned features than the default-mode rotation and no more than before the rotation.
2. **Given** a diagram, **When** it is rotated through a full cycle back to its original orientation using the new mode, **Then** the scanned feature count does not grow across the cycle and the final diagram represents the same knot as the original.

---

### User Story 3 - Opt in without changing existing output (Priority: P3)

A downstream consumer of the library (or example app) selects a single rendering mode to work in — the existing default rendering or the new height-precalculated rendering — and all subsequent operations run under that active mode. The mode is an operating context, not just a display option: rotation's produced notation depends on it. Existing renders, snapshots, and example outputs are unaffected unless the new mode is explicitly chosen.

**Why this priority**: Protects existing behavior and consumers. The feature must be additive; the existing mode stays the default so current snapshots and downstream expectations are unchanged, while the new mode is opt-in until proven and expected to become the default later.

**Independent Test**: Render a set of diagrams in the default mode and confirm output is byte-for-byte identical to today's output; render the same diagrams in the new mode and confirm the precalculated placement is used.

**Acceptance Scenarios**:

1. **Given** any diagram, **When** operated on in the default mode, **Then** rendering and rotation results are identical to the current behavior.
2. **Given** the same diagram, **When** operated on in the height-precalculated mode, **Then** opening features are placed at their precalculated rows rather than the lowest free row.
3. **Given** a working context with one mode selected, **When** a notation-only move (e.g., swap, Reidemeister, wrap-around) is applied, **Then** the result is identical regardless of which mode is active.

---

### User Story 4 - Fidelity preserved across all element types (Priority: P4)

A person renders diagrams that contain crossings as well as openings and closings. The new mode places strands at precalculated heights while still aligning crossings with their partner strands, so the rendered diagram remains a faithful representation of the abbreviated notation.

**Why this priority**: Correctness across the full element set. Openings/closings are the focus, but crossings must continue to render correctly when strands sit at new rows; otherwise the mode is unusable for real knots.

**Independent Test**: Render diagrams containing crossings (for example `basket` and `ugly_trefoil`) in the new mode and confirm each crossing still connects the correct two strands and the figure decodes to the same knot.

**Acceptance Scenarios**:

1. **Given** a diagram containing crossings, **When** rendered in the height-precalculated mode, **Then** every crossing connects the same two strands as in the default rendering.
2. **Given** a diagram with deeply nested openings, **When** rendered in the new mode, **Then** strand placement never overlaps two strands on the same row and the figure is well-formed.
3. **Given** a crossing whose two strands are not adjacent under the precalculated placement, **When** rendered in the new mode, **Then** the strands are transferred to adjacent rows for the crossing and the crossing is never drawn between non-adjacent rows.

---

### Edge Cases

- **Empty diagram**: renders nothing and does not error.
- **Single opening/closing pair with nothing opening beneath it**: the strand is already at its maximum height, so the new mode produces output equivalent to the default (no diagonals to remove).
- **Strand whose opening row already equals its maximum row**: no diagonal transfer is introduced or removed for that strand.
- **Unavoidable boundary diagonals**: a strand must still enter at its opening index and leave at its closing index; diagonals intrinsic to those transitions are permitted. Only the avoidable up-then-down displacement between them is removed.
- **Crossing partners no longer adjacent**: the default mode keeps any two strands that can cross directly above/below each other at a uniform distance, so a crossing is always between neighboring rows. Placing strands at their overall maximum row can separate two crossing partners; the new mode must then transfer them together to be adjacent for the crossing (and back afterward). This is a *new* class of transfer the default mode never needs, and it is an accepted cost of the new mode — see FR-011 and SC-002.
- **Interleaved openings and closings at the same indices**: placement remains consistent and non-overlapping.
- **Closings at the very bottom row**: handled without error and without forcing avoidable movement on strands above.
- **Repeated rotation**: rotating a diagram many times using the new mode for the scan must not accumulate features from reversed-direction transfers; the scanned feature count stays bounded by the knot's actual complexity rather than growing per rotation. (Transfers that scan to nothing, and crossing-alignment transfers, do not contribute to growth.)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a selectable rendering mode (distinct from the default) that, before placing strands, precalculates for each opening the maximum vertical row the resulting strand will occupy between its opening and its matching closing.
- **FR-002**: System MUST place each opening feature unconditionally at its precalculated maximum row rather than at the lowest free row, without weighing crossing-alignment cost against displacement savings.
- **FR-003**: In the new mode, a strand that does not change rows between its opening and closing MUST render as a straight horizontal line with no diagonal transfer segments, wherever the precalculated placement makes that possible.
- **FR-004**: In the new mode, the number of *open/close displacement* transfer segments (those caused by a strand being pushed up by an opening beneath it and later pulled back down by a closing beneath it) MUST be reduced versus the default mode for any diagram exhibiting such displacement.
- **FR-005**: System MUST leave the default rendering behavior unchanged; the height-precalculated behavior MUST be opt-in.
- **FR-006**: For every valid diagram, the new mode's rendering MUST represent the same knot as the default mode's rendering (notation fidelity preserved).
- **FR-007**: The new mode MUST support all existing diagram elements — openings, closings, and crossings — and MUST keep each crossing aligned with its correct partner strand.
- **FR-008**: The new mode MUST produce deterministic output: the same diagram always renders identically.
- **FR-009**: The new mode MUST still emit diagonal transfer segments that are intrinsic to a strand entering at its opening index or leaving at its closing index; only avoidable up-then-down movement is removed.
- **FR-010**: The new mode MUST handle empty and degenerate diagrams without error, producing output equivalent to the default mode where no avoidable movement exists.
- **FR-011**: When a crossing's two participating strands are not on adjacent rows under the precalculated placement, the new mode MUST insert the transfer segments needed to bring them adjacent for the crossing and to restore their placement afterward. These crossing-alignment transfers are permitted even though they are not open/close displacement transfers, and the rendering MUST never draw a crossing between non-adjacent rows.
- **FR-012**: The rendering mode MUST be a single operating context a user selects to work in; all diagram operations run under the active mode. Because rotation re-derives notation from the rendered grid, the rotation result MUST reflect the active mode; notation-only moves (swap, wrap-around, change-crossing, Reidemeister, bulge/collapse) MUST produce identical results regardless of the active mode.
- **FR-013**: The active rendering mode MUST default to the existing (legacy) mode so that current behavior — including rotation results and snapshots — is unchanged unless the user opts into the new mode. (Making the new mode the default, and any migration away from the legacy mode, is out of scope for this feature.)

### Key Entities *(include if feature involves data)*

- **Strand pair**: the two lines introduced by a single opening feature and retired by its matching closing feature; occupies a vertical row that may change over the diagram's width.
- **Opening feature**: the element that introduces a strand pair; the element whose placement row this feature precalculates.
- **Maximum strand row (precalculated height)**: the highest vertical row a given strand pair occupies at any point between its opening and closing — the target placement row for the new mode.
- **Diagonal transfer segment**: a rendered segment that moves a strand up or down between rows. Two kinds matter here: *open/close displacement transfers*, caused by openings/closings beneath a passing strand (the kind this feature reduces); and *crossing-alignment transfers*, needed to bring two crossing partners adjacent when precalculated placement has separated them (a new cost the default mode never incurs).
- **Crossing-alignment transfer**: a transfer the new mode adds to make two crossing strands adjacent at the moment they cross (and to restore placement afterward), because the default mode's guarantee that crossing partners are always adjacent no longer holds once strands sit at their maximum rows.
- **Scanned diagram feature**: an element the rotation move recovers by reading the rendered grid (openings, closings, crossings). Not every transfer becomes one — many scan to nothing. The transfers that inflate this count are (especially, perhaps only) those later reversed by an opposite-direction transfer, i.e. the avoidable up-then-down displacement the new mode removes; crossing-alignment transfers are scanned but do not add features. Reducing the reversed-direction transfers lowers the count without changing the knot, which is what the new mode aims for.
- **Active rendering mode (operating context)**: the single mode a user has selected to work in. It governs all operations; rotation's produced notation depends on it, while notation-only moves are independent of it. Defaults to the legacy mode; selecting the new mode is opt-in.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: For the `terrace` diagram, every strand that currently moves up and then back down renders with no intermediate vertical movement between its opening and closing — the staircase is eliminated.
- **SC-002**: For crossing-free diagrams, every diagram containing avoidable up-then-down strand movement renders with strictly fewer total diagonal transfer segments than the default mode, and none renders with more. For diagrams that contain crossings, open/close displacement transfers are strictly reduced where such displacement exists; the new mode may add crossing-alignment transfers (FR-011), so total transfer count is measured and reported per example, and the open/close-displacement and crossing-alignment counts are tracked separately so the tradeoff is explicit rather than hidden.
- **SC-003**: 100% of valid diagrams render to the same knot in the new mode as in the default mode (verified by round-trip / equivalence checks).
- **SC-004**: Default-mode output remains byte-for-byte identical to current output for every existing example and snapshot.
- **SC-005**: Rendering is deterministic — rendering the same diagram twice in the new mode yields identical output every time.
- **SC-006**: When a diagram is rotated using the new mode for the scan, the scanned feature count never increases relative to the original (crossing-alignment transfers the mode adds are scanned but do not add features), and for diagrams whose default rendering contains reversed-direction (up-then-down) transfers it is strictly lower than the default-mode rotation. Rotating through a full cycle back to the original orientation yields a feature count no greater than the original, and the diagram still represents the same knot.

## Assumptions

- The height-precalculated behavior is an additive, opt-in rendering path; the existing default rendering and its public output remain unchanged (per Constitution: Library-First, and to protect existing `insta` snapshots).
- "Reduce the need for strands to be moved up and down" means minimizing avoidable open/close displacement, not necessarily computing a globally optimal placement; a placement at each strand's maximum occupied row is the intended heuristic.
- The default mode's invariant that any two crossing strands are always vertically adjacent at a uniform distance does not survive max-row placement. The new mode therefore accepts crossing-alignment transfers (FR-011) as a tradeoff: it optimizes for fewer open/close displacement transfers, not for the global minimum of all transfers, and a crossing-heavy diagram could net out with a similar or larger total transfer count. Per the 2026-06-18 clarification, placement is unconditional (max row always) — the heuristic does not weigh crossing-alignment cost against displacement savings; that refinement is explicitly out of scope for this feature.
- The ASCII rendering is the target surface for this mode; the abbreviated knot notation remains the source of truth (per Constitution: Notation Fidelity), so example abbreviated-notation inputs and expected rendered outputs will accompany the implementation as snapshot tests.
- Expected outputs for the new mode will be captured via `insta` snapshot tests (per Constitution: Test-First), reviewed and accepted before commit.
- The rotation move (re-deriving notation by scanning the rendered grid) is the motivating consumer; the rotation algorithm itself is unchanged, but because it scans the rendered grid the notation it produces depends on the active mode. The two modes can therefore yield different — but equivalent — rotation results, which is why a working context fixes one mode.
- A working context (a diagram and the sequence of operations applied to it) uses a single active mode consistently. Mixing modes within one operation sequence is not a supported scenario; results are only defined relative to the active mode.
- Migration stance: the legacy mode is trusted and remains the default; the new mode is opt-in until validated in real use. Promoting the new mode to default (and any deprecation of the legacy mode) is explicitly out of scope for this feature.
- All new code lands in the core `knotty` crate and must compile for `wasm32-unknown-unknown` (per Constitution: WASM-Compatible).
