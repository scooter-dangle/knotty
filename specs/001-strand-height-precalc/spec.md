# Feature Specification: Height-Precalculated Strand Placement (Rendering Mode)

**Feature Branch**: `claude/diagram-strand-height-precalc-p4l2lo`

**Created**: 2026-06-18

**Status**: Draft

**Input**: User description: "Diagram rendering mode that precalculates diagram max heights for strands so that it can place the opening features for those strand openings at a diagram height that reduces the need for strands to be moved up and down via diagonals as other strands are opened and closed, respectively, below them in the diagram"

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

### User Story 2 - Opt in without changing existing output (Priority: P2)

A downstream consumer of the library (or example app) selects between the existing default rendering and the new height-precalculated rendering. Existing renders, snapshots, and example outputs are unaffected unless the new mode is explicitly chosen.

**Why this priority**: Protects existing behavior and consumers. The feature must be additive; changing the default would break existing snapshots and downstream expectations.

**Independent Test**: Render a set of diagrams in the default mode and confirm output is byte-for-byte identical to today's output; render the same diagrams in the new mode and confirm the precalculated placement is used.

**Acceptance Scenarios**:

1. **Given** any diagram, **When** rendered in the default mode, **Then** the output is identical to the current rendering.
2. **Given** the same diagram, **When** rendered in the height-precalculated mode, **Then** opening features are placed at their precalculated rows rather than the lowest free row.

---

### User Story 3 - Fidelity preserved across all element types (Priority: P3)

A person renders diagrams that contain crossings as well as openings and closings. The new mode places strands at precalculated heights while still aligning crossings with their partner strands, so the rendered diagram remains a faithful representation of the abbreviated notation.

**Why this priority**: Correctness across the full element set. Openings/closings are the focus, but crossings must continue to render correctly when strands sit at new rows; otherwise the mode is unusable for real knots.

**Independent Test**: Render diagrams containing crossings (for example `basket` and `ugly_trefoil`) in the new mode and confirm each crossing still connects the correct two strands and the figure decodes to the same knot.

**Acceptance Scenarios**:

1. **Given** a diagram containing crossings, **When** rendered in the height-precalculated mode, **Then** every crossing connects the same two strands as in the default rendering.
2. **Given** a diagram with deeply nested openings, **When** rendered in the new mode, **Then** strand placement never overlaps two strands on the same row and the figure is well-formed.

---

### Edge Cases

- **Empty diagram**: renders nothing and does not error.
- **Single opening/closing pair with nothing opening beneath it**: the strand is already at its maximum height, so the new mode produces output equivalent to the default (no diagonals to remove).
- **Strand whose opening row already equals its maximum row**: no diagonal transfer is introduced or removed for that strand.
- **Unavoidable boundary diagonals**: a strand must still enter at its opening index and leave at its closing index; diagonals intrinsic to those transitions are permitted. Only the avoidable up-then-down displacement between them is removed.
- **Interleaved openings and closings at the same indices**: placement remains consistent and non-overlapping.
- **Closings at the very bottom row**: handled without error and without forcing avoidable movement on strands above.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a selectable rendering mode (distinct from the default) that, before placing strands, precalculates for each opening the maximum vertical row the resulting strand will occupy between its opening and its matching closing.
- **FR-002**: System MUST place each opening feature at its precalculated target row rather than always at the lowest free row.
- **FR-003**: In the new mode, a strand that does not change rows between its opening and closing MUST render as a straight horizontal line with no diagonal transfer segments, wherever the precalculated placement makes that possible.
- **FR-004**: In the new mode, the number of diagonal transfer segments MUST be reduced (versus the default mode) for any diagram in which a strand is pushed up by an opening beneath it and later pulled back down by a closing beneath it.
- **FR-005**: System MUST leave the default rendering behavior unchanged; the height-precalculated behavior MUST be opt-in.
- **FR-006**: For every valid diagram, the new mode's rendering MUST represent the same knot as the default mode's rendering (notation fidelity preserved).
- **FR-007**: The new mode MUST support all existing diagram elements — openings, closings, and crossings — and MUST keep each crossing aligned with its correct partner strand.
- **FR-008**: The new mode MUST produce deterministic output: the same diagram always renders identically.
- **FR-009**: The new mode MUST still emit diagonal transfer segments that are intrinsic to a strand entering at its opening index or leaving at its closing index; only avoidable up-then-down movement is removed.
- **FR-010**: The new mode MUST handle empty and degenerate diagrams without error, producing output equivalent to the default mode where no avoidable movement exists.

### Key Entities *(include if feature involves data)*

- **Strand pair**: the two lines introduced by a single opening feature and retired by its matching closing feature; occupies a vertical row that may change over the diagram's width.
- **Opening feature**: the element that introduces a strand pair; the element whose placement row this feature precalculates.
- **Maximum strand row (precalculated height)**: the highest vertical row a given strand pair occupies at any point between its opening and closing — the target placement row for the new mode.
- **Diagonal transfer segment**: a rendered segment that moves a strand up or down between rows; the quantity this feature seeks to reduce.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: For the `terrace` diagram, every strand that currently moves up and then back down renders with no intermediate vertical movement between its opening and closing — the staircase is eliminated.
- **SC-002**: Across the existing example diagrams, no diagram produces more diagonal transfer segments in the new mode than in the default mode, and every diagram that contains an avoidable up-then-down strand movement produces strictly fewer.
- **SC-003**: 100% of valid diagrams render to the same knot in the new mode as in the default mode (verified by round-trip / equivalence checks).
- **SC-004**: Default-mode output remains byte-for-byte identical to current output for every existing example and snapshot.
- **SC-005**: Rendering is deterministic — rendering the same diagram twice in the new mode yields identical output every time.

## Assumptions

- The height-precalculated behavior is an additive, opt-in rendering path; the existing default rendering and its public output remain unchanged (per Constitution: Library-First, and to protect existing `insta` snapshots).
- "Reduce the need for strands to be moved up and down" means minimizing avoidable up-then-down displacement, not necessarily computing a globally optimal placement; a placement at each strand's maximum occupied row is the intended heuristic.
- The ASCII rendering is the target surface for this mode; the abbreviated knot notation remains the source of truth (per Constitution: Notation Fidelity), so example abbreviated-notation inputs and expected rendered outputs will accompany the implementation as snapshot tests.
- Expected outputs for the new mode will be captured via `insta` snapshot tests (per Constitution: Test-First), reviewed and accepted before commit.
- All new code lands in the core `knotty` crate and must compile for `wasm32-unknown-unknown` (per Constitution: WASM-Compatible).
