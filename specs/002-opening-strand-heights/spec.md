# Feature Specification: Initial Strand Heights at Opening Features

**Feature Branch**: `claude/strand-opening-height-calc-1t2y0y`

**Created**: 2026-08-08

**Status**: Draft

**Input**: User description: "The important thing to start with is calculating the starting heights of the stands at each diagram opening feature. Important: each opening feature has two stands…the opening feature itself ends up having an index half way between the two strands' initial heights, with 0.5 subtracted. In the current diagram mode, strands always start adjacent to each other. With the new feature, though, it's important to calculate the individual initial strand heights. The strand opening feature's height can be derived from the two heights of the strands it introduces."

## Context

This is the first, foundational slice of the height-precalculated strand
placement rendering mode specified in `specs/001-strand-height-precalc/`.
That feature describes the end goal (open each strand at the highest row it
will ever need, so passing strands run flat). This feature specifies only the
calculation that everything else in that mode depends on: **for each opening
feature, what are the two initial heights of the two strands it introduces, and
what height does the opening feature itself therefore sit at.**

Nothing here changes what is rendered. It defines the numbers the renderer will
later be driven by, and it defines them in a way that is verifiable on its own
against today's behavior.

### Domain background

<!--
  A diagram is drawn as a grid of horizontal rows. Row numbers increase in one
  consistent direction; "height" and "row" are used interchangeably below, and
  "index" is the notation's name for the same quantity.

  Every opening feature introduces exactly TWO strands (the two sides of the
  loop it opens); the matching closing feature retires both. Today's renderer
  always places those two strands on adjacent rows h and h+1, and the opening
  feature's index is simply h — the lower of the two.

  Once strands may open at their precalculated maximum rows, the two strands of
  one opening are no longer necessarily adjacent: rows between them can be
  reserved for strands that will open nested inside this pair later on. The
  pair's heights must then be computed individually, and the feature's own
  height derived from them.
-->

The derivation the user states, and which this feature makes explicit:

```text
opening_feature_height = (strand_height_a + strand_height_b) / 2 - 0.5
```

This is the midpoint of the two strand heights, shifted down by half a row. It
is a strict generalization of today's rule: with adjacent strands at `h` and
`h+1` it yields `(h + h + 1) / 2 - 0.5 = h`, exactly the index today's
notation carries. That equivalence is the compatibility anchor for the whole
feature.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compute both initial strand heights for every opening (Priority: P1)

A person (or the renderer acting on their behalf) takes a diagram and asks, for
each opening feature in it, where its two strands start. The answer is two
individual heights — one per strand — computed from how high each of those two
strands will need to rise before its closing feature retires it. Under the
current adjacent-placement rule the two heights come back adjacent; under
precalculated placement they may be separated by rows reserved for strands that
open nested inside the pair.

**Why this priority**: Every other part of the height-precalculated mode
consumes these numbers. Without a correct per-strand initial height there is
nothing to place strands at, nothing to derive the feature height from, and no
way to tell whether a strand can run flat. It is independently shippable and
independently checkable — the heights can be inspected and asserted without
changing a single rendered character.

**Independent Test**: For each example diagram in the repository, request the
pair of initial strand heights for every opening feature and confirm the pairs
are well-formed (two distinct heights, neither shared with any other strand
alive at that moment) and that under the current adjacent-placement rule each
pair comes back as `(h, h+1)` matching today's placement.

**Acceptance Scenarios**:

1. **Given** any valid diagram, **When** initial strand heights are requested, **Then** every opening feature reports exactly two heights, one for each strand it introduces.
2. **Given** a diagram under the current adjacent-placement rule, **When** initial strand heights are requested, **Then** every opening's two heights are adjacent and equal to `(h, h+1)` where `h` is the index that opening carries in the abbreviated notation.
3. **Given** a diagram under precalculated placement, **When** initial strand heights are requested, **Then** each strand's initial height equals the highest row that strand occupies at any point between its opening and its matching closing.
4. **Given** the same diagram twice, **When** initial strand heights are requested each time, **Then** the results are identical (the calculation is deterministic).
5. **Given** an empty diagram, **When** initial strand heights are requested, **Then** an empty result is returned and no error is raised.

---

### User Story 2 - Derive the opening feature's own height from its strand pair (Priority: P2)

Having the two strand heights, a person asks where the opening feature itself
sits. It sits half way between its two strands, less half a row — the midpoint
of the pair shifted down by 0.5. For adjacent strands this reduces to the lower
strand's height, which is precisely the index the feature carries today; for
separated strands it lands in the middle of the block of rows the pair spans.

**Why this priority**: This is the quantity the renderer and the notation
actually address the feature by. It is a pure derivation from Story 1's output,
so it is only meaningful once those heights exist — but it is what makes them
usable, and it is the point where compatibility with today's notation is
provable.

**Independent Test**: For every opening feature in every example diagram, apply
the derivation to the pair of initial strand heights and confirm the result is
a whole number, and that under adjacent placement it equals the opening's
existing notation index for 100% of openings.

**Acceptance Scenarios**:

1. **Given** an opening feature whose two strands start at heights `a` and `b`, **When** the feature's height is derived, **Then** the result is `(a + b) / 2 - 0.5`.
2. **Given** an opening whose strands start adjacent at `h` and `h+1`, **When** the feature's height is derived, **Then** the result is exactly `h`, matching the index carried in the abbreviated notation today.
3. **Given** an opening whose strands start separated (for example at heights 2 and 5), **When** the feature's height is derived, **Then** the result is the whole number in the middle of the spanned block (here, 3).
4. **Given** any valid diagram, **When** every opening's height is derived, **Then** every result is a whole number — never a fractional row.

---

### User Story 3 - Today's diagrams keep today's numbers (Priority: P3)

A person working in the existing rendering mode sees no change whatsoever. The
new calculation, applied to adjacent placement, reproduces the existing indices
exactly, so existing renders, snapshots, and downstream expectations are
untouched.

**Why this priority**: Protects existing behavior. The calculation is only
trustworthy if it can be shown to reproduce the mode that is already trusted;
this is the safety net that lets precalculated placement be built on top of it.

**Independent Test**: Run every existing example and snapshot in the current
mode and confirm the output is byte-for-byte identical to today's, and that
each opening's derived height equals its original notation index.

**Acceptance Scenarios**:

1. **Given** any existing example diagram, **When** it is rendered in the current mode, **Then** the output is byte-for-byte identical to today's output.
2. **Given** any existing example diagram, **When** each opening's height is derived from its strand pair, **Then** the derived height equals the opening's original notation index.

---

### User Story 4 - Inconsistent placements are reported, not papered over (Priority: P4)

A person supplies or produces a placement in which an opening's two strand
heights do not describe a valid pair — the heights are equal, or the number of
rows between them is odd so the midpoint falls between rows. Rather than
rounding to the nearest row and silently producing a wrong diagram, the
calculation reports the inconsistency.

**Why this priority**: The whole-number invariant is what makes the derivation
safe to build on. Detecting a violation turns a class of silent rendering
corruption into an immediate, localized failure. It is last because it is a
guard on the mechanism rather than the mechanism itself.

**Independent Test**: Present a pair of initial heights whose separation would
place the feature between rows (for example heights 2 and 4), and confirm the
calculation reports an inconsistency identifying the offending opening rather
than returning a rounded height.

**Acceptance Scenarios**:

1. **Given** an opening whose two strand heights are equal, **When** its height is derived, **Then** an inconsistency is reported identifying that opening.
2. **Given** an opening whose two strand heights are separated by an odd number of rows (an even number of rows between them), **When** its height is derived, **Then** an inconsistency is reported rather than a rounded height.
3. **Given** an inconsistency is reported, **When** the report is read, **Then** it identifies which opening feature is affected and what the two offending heights were.

---

### Edge Cases

- **Empty diagram**: no openings, so an empty set of heights; no error.
- **Single opening/closing pair with nothing else in the diagram**: the pair's two strands start at the two lowest rows and the derived feature height is the lower of them — identical in both placement rules.
- **Opening whose pair is never nested into**: no rows are ever reserved between its two strands, so they start adjacent and the derived height matches today's index exactly.
- **Deeply nested openings**: an outer pair's two strands are separated by all the rows reserved for the pairs nested inside it; because nested strands always arrive two at a time, that separation is always odd and the derived height is always a whole number.
- **Pair pushed up as a whole by an opening beneath it**: both strands rise by the same amount, so their separation — and therefore the derived feature height's position relative to them — is preserved.
- **Two strands of one opening whose maximum rows are reached at different points in the diagram**: each strand's initial height is its own maximum, computed independently; they are not forced to move together beyond what keeps the pair well-formed.
- **Separation that would place the feature between rows**: reported as an inconsistency (User Story 4), never rounded.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST compute, for every opening feature in a diagram, two individual initial strand heights — one for each of the two strands that opening introduces.
- **FR-002**: Under the current adjacent-placement rule, the two computed heights for an opening MUST be `h` and `h+1`, where `h` is the index that opening carries in the abbreviated notation.
- **FR-003**: Under precalculated placement, each strand's initial height MUST be the highest row that strand occupies at any point between its opening feature and its matching closing feature.
- **FR-004**: System MUST derive an opening feature's own height from its two strand heights as the midpoint of the two, less one half row: `(a + b) / 2 - 0.5`.
- **FR-005**: The derivation in FR-004 MUST reproduce the existing notation index exactly for every opening whose two strands are adjacent.
- **FR-006**: For every valid placement, the derived opening height MUST be a whole row number; the system MUST NOT round a fractional result to a neighboring row.
- **FR-007**: System MUST report an inconsistency, identifying the affected opening feature and its two heights, when a pair of initial strand heights does not yield a whole-number feature height, or when the two heights are equal.
- **FR-008**: The calculation MUST be deterministic — the same diagram always yields the same heights.
- **FR-009**: The calculation MUST handle empty and single-pair diagrams without error.
- **FR-010**: The calculation MUST NOT change any currently rendered output; it is an additional derivation available alongside existing behavior.
- **FR-011**: The computed heights MUST be available per opening feature in a form the rendering mode of `001-strand-height-precalc` can consume for strand placement.
- **FR-012**: Within a single placement, no two strands that are alive at the same point in the diagram may be assigned the same height; the calculation MUST NOT produce a placement that violates this.

### Key Entities *(include if feature involves data)*

- **Strand**: one of the two lines introduced by an opening feature and retired by its matching closing feature. It occupies one row at any given point in the diagram.
- **Strand pair**: the two strands introduced by a single opening feature. Always exactly two; always opened together and closed together.
- **Initial strand height**: the row a strand starts on at the moment its opening feature is drawn. Under adjacent placement this is dictated by the feature's notation index; under precalculated placement it is the strand's maximum occupied row over its lifetime.
- **Opening feature height (derived index)**: the row the opening feature itself sits at, derived from its pair as the midpoint less half a row. Equals the lower strand's height when the pair is adjacent.
- **Pair separation**: the difference between an opening's two initial strand heights. Always 1 under adjacent placement; always an odd number under any valid precalculated placement, because rows between the pair are reserved by nested pairs, which arrive two at a time.
- **Placement rule**: the convention that decides initial strand heights — the current adjacent rule, or the precalculated maximum-row rule from `001-strand-height-precalc`. The derivation in FR-004 is common to both.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: For 100% of opening features across all existing example diagrams, computing the pair of initial strand heights under adjacent placement and applying the derivation reproduces the opening's original notation index.
- **SC-002**: For 100% of opening features in any valid placement, the derived feature height is a whole row number.
- **SC-003**: For the `terrace` diagram under precalculated placement, every opening reports two initial strand heights equal to the respective strands' maximum occupied rows, and each strand whose maximum equals its opening row is identified as able to run flat.
- **SC-004**: Rendered output for every existing example and snapshot is byte-for-byte identical to today's output.
- **SC-005**: The calculation is deterministic — running it twice on the same diagram yields identical heights every time.
- **SC-006**: 100% of inconsistent height pairs (equal heights, or a separation that would place the feature between rows) are reported with the offending opening identified; none are silently rounded.
- **SC-007**: Every opening feature in every valid diagram, including empty and single-pair diagrams, is handled without error.

## Assumptions

- This feature is scoped to the *calculation* of initial strand heights and the derived opening-feature height. Actually placing strands at those heights, emitting or removing transfer segments, and the rendering-mode selection itself belong to `001-strand-height-precalc` and are out of scope here.
- "The highest row a strand occupies" is measured over the strand's lifetime — from its opening feature to its matching closing feature — consistent with the maximum-row placement heuristic already specified in `001-strand-height-precalc` (FR-001/FR-002 there). Placement is unconditional maximum-row; this feature does not weigh alternatives.
- Row numbers increase in one consistent direction throughout, matching the existing renderer's convention. "Height", "row", and "index" all refer to the same quantity; the user's phrasing "index half way between … with 0.5 subtracted" is captured verbatim as FR-004.
- The whole-number guarantee (FR-006) rests on nesting: any row between an opening's two strands is reserved by a pair nested inside it, and pairs supply two rows each, so the separation is always odd. A fractional result therefore indicates a defective placement, which is why FR-007 reports rather than rounds.
- The abbreviated knot notation remains the source of truth (per Constitution: Notation Fidelity). Under precalculated placement an opening's derived grid height may differ from the index it carries in the notation; reconciling the two when re-deriving notation from a rendered grid is the rotation/scan concern owned by `001-strand-height-precalc`, not this feature.
- Verification is by unit tests over the existing example diagrams, plus `insta` snapshots where rendered output is involved (per Constitution: Test-First), reviewed and accepted before commit.
- The calculation lands in the core `knotty` crate and must compile for `wasm32-unknown-unknown` (per Constitution: WASM-Compatible), with no new dependencies (per Constitution: Minimal Dependencies).
