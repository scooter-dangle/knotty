# Feature Specification: Height-Precalculated Strand Placement (Placement Mode)

**Feature Branch**: `claude/diagram-strand-height-precalc-p4l2lo`

**Created**: 2026-06-18

**Status**: Draft

**Input**: User description: "Diagram rendering mode that precalculates diagram max heights for strands so that it can place the opening features for those strand openings at a diagram height that reduces the need for strands to be moved up and down via diagonals as other strands are opened and closed, respectively, below them in the diagram"

## Clarifications

### Session 2026-09-04

- Q: The new mode can make a diagram taller — measured up to +50% (`(0 (0 )0 (2 )2 )0`: 4 rows → 6; encircled: 12 → 16). Should the spec bound that, minimize it, or accept it? → A: **Accept and track**, mirroring how SC-002 already handles the transfer-count tradeoff. Height growth is an accepted consequence of flat placement: a strand holds its height for its whole flat run, so a divergent pair keeps its gap open even where nothing occupies it, and the grid may span more rows than are ever simultaneously occupied. No bound is claimed and no minimization is required; rendered height is measured and reported per example so the cost is explicit rather than hidden.

### Session 2026-09-03

- Q: Which category do the transfers introduced by the midpoint rule (cap→max, max→cup) belong to? → A: **A third category — boundary transfers** — reported alongside open/close displacement and crossing-alignment. A crossing's return transfers count as crossing-alignment. Worked example `(0 (1 )1 )0`: the default renders 4 displacement glyphs; the new mode renders 0 displacement and 4 boundary glyphs. Folding boundary into displacement would report "4 → 4" and hide that the kind of cost changed; excluding it would report "4 → 0" and claim a win that the picture does not show.
- Q: What counts as one transfer segment for the purpose of comparing modes? → A: **One transfer glyph — every transfer cell in the rendered grid counts.** Five strands each rising one level counts 5; one strand rising two levels counts 2. Chosen because it is countable directly off the grid without tracking strand identity across columns, and because it is the strictest measure: coarser units would hide a regression that displaces more strands, or that splits one two-level climb into two one-level climbs.
- Q: Where do a crossing's two partners meet when precalculated placement has separated them? → A: **At the floored midpoint of their two rows**, extending the cap/cup rule to crossings. Both strands then return to their maxima afterward, since unlike a cap or cup a crossing is not a boundary and both strands survive it. This makes one rule cover every two-strand feature: a cap, cup, or crossing is drawn at the floored midpoint of the two strands it joins, with the movement split evenly between them.
- Q: The midpoint rule makes the lower strand rise to meet a cup, above the maximum used to place it — does that count toward the strand's maximum? → A: **No — the maximum is taken over the strand's flat run only**, from the end of its opening transfer to the start of its closing transfer. Movement to meet a cap or cup never raises the maximum. This removes the self-reference (a maximum that depends on a placement computed from that maximum) and keeps the calculation a single forward pass with no fixpoint, consistent with FR-009 already treating boundary movement as a separate permitted category. *(Superseded in part: "single forward pass" was optimistic — heights are assigned by a longest-path walk over the nesting order, per research R2. The no-fixpoint guarantee this answer establishes is unaffected and is what FR-001 relies on.)*
- Q: A closing draws a cup across two adjacent levels, but the two strands it retires may sit at different heights. Where is the cup drawn? → A: **Mirror the opening rule.** The cup is drawn at the floored midpoint of the two strands' rows at that point, and each strand transfers from its own row to meet it. One concept covers both: a cap or cup sits at the floored midpoint of the strands it joins, and the convergence is split evenly between them.
- Q: A pair's two strands can end up at different maximum heights, but an opening draws a cap across two adjacent levels. At what height is the opening feature drawn? → A: **Halfway between the two strands' maximum heights, floored to a whole row.** Because the opening paren renders halfway up its tile vertically, a computed midpoint of 3.5 is drawn at row 3 — the tile whose center already sits at 3.5. So `opening_row = floor((lower_max + upper_max) / 2)`. This balances the boundary movement between the two strands instead of loading it entirely onto one of them.
- Q: How does this mode relate to the "opening-centered" rendering added by spec 003 and left as the sole rendering by spec 005? → A: They are **orthogonal axes**. Opening-centered governs how an already-placed diagram is mapped onto the 2D character grid (glyph mapping). This feature governs how strand heights are *calculated* — which vertical level each strand occupies — and does not change the subsequent mapping to the grid. Adding a choice on the placement axis therefore does not reintroduce the rendering choice that spec 005 removed. Terminology: this feature adds a **placement mode**, not a rendering mode.

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
  of a straight horizontal line. This feature adds a placement mode that opens
  each strand directly at the highest row it will ever need, so it can run flat.

  Scope note (see Clarifications 2026-09-03): "placement" means deciding which
  vertical level each strand occupies. It is a separate axis from the *grid
  mapping* — how an already-placed diagram becomes characters in a 2D grid —
  which is what the opening-centered rendering governs. This feature changes
  only the former and leaves the latter untouched.

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

A person rendering a knot diagram chooses the height-precalculated placement mode. For each opening, the renderer first determines the maximum vertical row the resulting strand will occupy at any point before it closes, and opens the strand directly at (or near) that row. Strands that previously zig-zagged up and then back down as other strands opened and closed beneath them instead run as straight horizontal lines.

**Why this priority**: This is the core value of the feature — cleaner, easier-to-read diagrams. Without it, nothing else matters. It is independently shippable as a new rendering path.

**Independent Test**: Render a diagram known to produce avoidable up-then-down movement (for example the `terrace` diagram, abbreviated notation `(0 (2 (4 (6 )5 )3 )1 (1 (3 (5 )6 )4 )2 )0`) in the new mode and confirm that strands which previously climbed and descended now render with no intermediate vertical movement between their opening and closing, while the diagram still represents the same knot.

**Acceptance Scenarios**:

1. **Given** a diagram in which a strand opens, is passed over by one or more openings/closings beneath it, and then closes at the same row it opened, **When** rendered in the height-precalculated mode, **Then** that strand is drawn flat (no diagonal transfer segments) for the portion where it was previously displaced up and back down.
2. **Given** the `terrace` diagram, **When** rendered in the height-precalculated mode, **Then** the total number of diagonal transfer segments is lower than in the default mode and the rendered figure decodes to the same knot.
3. **Given** any valid diagram, **When** rendered in the height-precalculated mode, **Then** the figure represents the same knot as the default-mode rendering of that diagram.

---

### User Story 2 - Keep diagram complexity stable under repeated rotation (Priority: P2)

A person repeatedly applies the rotation move to a diagram. Because rotation re-derives the diagram by scanning the rendered grid, transfers that reverse direction (a strand pushed up then later pulled back down) are re-encoded as additional diagram features and accumulate with each rotation, making the diagram progressively more complicated even though the knot is unchanged. (Many transfers scan to nothing and do not inflate the count.) Using the height-precalculated placement mode for the scan removes those avoidable reversed-direction transfers, so the scanned feature count stays stable across rotations.

**Why this priority**: This is the motivating use case for the feature. Without it, repeated rotation inflates the feature count and degrades usability of the rotation move; it is the primary real-world payoff of reduced transfers. It is independently testable against the existing rotation move.

**Independent Test**: Take a diagram, render/scan it for rotation in the new mode, rotate it through a full cycle (e.g., four 90° rotations back to the original orientation), and confirm the number of scanned diagram features does not grow across rotations and the final diagram represents the same knot as the original.

**Acceptance Scenarios**:

1. **Given** a diagram whose default rendering contains reversed-direction (up-then-down) transfers, **When** it is rotated using the height-precalculated rendering for the scan, **Then** the resulting diagram has fewer scanned features than the default-mode rotation and no more than before the rotation.
2. **Given** a diagram, **When** it is rotated through a full cycle back to its original orientation using the new mode, **Then** the scanned feature count does not grow across the cycle and the final diagram represents the same knot as the original.

---

### User Story 3 - Opt in without changing existing output (Priority: P3)

A downstream consumer of the library (or example app) selects a single placement mode to work in — the existing default placement or the new height-precalculated placement — and all subsequent operations run under that active mode. The mode is an operating context, not just a display option: rotation's produced notation depends on it. The grid mapping is unaffected either way. Existing renders, snapshots, and example outputs are unaffected unless the new mode is explicitly chosen.

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
- **Pair whose two strands diverge**: an opening occurring between a pair's two strands raises the upper without raising the lower, so the pair spans non-adjacent rows and its two strands have different maxima. The cap is drawn at the floored midpoint (FR-002) and each strand transfers to its own maximum (FR-015).
- **Odd versus even midpoint**: when the two maxima sum to an odd number the midpoint ends in `.5` and floors onto the tile whose center is exactly that height, so no rounding error is introduced; when the sum is even the midpoint is already a whole row.
- **Interleaved openings and closings at the same indices**: placement remains consistent and non-overlapping.
- **Closings at the very bottom row**: handled without error and without forcing avoidable movement on strands above.
- **Repeated rotation**: rotating a diagram many times using the new mode for the scan must not accumulate features from reversed-direction transfers; the scanned feature count stays bounded by the knot's actual complexity rather than growing per rotation. (Transfers that scan to nothing, and crossing-alignment transfers, do not contribute to growth.)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a selectable placement mode (distinct from the default) that, before placing strands, assigns **each of the two strands an opening introduces** a *height* — the row that strand occupies for its whole flat run. A strand's height MUST be **one greater than the greatest height among all strands ever below it** while both are live, and **zero** when no strand is ever below it. Heights are *assigned*, not observed: a strand's height is generally **not** the row it would occupy under the default placement, and MUST NOT be computed from that rendering. The two strands of one opening MAY receive different heights, because an opening occurring between them raises the upper without raising the lower; each is retired by whichever closing takes it, which is generally not the closing that retires its partner. The flat run excludes the boundary movement by which a strand meets its cap or cup (FR-015, FR-016) — including it would make a height depend on a cap/cup row itself derived from that height.
- **FR-002**: System MUST place each opening feature unconditionally at the midpoint of its two strands' precalculated maxima, floored to a whole row — `floor((lower_max + upper_max) / 2)` — rather than at the lowest free row, and without weighing crossing-alignment cost against displacement savings. Flooring is correct rather than approximate: the opening glyph renders halfway up its tile, so the tile at `floor(m)` already places the cap at height `m` for any midpoint ending in `.5`. **The resulting split is even only when the separation is even.** For two strands at rows `a < b`, separation `g = b − a − 1`: an even `g` splits `g/2` each, while an odd `g` gives the lower strand `(g+1)/2` steps and the upper `(g−1)/2` — the extra step falls on the lower strand, as a consequence of flooring. A separation of 1 therefore moves the lower strand once and the upper not at all.
- **FR-003**: In the new mode, a strand that does not change rows between its opening and closing MUST render as a straight horizontal line with no diagonal transfer segments, wherever the precalculated placement makes that possible.
- **FR-004**: In the new mode, the number of *open/close displacement* transfer segments (those caused by a strand being pushed up by an opening beneath it and later pulled back down by a closing beneath it) MUST be reduced versus the default mode for any diagram exhibiting such displacement.
- **FR-005**: System MUST leave the default rendering behavior unchanged; the height-precalculated behavior MUST be opt-in.
- **FR-006**: For every valid diagram, the new mode's rendering MUST represent the same knot as the default mode's rendering (notation fidelity preserved).
- **FR-007**: The new mode MUST support all existing diagram elements — openings, closings, and crossings — and MUST keep each crossing aligned with its correct partner strand.
- **FR-008**: The new mode MUST produce deterministic output: the same diagram always renders identically.
- **FR-009**: The new mode MUST still emit diagonal transfer segments that are intrinsic to a strand entering at its opening index or leaving at its closing index; only avoidable up-then-down movement is removed.
- **FR-010**: The new mode MUST handle empty and degenerate diagrams without error, producing output equivalent to the default mode where no avoidable movement exists.
- **FR-011**: When a crossing's two participating strands are not on adjacent rows under the precalculated placement, the new mode MUST insert the transfer segments needed to bring them adjacent for the crossing and to restore their placement afterward. The two partners MUST meet at the floored midpoint of their two rows — the same rule as FR-002 and FR-016, including its odd-separation bias toward the lower strand — and because a crossing is not a boundary, both strands MUST return to their own maxima after it. These crossing-alignment transfers are permitted even though they are not open/close displacement transfers, and the rendering MUST never draw a crossing between non-adjacent rows.
- **FR-012**: The placement mode MUST be a single operating context a user selects to work in; all diagram operations run under the active mode. Because rotation re-derives notation from the rendered grid, the rotation result MUST reflect the active mode; notation-only moves (swap, wrap-around, change-crossing, Reidemeister, bulge/collapse) MUST produce identical results regardless of the active mode.
- **FR-013**: The active placement mode MUST default to the placement behavior the current renderer already performs, so that current behavior — including rotation results and snapshots — is unchanged unless the user opts into the new mode. (Making the new mode the default, and any migration away from the existing placement, is out of scope for this feature.)
- **FR-014**: The placement mode MUST be independent of the grid mapping (the opening-centered rendering). Selecting either placement mode MUST NOT change how an already-placed diagram is mapped onto the character grid, and MUST NOT reintroduce a choice on the rendering axis that spec 005 removed.
- **FR-015**: From the opening cap, each of the two strands MUST transfer to its own precalculated maximum where that differs from the cap's placement. These are boundary diagonals permitted by FR-009, and the midpoint rule of FR-002 exists to distribute them evenly between the two strands rather than concentrating them on one. Their count is invariant: bridging a separation costs `upper_max − lower_max − 1` segments wherever the cap is placed between the two strands, so the midpoint choice redistributes the cost without changing it.
- **FR-016**: A closing feature MUST be drawn at the floored midpoint of the two strands it retires — the same rule as FR-002, applied to the strands' rows at that point rather than to their maxima — and each strand MUST transfer from its own row to meet the cup. The general rule is therefore uniform across every two-strand feature: **a cap, cup, or crossing is drawn at the floored midpoint of the two strands it joins, and the movement needed to meet it is split as evenly as the separation allows** (FR-002 openings, FR-016 closings, FR-011 crossings; the odd-separation bias is specified in FR-002). Caps and cups are boundaries, so the strands do not return; after a crossing both strands do return to their maxima.
- **FR-017**: The new mode MUST NOT be required to preserve the rendered diagram height. Because each strand holds its assigned height for its whole flat run, a pair whose strands diverge keeps its gap open even in columns where nothing occupies it, so the grid may span more rows than are ever simultaneously occupied. The rendered height MUST be one more than the greatest assigned height; it is at least the height the default placement requires and MAY exceed it. Implementations MUST NOT size the grid from the count of simultaneously live strands, which under-counts whenever a pair diverges.

### Key Entities *(include if feature involves data)*

- **Strand pair**: the two lines introduced by a single opening feature; each occupies a vertical row that may change over the diagram's width. The two are **not** retired together: a closing retires two adjacent *logical levels*, whose strands generally come from two different openings. In the rotated 5₁ fixture, `)1` retires `(0`'s upper strand together with `(6`'s lower strand, and `)0` retires the remaining two. "Pair" therefore names how two strands are *born*, never how they die.
- **Opening feature**: the element that introduces a strand pair; the element whose placement row this feature precalculates.
- **Strand height** *(canonical term; also written "maximum strand row")*: the row a **single strand** occupies over its **flat run** — from the end of its opening transfer to the start of its closing transfer. Assigned as one greater than the tallest strand ever below it, or zero if none is (FR-001). Boundary movement to meet a cap or cup is excluded by definition, so a height never depends on a placement derived from it. Assigned per strand, so the two strands of one opening may differ.
- **Cap/cup placement row**: the row at which an opening's cap or a closing's cup is drawn — the floored midpoint of the two strands it joins. For an opening the inputs are the two strands' precalculated maxima (FR-002); for a closing they are the two strands' rows at that point (FR-016). Distinct from either strand's own row, and the row each strand transfers to or from (FR-015).
- **Diagonal transfer segment**: one rendered cell that moves a strand up or down between rows. **The unit of measurement is the glyph, not the movement**: a strand rising two levels contributes two segments, and a single opening that displaces five strands at once contributes five. Counting is therefore a direct census of transfer cells in the grid, requiring no tracking of strand identity across columns. Three kinds matter here: *open/close displacement transfers*, caused by openings/closings beneath a passing strand (the kind this feature reduces); *boundary transfers*, moving a strand between a cap or cup and its own maximum; and *crossing-alignment transfers*, bringing two crossing partners together when precalculated placement has separated them. The latter two are costs the default placement never incurs (SC-002).
- **Boundary transfer**: a transfer carrying a strand between a cap or cup and its own maximum, introduced because the midpoint rule places a cap or cup between two strands that sit at different rows (FR-015, FR-016). A cost the default placement never incurs, and counted as its own category in SC-002. Its total for a pair is fixed at `upper_max − lower_max − 1` regardless of where between them the cap is placed — so the midpoint rule of FR-002 distributes this cost evenly between the two strands rather than reducing it.
- **Crossing-alignment transfer**: a transfer the new mode adds to bring two crossing strands together at the floored midpoint of their rows so they are adjacent at the moment they cross, plus the return transfers restoring both to their maxima afterward. Needed because the default placement's guarantee that crossing partners are always adjacent no longer holds once strands sit at their own maximum rows (FR-011).
- **Scanned diagram feature**: an element the rotation move recovers by reading the rendered grid (openings, closings, crossings). Not every transfer becomes one — many scan to nothing. The transfers that inflate this count are (especially, perhaps only) those later reversed by an opposite-direction transfer, i.e. the avoidable up-then-down displacement the new mode removes; crossing-alignment transfers are scanned but do not add features. Reducing the reversed-direction transfers lowers the count without changing the knot, which is what the new mode aims for.
- **Active placement mode (operating context)**: the single mode a user has selected to work in, governing how strand heights are calculated. It governs all operations; rotation's produced notation depends on it, while notation-only moves are independent of it. Defaults to the existing placement behavior; selecting the new mode is opt-in.
- **Rendered diagram height**: one more than the greatest assigned strand height. At least the height the default placement requires, and greater whenever a pair diverges, because a divergent pair holds its gap open for its whole life even where nothing occupies it. **Not** the count of simultaneously live strands, which under-counts in exactly that case (FR-017, SC-007).
- **Grid mapping (separate axis, out of scope)**: how an already-placed diagram is turned into characters in the 2D grid — what the opening-centered rendering governs. Orthogonal to the placement mode and unchanged by this feature (FR-014).

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: For the `terrace` diagram, every strand that currently moves up and then back down renders with no intermediate vertical movement between its opening and closing — the staircase is eliminated.
- **SC-002**: Transfer segments are counted **per glyph** — every transfer cell in the rendered grid, so a strand rising two levels counts twice and one opening displacing five strands counts five times. Segments are classified into **three** categories, each reported separately per example so no tradeoff is hidden:
  1. *open/close displacement* — a passing strand pushed up by an opening beneath it and later pulled back down;
  2. *boundary* — a strand moving between a cap or cup and its own maximum (FR-015, FR-016);
  3. *crossing-alignment* — bringing two crossing partners together and returning them afterward (FR-011).

  The guarantee is on category 1: **open/close displacement segments MUST be strictly reduced** for any diagram exhibiting such displacement, and MUST never increase. Categories 2 and 3 are costs the new mode may introduce, so **the total is not guaranteed to fall**. Worked example: `(0 (0 )0 )0` goes from 8 displacement segments to 0 of any kind, while `(0 (1 )1 )0` — crossing-free, and exhibiting exactly the up-then-down movement this feature targets — goes from 4 displacement segments to 4 boundary segments, for no change in total. Both outcomes are conforming; reporting all three counts is what makes the difference between them visible.
- **SC-003**: 100% of valid diagrams render to the same knot in the new mode as in the default mode (verified by round-trip / equivalence checks).
- **SC-004**: Default-mode output remains byte-for-byte identical to current output for every existing example and snapshot.
- **SC-005**: Rendering is deterministic — rendering the same diagram twice in the new mode yields identical output every time.
- **SC-006**: When a diagram is rotated using the new mode for the scan, the scanned feature count never increases relative to the original (crossing-alignment transfers the mode adds are scanned but do not add features), and for diagrams whose default rendering contains reversed-direction (up-then-down) transfers it is strictly lower than the default-mode rotation. Rotating through a full cycle back to the original orientation yields a feature count no greater than the original, and the diagram still represents the same knot.

  This is the feature's motivating claim but **not an established result** — implementing rotation is what exposed the compounding in the older placement, and this criterion is what establishes whether the new placement removes it. The counts MUST therefore be measured and reported per example rather than merely asserted. Should the claim not hold in full, FR-003 and FR-004 stand independently: reducing displacement transfers is valuable on its own, and failing SC-006 is grounds for revisiting the motivating claim, not for treating the feature as unsuccessful.

- **SC-007**: Rendered diagram height is measured and reported per example, alongside the transfer counts of SC-002 and over the same examples. **No reduction is claimed and no bound is set** — growth is an accepted consequence of flat placement (FR-017). Every example whose height differs from the default placement's is reported with both figures, so the cost is explicit rather than hidden. Measured at time of writing: unchanged for `rotated-5_1` (8), `square-knot` (6) and `non-adjacent-crossing` (6); `little-dumb-link` 6 → 8; `square-knot-links-encircled` 12 → 16; `(0 (0 )0 (2 )2 )0` 4 → 6.

## Assumptions

- The height-precalculated behavior is an additive, opt-in rendering path; the existing default rendering and its public output remain unchanged (per Constitution: Library-First, and to protect existing `insta` snapshots).
- "Reduce the need for strands to be moved up and down" means minimizing avoidable open/close displacement, not necessarily computing a globally optimal placement; a placement at each strand's maximum occupied row is the intended heuristic.
- The default mode's invariant that any two crossing strands are always vertically adjacent at a uniform distance does not survive max-row placement. The new mode therefore accepts crossing-alignment transfers (FR-011) as a tradeoff: it optimizes for fewer open/close displacement transfers, not for the global minimum of all transfers, and a crossing-heavy diagram could net out with a similar or larger total transfer count. The same is true of **crossing-free** diagrams whose pairs diverge: the boundary transfers of FR-015/FR-016 can exactly offset the displacement removed (worked example in SC-002), so a reduction in the total is not claimed for any diagram class — only a reduction in open/close displacement. The same stance applies to **vertical space**: flat placement holds a divergent pair's gap open for the pair's whole life, so the diagram can need more rows than the default placement (FR-017). Both costs are tracked and reported rather than bounded (SC-002, SC-007). Per the 2026-06-18 clarification, placement is unconditional (max row always) — the heuristic does not weigh crossing-alignment cost against displacement savings; that refinement is explicitly out of scope for this feature.
- The ASCII rendering is the target surface for this mode; the abbreviated knot notation remains the source of truth (per Constitution: Notation Fidelity), so example abbreviated-notation inputs and expected rendered outputs will accompany the implementation as snapshot tests.
- Expected outputs for the new mode will be captured via `insta` snapshot tests (per Constitution: Test-First), reviewed and accepted before commit.
- The rotation move (re-deriving notation by scanning the rendered grid) is the motivating consumer; the rotation algorithm itself is unchanged, but because it scans the rendered grid the notation it produces depends on the active mode. The two modes can therefore yield different — but equivalent — rotation results, which is why a working context fixes one mode.
- A working context (a diagram and the sequence of operations applied to it) uses a single active mode consistently. Mixing modes within one operation sequence is not a supported scenario; results are only defined relative to the active mode.
- Migration stance: the legacy mode is trusted and remains the default; the new mode is opt-in until validated in real use. Promoting the new mode to default (and any deprecation of the legacy mode) is explicitly out of scope for this feature.
- All new code lands in the core `knotty` crate and must compile for `wasm32-unknown-unknown` (per Constitution: WASM-Compatible).
