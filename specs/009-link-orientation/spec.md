# Feature Specification: Link Orientation

**Feature Branch**: `claude/link-orientation-knotty-ec2xy8`

**Created**: 2026-09-24

**Status**: Draft

**Input**: User description (verbatim):

```text
Link orientation: given a well-formed diagram encoding in knotty's format, return the following:
1. What the opening feature is of each individually knot in the link and
At every feature in the diagram:
2. which of the opening features from (1) each strand in the feature is connected to, and
3. for each strand in the feature, whether the direction/orientation of the strand 'matches' the direction/orientation the the feature from (2) or is the opposite direction/orientation.

For example, take the unknot: `(0 )0`
Output:
1. `vec![0]` (the first and only knot component starts at the 0th index in the encoded diagram)
2. `vec![0, 0]` (both encoded diagram features are connected to the 0th knot identified in (1))
3. `vec![(Pos, Neg), (Pos, Neg)]` (the top and bottom strands of the first feature match and are opposite, respectively, to the 'positive' direction of the first feature…the second feature, closing the knot, has a top strand matching the first feature's top strand and bottom strand matching the first feature's bottom strand)

Example, the unknot with one twist: `(0 /0 )0`
Output:
1. `vec![0]` (the first and only knot of the link starts at the 0th index in the encoded diagram)
2. `vec![0, (0, 0), 0]` (all three encoded diagram features are connected to the 0th knot identified in (1) (including both the top and bottom strands of the crossing feature…might need an enum or tuple with second item behind optional to express this)
3. `vec![(Pos, Neg), (Pos, Neg), (Neg, Pos)]` (in the crossing, the strands start as before, but then they are swapped so that, coming into the closing feature, they are now opposite their order in the closing feature)

Unconnected link example: `(0 (0 )0 )0`
Output:
1. `vec![0, 1]` (two separate knot components with orientations independent of each other)
2. `vec![0, 1, 1, 0]`
3. `vec![(Pos, Neg), (Pos, Neg), (Pos, Neg), (Pos, Neg)]`

Simple connected link: `(0 (1 /0 /2 )1 )0`
Output:
1. `vec![0, 1]` (still two separate knot components with orientations independent of each other)
2. `vec![0, 1, (1, 0), (0, 1), 0, 1]`
3. `vec![(Pos, Neg), (Pos, Neg), (Neg, Neg), (Pos, Pos), (Pos, Neg), (Pos, Neg)]`

Trefoil: `(0 (2 /1 \0 /1 )2 )0`
Output:
1. `vec![0]`
2. `vec![0, 0, (0, 0), (0, 0), (0, 0), 0, 0]`
3. `vec![(Pos, Neg), (Pos, Neg), (Neg, Pos), (Neg, Neg), (Pos, Neg), (Pos, Neg), (Pos, Neg)]`

Other unkot 1: `(0 (2 /2 )1 )0`
Output:
1. `vec![0]`
2. `vec![0, 0, (0, 0), 0, 0]`
3. `vec![(Pos, Neg), (Neg, Pos), (Neg, Pos), (Neg, Pos), (Pos, Neg)]`

Logical correctness is paramount. The motivation for this feature is the ability to calculate knot invariants from encoded diagrams, so it will be worse than useless unless it's 100% correct.
```

## User Scenarios & Testing *(mandatory)*

**Background.** A knotty diagram is written as a left-to-right sequence of
*features*. An opening `(k` starts two new strands, a closing `)k` joins two
strands and ends them, and a crossing `/k` or `\k` makes two neighbouring
strands swap places. The feature's number `k` says which strands it affects,
counting positions from 0 at the bottom. Following the strands from feature
to feature traces out one or more closed loops, the *components* of the link.
Knot and link invariants such as writhe, linking number and the Jones
polynomial need an **orientation**: each component gets a direction of
travel, and at every crossing you need to know which component each strand
belongs to and which way it runs. The notation doesn't record any of this.
This feature derives it, deterministically, from the notation alone. The
precise meaning of every term and output is fixed under
[Definitions](#definitions), and the expected results are fixed by the
[Worked Examples](#worked-examples-normative), W1–W13 and M1–M7.

### User Story 1 - Identify the components of a link diagram (Priority: P1)

A person computing invariants from a diagram asks which separate loops the
diagram contains and where each one begins (output 1). For every feature,
they also get the loop(s) its strands belong to (output 2).

**Why this priority**: Every invariant computation starts with the
components. Telling a component's self-crossings apart from crossings
between two components depends on it, and it is useful by itself (for
example, counting components or finding split pieces).

**Independent Test**: Request orientation for W1–W13 and compare outputs (1)
and (2) with the listed values.

**Acceptance Scenarios**:

1. **Given** the unknot W1 `(0 )0`, **When** orientation is requested, **Then** output (1) is `[0]` and output (2) is `[0, 0]`.
2. **Given** the trefoil W5 `(0 (2 /1 \0 /1 )2 )0`, which contains two openings, **When** orientation is requested, **Then** output (1) is `[0]`. The second opening lies on the same loop, so it is not a separate reference opening.
3. **Given** the linked pair W4 `(0 (1 /0 /2 )1 )0`, **When** orientation is requested, **Then** output (1) is `[0, 1]` and the two crossings report components `(1, 0)` and `(0, 1)`.
4. **Given** two side-by-side unknots W7 `(0 )0 (0 )0`, **When** orientation is requested, **Then** output (1) is `[0, 2]` and output (2) is `[0, 0, 1, 1]`. The entries of output (2) are component numbers (positions within output 1), not sequence positions.
5. **Given** W12 `(0 (2 )0 (2 )2 (0 )1 )0`, whose components interleave, **When** orientation is requested, **Then** output (2) is `[0, 1, 0, 2, 2, 1, 1, 1]`.

---

### User Story 2 - Orient every strand at every feature (Priority: P2)

For every strand at every feature, the same person needs to know whether it
runs with its component's reference direction (Pos) or against it (Neg)
(output 3). That is what lets a crossing's sign be worked out.

**Why this priority**: Crossing signs, and so writhe, linking numbers and
the invariants built on them, need a direction for both strands at every
crossing. This output builds on the components from User Story 1.

**Independent Test**: Request orientation for W1–W13 and compare output (3)
with the listed values. Then check the structural properties FR-008 and
FR-009 on every well-formed diagram of up to 8 features.

**Acceptance Scenarios**:

1. **Given** the twisted unknot W2 `(0 /0 )0`, **When** orientation is requested, **Then** output (3) is `[(Pos, Neg), (Pos, Neg), (Neg, Pos)]`. The twist exchanges which strand arrives on top at the closing.
2. **Given** W4 `(0 (1 /0 /2 )1 )0`, **When** orientation is requested, **Then** its crossings read `(Neg, Neg)` and `(Pos, Pos)`, so both strands of a crossing can run the same way.
3. **Given** W6 `(0 (2 /2 )1 )0`, **When** orientation is requested, **Then** its second opening reads `(Neg, Pos)`, because an opening other than the reference opening can be traversed "backwards".
4. **Given** W2 `(0 /0 )0` and W9 `(0 \0 )0`, which differ only in which strand passes over, **When** orientation is requested for each, **Then** the outputs are identical.
5. **Given** any well-formed diagram, **When** orientation is requested, **Then** every reference opening reads `(Pos, Neg)`, and every opening and closing has exactly one `Pos` strand and one `Neg` strand.

---

### User Story 3 - Refuse malformed diagrams instead of answering wrongly (Priority: P3)

The notation parser accepts some sequences that don't describe a diagram:
features that refer to strand positions that don't exist, or strands left
open at the end. A wrong orientation is worse than none. So for a malformed
diagram, the person gets a clear error saying where and why, and never an
output or a crash.

**Why this priority**: Diagrams reach this feature from hand-typed input and
from the results of moves. A silent wrong answer would corrupt every
invariant computed from it.

**Independent Test**: Request orientation for M1–M7 and for the empty diagram.

**Acceptance Scenarios**:

1. **Given** any of M1–M7, **When** orientation is requested, **Then** an error is reported that identifies the listed feature (or the end of the diagram) and reason, and no output is produced.
2. **Given** the empty diagram (no features), **When** orientation is requested, **Then** all three outputs are empty (zero components) and no error is reported.

---

### User Story 4 - Trust the output for invariant calculations (Priority: P4)

The person combines the output with each crossing's over/under information
to compute crossing signs, and from those, linking numbers and writhe. The
values match what is known about the example links. They also don't change
when the diagram is transformed by a move that doesn't change the link.

**Why this priority**: This is the reason the feature exists. It is also the
strongest check of correctness that doesn't depend on how the orientation
was derived.

**Independent Test**: Compute crossing signs from the output for the worked
examples and check them against known values. Then apply every available
move to a corpus of diagrams and check that the derived invariants are
unchanged (SC-003).

**Acceptance Scenarios**:

1. **Given** the linked pair W4, **When** its two crossing signs are computed from the output, **Then** they are equal, so the linking number has magnitude 1.
2. **Given** W11 `(0 (2 /1 \1 )2 )0`, where two loops cross twice without linking, **When** its crossing signs are computed, **Then** they are opposite, so the linking number is 0.
3. **Given** the trefoil W5, **When** its crossing signs are computed, **Then** all three are equal.
4. **Given** any well-formed diagram and any available move other than changing a crossing, **When** orientation is requested before and after the move, **Then** the derived invariants listed in SC-003 are equal.

---

### Edge Cases

- **Empty diagram**: the result is three empty outputs, not an error (FR-012).
- **Split diagrams**, where the strand count returns to zero partway through (W7, W10): each piece gets its own components, and numbering continues across pieces.
- **Stacked or nested components with no crossings** (W3, W8, W12): each is its own component. A component's features need not be contiguous in the sequence, and components can interleave (W12).
- **A component with several openings** (W5, W6, W12, W13): only the first is its reference opening. The others may read `(Pos, Neg)` or `(Neg, Pos)`.
- **Closings that read `(Neg, Pos)`** occur even in diagrams with no crossings (W12 at sequence position 6).
- **Self-crossings and crossings between components**: a crossing's two entries in output (2) are equal in the first case (W5) and different in the second (W4).
- **Both strands of a crossing running the same way**, `(Pos, Pos)` or `(Neg, Neg)`, is normal (W4, W13).
- **Over/under**: reversing any crossing never changes any output (FR-007).
- **Diagrams produced by moves** (including rotation) follow exactly the same rules as hand-written ones. Nothing is special-cased.
- **Malformed input**: an out-of-range opening index, a closing or crossing that needs a strand position that doesn't exist, or strands left open at the end (M1–M7). The first problem in sequence order is the one reported (FR-011).
- **`bad_diagram` repository sample** (W12): despite its name, it is well-formed under this spec's definition and is oriented normally.
- **Very large diagrams**: see SC-006.

## Requirements *(mandatory)*

### Definitions

- **Feature**: one item of the abbreviated notation: an opening `(k`, a closing `)k`, or a crossing `/k` or `\k`, where `k` is the feature's vertical index. A **diagram** is a sequence of features read left to right. A feature's **sequence position** is its 0-based place in that sequence.
- **Strand positions**: between two neighbouring features, some number of strands run side by side. Their positions are numbered from 0 at the bottom, matching the library's rendering. No strands are present before the first feature.
- **What each feature does** (when `h` strands are present just before it):
  - **Opening `(k`** (requires `k ≤ h`): two new strands begin at the feature, joined to each other at its left. They occupy positions `k` (the opening's **lower** strand) and `k+1` (its **upper** strand) just after it. Strands previously at position `k` or above move up by two.
  - **Closing `)k`** (requires `k+1 < h`): the strands at positions `k` (the closing's **lower** strand) and `k+1` (its **upper** strand) are joined to each other and end. Strands previously above them move down by two.
  - **Crossing `/k` or `\k`** (requires `k+1 < h`): the strands at positions `k` and `k+1` swap places. The crossing's **upper** strand is the one at position `k+1` *before* the crossing; it continues at position `k` after it. Its **lower** strand is the one at position `k` before the crossing; it continues at position `k+1` after it. Upper and lower are fixed by the left-hand (before) side. Which strand passes over (`/` or `\`) has no part in this feature.
  - So every feature has exactly two strands, an upper one and a lower one.
- **Well-formed diagram**: every feature meets its requirement above, given the strands present just before it, and no strands remain after the last feature. The empty diagram is well-formed.
- **Component**: one closed loop of the diagram. Start from any strand and follow it: continue straight through each crossing as described above, and at an opening or closing turn back onto that feature's other strand, until the loop closes. Every component has at least one opening, at its leftmost point.
- **Reference opening** (of a component): the opening on that component with the smallest sequence position.
- **Component number**: components are numbered 0, 1, 2, … in increasing order of their reference openings' sequence positions.
- **Reference direction** (of a component): the direction of travel around the loop that leaves the reference opening along its upper strand, moving rightward (towards later sequence positions).
- **Strand direction** (of a feature's upper or lower strand): **Pos** ("matches") if travelling the strand's component in its reference direction moves rightward along that strand. **Neg** ("opposite") if it moves leftward. Pos therefore means "running the same way the reference opening's upper strand leaves it". A reference opening always reads `(Pos, Neg)`.
- **The three outputs** for a well-formed diagram:
  1. **Reference openings**: the sequence position of each component's reference opening, listed in component-number order (so the list is strictly increasing).
  2. **Feature components**: one entry per feature, in sequence order. An opening or closing has a single component number: its two strands are joined to each other, so they always share a component. A crossing has an ordered pair: (component number of its upper strand, component number of its lower strand).
  3. **Feature directions**: one entry per feature, in sequence order: (direction of its upper strand, direction of its lower strand).

### Worked Examples *(normative)*

W1–W6 are the examples from the feature request, reproduced unchanged. W7–W13
were added to pin down cases those examples leave open. Each was worked by
hand and cross-checked against an independent model of the definitions above.
Lists use `[…]`. A crossing's entries are written `(upper, lower)`.

```text
W1  (0 )0                                    unknot
    (1) [0]
    (2) [0, 0]
    (3) [(Pos, Neg), (Pos, Neg)]

W2  (0 /0 )0                                 unknot with one twist
    (1) [0]
    (2) [0, (0, 0), 0]
    (3) [(Pos, Neg), (Pos, Neg), (Neg, Pos)]

W3  (0 (0 )0 )0                              two unlinked loops, stacked
    (1) [0, 1]
    (2) [0, 1, 1, 0]
    (3) [(Pos, Neg), (Pos, Neg), (Pos, Neg), (Pos, Neg)]

W4  (0 (1 /0 /2 )1 )0                        two linked loops
    (1) [0, 1]
    (2) [0, 1, (1, 0), (0, 1), 0, 1]
    (3) [(Pos, Neg), (Pos, Neg), (Neg, Neg), (Pos, Pos), (Pos, Neg), (Pos, Neg)]

W5  (0 (2 /1 \0 /1 )2 )0                     trefoil
    (1) [0]
    (2) [0, 0, (0, 0), (0, 0), (0, 0), 0, 0]
    (3) [(Pos, Neg), (Pos, Neg), (Neg, Pos), (Neg, Neg), (Pos, Neg), (Pos, Neg), (Pos, Neg)]

W6  (0 (2 /2 )1 )0                           unknot, second opening traversed backwards
    (1) [0]
    (2) [0, 0, (0, 0), 0, 0]
    (3) [(Pos, Neg), (Neg, Pos), (Neg, Pos), (Neg, Pos), (Pos, Neg)]

W7  (0 )0 (0 )0                              split: component numbers are not sequence positions
    (1) [0, 2]
    (2) [0, 0, 1, 1]
    (3) [(Pos, Neg), (Pos, Neg), (Pos, Neg), (Pos, Neg)]

W8  (0 (2 )2 (0 )0 )0                        three components
    (1) [0, 1, 3]
    (2) [0, 1, 1, 2, 2, 0]
    (3) [(Pos, Neg), (Pos, Neg), (Pos, Neg), (Pos, Neg), (Pos, Neg), (Pos, Neg)]

W9  (0 \0 )0                                 W2 with the crossing reversed: identical output
    (1) [0]
    (2) [0, (0, 0), 0]
    (3) [(Pos, Neg), (Pos, Neg), (Neg, Pos)]

W10 (0 )0 (0 (1 /0 /2 )1 )0                  crossing entries are component numbers
    (1) [0, 2, 3]
    (2) [0, 0, 1, 2, (2, 1), (1, 2), 1, 2]
    (3) [(Pos, Neg), (Pos, Neg), (Pos, Neg), (Pos, Neg), (Neg, Neg), (Pos, Pos), (Pos, Neg), (Pos, Neg)]

W11 (0 (2 /1 \1 )2 )0                        two loops crossing twice, unlinked
    (1) [0, 1]
    (2) [0, 1, (1, 0), (0, 1), 1, 0]
    (3) [(Pos, Neg), (Pos, Neg), (Neg, Pos), (Pos, Neg), (Pos, Neg), (Pos, Neg)]

W12 (0 (2 )0 (2 )2 (0 )1 )0                  "bad_diagram" sample: interleaved components
    (1) [0, 1, 3]
    (2) [0, 1, 0, 2, 2, 1, 1, 1]
    (3) [(Pos, Neg), (Pos, Neg), (Pos, Neg), (Pos, Neg), (Pos, Neg), (Pos, Neg), (Neg, Pos), (Pos, Neg)]

W13 (0 (2 \1 (3 /2 /4 )3 \1 )2 )0            README diagram: one component, three openings
    (1) [0]
    (2) [0, 0, (0, 0), 0, (0, 0), (0, 0), 0, (0, 0), 0, 0]
    (3) [(Pos, Neg), (Neg, Pos), (Pos, Pos), (Neg, Pos), (Pos, Pos), (Neg, Neg), (Neg, Pos), (Pos, Pos), (Neg, Pos), (Pos, Neg)]
```

Malformed diagrams must be rejected. Each error must identify the listed
sequence position (or the end of the diagram) and convey the listed reason.
The exact wording is not prescribed.

```text
M1  (0            end of diagram: 2 strands are still open
M2  )0            position 0: closing needs strands at positions 0 and 1; 0 strands present
M3  (0 )1         position 1: closing needs strands at positions 1 and 2; 2 strands present
M4  (1 )1         position 0: opening index 1 exceeds the 0 strands present
M5  (0 /1 )0      position 1: crossing needs strands at positions 1 and 2; 2 strands present
M6  (0 )0 )0      position 2: closing needs strands at positions 0 and 1; 0 strands present
M7  (0 (3 )0 )0   position 1: opening index 3 exceeds the 2 strands present
```

### Functional Requirements

- **FR-001**: The core library MUST produce the three outputs defined under Definitions (reference openings, feature components, feature directions) for any well-formed diagram, all from a single request.
- **FR-002**: Two feature strands MUST receive the same component number if and only if they lie on the same closed loop.
- **FR-003**: Output (1) MUST contain exactly one entry per component: the sequence position of that component's reference opening, in strictly increasing order. A component's number is the position of its entry within output (1).
- **FR-004**: Output (2) MUST contain exactly one entry per feature, in sequence order. An opening's or closing's entry carries one component number. A crossing's entry carries two, ordered (upper strand, lower strand).
- **FR-005**: Output (3) MUST contain exactly one entry per feature, in sequence order, giving the directions of its (upper strand, lower strand), each either Pos or Neg as defined.
- **FR-006**: A crossing's upper and lower strands MUST be identified by their positions on the crossing's left-hand (before) side, as defined, and never by which strand passes over.
- **FR-007**: The outputs MUST NOT depend on over/under. Two diagrams that differ only in whether crossings are written `/` or `\` MUST produce identical outputs.
- **FR-008**: Every stretch of strand, from one feature to the next feature that involves it, MUST be reported consistently at both ends: the component number and direction reported for it at the feature where it starts MUST equal those reported at the feature where it ends.
- **FR-009**: Every opening and every closing MUST report exactly one Pos strand and one Neg strand. Every reference opening MUST report `(Pos, Neg)`.
- **FR-010**: The outputs MUST depend only on the diagram's feature sequence, never on the placement mode, rendering, or any other setting. Repeated requests for the same diagram MUST produce identical outputs.
- **FR-011**: For a diagram that is not well-formed, the library MUST report an error and no outputs, and MUST NOT crash or abort. The error MUST identify the sequence position of the first feature, in sequence order, that cannot be applied, and why. If every feature applies but strands remain open, the error MUST say the diagram ends with that many open strands.
- **FR-012**: The empty diagram MUST produce three empty outputs (zero components), not an error.
- **FR-013**: Requesting orientation MUST NOT modify the diagram.
- **FR-014**: Worked examples W1–W13 MUST produce exactly the listed outputs, and M1–M7 MUST produce the listed errors.

### Key Entities

- **Diagram**: an ordered sequence of features in the abbreviated notation. It is the only input, and it is not changed.
- **Feature**: an opening, closing or crossing with a vertical index, and exactly two strands (upper and lower) as defined.
- **Component**: a closed loop of the diagram. It is identified by its component number and anchored at its reference opening.
- **Reference opening**: a component's first opening in sequence order. It fixes the component's number and reference direction.
- **Orientation result**: outputs (1), (2) and (3) together, for one diagram.
- **Orientation error**: the reason a diagram was rejected. It carries the offending feature's sequence position (or the end of the diagram) and a description.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 13 worked examples (W1–W13) produce exactly the listed outputs, including the six from the feature request (W1–W6) reproduced unchanged, and all 7 malformed examples (M1–M7) are rejected as listed.
- **SC-002**: For every well-formed diagram of up to 8 features (175,537 diagrams; that count confirms the enumeration is complete), the output satisfies all of the following, with zero failures:
  - (a) the number of components equals the number of closed loops counted by a method independent of the orientation;
  - (b) output (1) is strictly increasing and every entry is an opening;
  - (c) FR-008 continuity holds for every strand stretch;
  - (d) FR-009 holds for every opening and closing;
  - (e) reversing every crossing leaves all three outputs unchanged.
- **SC-003**: For every well-formed diagram of up to 6 features (1,121 diagrams), and every move the library offers on it (plus rotation), the following invariants, derived from the output and each crossing's over/under by the standard crossing-sign rule, are equal before and after the move:
  - the number of components and the sum over component pairs of |linking number|, for every move except changing a crossing;
  - the total writhe of self-crossings, for every move except changing a crossing and the Reidemeister I moves (introducing or collapsing a kink).

  Any discrepancy MUST be root-caused. A discrepancy traced to a move that itself changes the link it is applied to is a defect in that move. It is recorded separately and does not count against this feature.
- **SC-004**: Known values are reproduced:
  - W4's two crossings have equal signs (|linking number| = 1);
  - W11's two crossings have opposite signs (linking number 0);
  - W4 with one crossing reversed (`(0 (1 /0 \2 )1 )0`) has linking number 0;
  - W5's three crossings have equal signs;
  - W2 and W9 have single crossings of opposite sign.
- **SC-005**: Every malformed diagram obtained by altering a single feature of a well-formed diagram of up to 6 features is rejected with an error that names the correct position, with zero crashes and zero outputs produced. Alterations covered: changing its index to any value up to the strand count plus two, or deleting it.
- **SC-006**: A well-formed diagram of 10,000 features is fully oriented in under one second on a typical developer machine.

## Assumptions

- **Interpretations fixed by the feature request's examples**. The six examples are consistent with exactly one reading, and every one of them is reproduced unchanged:
  - Every pair is ordered (upper strand, lower strand).
  - A crossing's upper and lower strands are taken from its left-hand side. The alternative readings, (over, under) and right-hand side, both contradict W2.
  - Pos means travel rightward in the component's reference direction.
  - A component's "opening feature" is its first opening in sequence order (W5, W6).
  - Output (2) holds component numbers, i.e. positions within output (1). The request's own gloss says so ("the 0th knot identified in (1)"), and W7 and W10 are included because they tell the two readings apart.
- **Positions and vertical indices** mean what they mean everywhere else in the library: position 0 is the bottom strand. The renderer confirms this.
- **Output representation** is left to planning. The request suggests an enum, or a pair whose second item is optional, so that crossings (two component numbers) can be told apart from openings and closings (one).
- **Scope**: this is a core-library capability only.
  - Showing orientation in the `ascii_print` tool or the knot-so-good GUI is out of scope.
  - Public computation of crossing signs, writhe, linking numbers or other invariants is out of scope. They appear here only as verification (SC-003, SC-004) and are natural follow-on features.
- **Well-formedness** is checked by this capability itself. The existing notation parser still accepts structurally invalid sequences, and the renderer currently panics on some of them (for example M3, M4, M5 and M7). Changing the parser or the renderer is out of scope.
- **Moves**: SC-003 relies on the library's existing moves preserving the link they are applied to. It is also expected to act as an incidental check of those moves.
- **Dependencies**: no new third-party dependencies are expected (Constitution V).
