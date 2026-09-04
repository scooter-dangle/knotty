# Internal Contract: Precalculated Strand Heights

This is the seam between the two independently-implementable halves of the
feature. It is an **internal** contract (not public API — see
[public-api.md](./public-api.md) for the public surface), but it is written down
because both halves are developed against it rather than against each other.

```text
        ┌─────────────────────────┐        ┌─────────────────────────┐
        │  Component A            │        │  Component B            │
 enc ──▶│  height calculation     │──map──▶│  render from heights    │──▶ grid
        │  (encoding → heights)   │   ▲    │  (encoding+heights→grid)│
        └─────────────────────────┘   │    └─────────────────────────┘
                                      │              ▲
                        THIS CONTRACT ┘              │
                                          enc ───────┘
```

Both components take the diagram encoding. Only A produces the height map; only
B consumes it. Neither calls the other.

## Why this contract exists

Component B must be testable **without** Component A. Its tests supply heights
from fixtures directly, so a bug in A cannot mask or manufacture a bug in B, and
the two can be built in either order or concurrently. The end-to-end wiring is a
separate, later step (Phase 5), where A's real output is fed to B and
cross-checked against the fixture heights.

## Component A — height calculation

**Input**: the diagram encoding (the ordered `AbbreviatedItem` sequence).

**Output**: the precalculated starting height of every strand, as defined under
*Shape* below.

**Properties**:

- **Pure**: no rendering, no grid, no `Horiz` glyphs. Encoding in, numbers out.
- **Deterministic**: identical input always yields identical output (FR-008).
- **Total**: every valid encoding yields a result, including the empty diagram
  (FR-010).
- **No fixpoint iteration.** Spec FR-001 guarantees this by defining a maximum
  over the strand's **flat run only** — excluding the boundary movement by which
  it meets its cap and cup. Without that exclusion a strand's maximum would
  depend on a cap/cup row computed from that same maximum. An implementation
  that finds itself iterating to convergence has misread the definition.
- **Longest path, not a flat count.** A strand's height is one more than the
  tallest thing ever beneath it (research R2):

  ```text
  height(s) = 0                                     if no strand is ever below s
  height(s) = 1 + max{ height(t) : t ever below s }  otherwise
  ```

  Simulate the ordered stack of live strands — `(N` inserts a pair at logical
  index `N`, `)N` removes two, **crossings do not reorder levels** — record the
  immediately-below relation among adjacent neighbours after each mutation, and
  take the memoized longest path. Adjacent edges suffice; the full below-relation
  gives identical heights.

> ⚠️ **Do not replace this with a gap formula inferred from the fixtures.** The
> natural one — a pair's gap equals the count of strands opened between it —
> matches all 23 pairs in all five fixtures and is wrong; `(0 (1 )1 (1 )1 )0`
> breaks it. Heights are assigned absolutely, from what lies beneath; the gap is
> a consequence, never an input.

**Derived grid height** — Component B sizes the grid from A's output:

```text
height = max(all maxima) + 1
```

This equals `AbbreviatedDiagram::height()` when no pair diverges and **exceeds it
otherwise**: a divergent pair holds its gap open for its whole life, so the
diagram can span more rows than are ever occupied at once. The encircled fixture
needs 16 rows while never having more than 12 strands live. Do not size the grid
with `height()` under this mode.

## Component B — render from heights

**Input**: the diagram encoding **and** a height map satisfying this contract.

**Output**: the rendered grid (`Vec<Vec<Horiz>>` / `VerboseDiagram`).

**Properties**:

- Opens each strand at the height the map specifies (FR-001, FR-002).
- Strands that do not change rows between open and close render flat, with no
  transfer segments (FR-003).
- Still emits the boundary diagonals intrinsic to entering at the opening index
  and leaving at the closing index (FR-009).
- Brings crossing partners together **at the floored midpoint of their two rows**,
  crosses there, and returns both to their maxima afterward; never draws a
  crossing between non-adjacent rows (FR-007, FR-011).
- Applies one uniform rule to every two-strand feature: a cap, cup, or crossing
  sits at the floored midpoint of the two strands it joins, with the movement
  split evenly between them (FR-002, FR-011, FR-016). Caps and cups are
  boundaries and the strands do not return; after a crossing they do.
- **Maintains a logical-level → rendered-row mapping.** A notation index names a
  logical level among the currently-live strands, not a grid row. Under the
  default placement the two coincide, because every close pulls the stack back
  down; under precalculated placement they diverge. In the rotated 5₁ fixture
  `\2` is drawn at row 6 and `)1` at row 3. Resolving indices to rows is
  Component B's job alone — Component A never sees rendered rows.
- **Trusts the map.** B does not recompute, validate, or second-guess the
  heights. Given a well-formed map it produces a well-formed grid. A map that
  violates this contract is Component A's defect, not B's.

## Shape

> **RESOLVED 2026-09-03 — per strand, two values per opening event.**
>
> [research.md](../research.md) R2 and [data-model.md](../data-model.md) describe
> a **per-pair** value (one row per opening event). That is an oversimplification
> and is superseded. Heights are calculated **per strand**: a pair's two strands
> can diverge, because an opening occurring between them raises the upper without
> raising the lower. Tracing `OpeningCentered::append` on main, a pair opened at
> levels 3,4 followed by `(4` ends up spanning levels 3 and 6.
>
> Component A therefore yields **two maxima per opening event** — one for the
> lower strand, one for the upper. Amend research R2 and data-model.md to match.

### Derived: the opening placement row

Component B does not place a cap at either strand's maximum. Per spec FR-002 the
cap is drawn at the floored midpoint of the pair's two maxima:

```text
opening_row = floor((lower_max + upper_max) / 2)
```

Flooring is exact rather than lossy: the opening glyph renders halfway up its
tile, so tile `floor(m)` places the cap at height `m` whenever the midpoint ends
in `.5`. Each strand then transfers from the cap to its own maximum (FR-015).

Whether this derivation belongs to A or B is an implementation choice, but the
**contract carries the two maxima**, not the derived row — B needs both maxima
anyway to emit the two boundary transfers.

Three further details, **settled by the fixtures**:

| Detail | Resolution |
|--------|------------|
| **Key** | Opening order. The fixtures list maxima "ordered by strand opening", one `(lower, upper)` pair per opening feature, in the order the openings appear in the encoding. |
| **Origin** | A **rendered row**, 0 at the bottom, matching the grid `to_text()` produces (which prints top-down, so row 0 is the last line). Not a logical index — the two diverge under this mode. |
| **Direction** | Larger means visually higher. `upper_max ≥ lower_max` always, and the pair is adjacent (`upper = lower + 1`) unless something opens between them. |

## Fixture format

Each fixture pairs an input with a known-correct expected output, supplied by
the feature owner rather than generated from the implementation.

**Component A** — encoding → heights:

```text
input:    (0 (2 (4 (6 )5 )3 )1 (1 (3 (5 )6 )4 )2 )0
expected: <one row per strand, or per pair — per Shape above>
```

**Component B** — encoding + heights → rendered ASCII:

```text
input:    <encoding>
heights:  <a map satisfying this contract, supplied directly, NOT from A>
expected: <the exact rendered ASCII>
```

Component B fixtures must include at least one case where a crossing's partners
are **not** adjacent under the supplied heights, since the crossing-alignment
construction is the highest-uncertainty area in the whole feature (research R4).

## Consistency obligation

Where a Component A fixture input also appears as a Component B fixture input,
A's expected output must be exactly the height map B's fixture supplies.
Asserting that equality (T028) is what proves the two halves actually meet.

## Traceability

| Contract element | Requirements |
|---|---|
| A: pure, deterministic, total, linear | FR-001, FR-008, FR-010 |
| B: placement at supplied heights | FR-002 |
| B: flat strands | FR-003, FR-004 |
| B: boundary diagonals retained | FR-009 |
| B: crossing alignment | FR-007, FR-011 |
| A+B integrated | FR-006, C2–C6, SC-001–SC-003 |
