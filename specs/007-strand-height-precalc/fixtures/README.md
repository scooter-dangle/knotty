# Golden Fixtures

Owner-supplied inputs and expected outputs for the height-precalculated
placement mode. **Authoritative** — expected values come from the feature owner,
never from running an implementation. See
[../contracts/strand-heights.md](../contracts/strand-heights.md) for the seam
these feed, and tasks T012 (Component A) / T018 (Component B).

| Fixture | Features | Grid | Exercises | Status |
|---------|----------|------|-----------|--------|
| [rotated-5_1](./rotated-5_1.md) | 13 | 8×18 | divergent closings; logical level ≠ rendered row | ✅ verified |
| [square-knot](./square-knot.md) | 10 | 6×12 | divergent pair `(0,3)`; non-adjacent crossing and closing | ✅ verified |
| [little-dumb-link](./little-dumb-link.md) | 10 | 8×15 | pair opened *inside* another pair's gap; separation 6 | ✅ verified |
| [non-adjacent-crossing](./non-adjacent-crossing.md) | 10 | 6×19 | **crossing alignment with return transfers**, repeated | ✅ verified |
| [square-knot-links-encircled](./square-knot-links-encircled.md) | 20 | 16×36 | deep nesting (6 pairs); separation 14; clasp | ✅ verified |

## Height format

Component A's output is **two maxima per opening**, in opening order:

```text
(lower_max, upper_max), (lower_max, upper_max), ...
```

The opening's drawn row is the derived value `floor((lower_max + upper_max) / 2)`
(FR-002) and is *not* part of the contract — Component B recomputes it, because
it needs both maxima anyway to emit the two boundary transfers.

## How these were verified

Each grid was decomposed by column and checked against the spec:

1. Every feature column holds exactly one feature glyph, and that glyph's
   element matches the encoding at that position (`x` = abbreviated `\`,
   `y` = abbreviated `/`).
2. Every opening's drawn row equals `floor((lower_max + upper_max) / 2)` (FR-002).
3. Strand positions were simulated forward from the transfer columns, and every
   cap, cup, and crossing was checked to sit at the floored midpoint of the two
   strands it joins (FR-002, FR-011, FR-016).
4. Every bridged separation was checked against the invariant
   `upper − lower − 1` (FR-015).

Across all five fixtures that is **63 features**, all matching.

This process earned its keep once: `square-knot-links-encircled` was supplied
with `\5` as its 8th feature, which the simulation placed at row 9 against a
diagram drawing it at row 8. The owner confirmed the encoding was at fault and
the diagram correct. Simulating the logical stack is what caught it — by eye the
index looks unremarkable, because under precalculated placement a notation index
never matches the rendered row anyway.
