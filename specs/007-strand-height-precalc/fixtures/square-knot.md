# Golden Fixture: square knot

Supplied by the feature owner 2026-09-03. **Authoritative.** ✅ Fully verified.

## Input

```text
(0 (0 \1 (1 /0 /2 )1 \1 )0 )0
```

10 features: 3 openings, 4 crossings, 3 closings.

## Expected — Component A

Per-strand maxima, in opening order:

```text
(4, 5), (0, 3), (1, 2)
```

The second pair **diverges** — separation `3 − 0 − 1 = 2`.

## Expected — Component B

6 rows × 12 columns (10 feature columns + 2 transfer columns), read top-down:

```text
.__________.
(__.____.__)
...x__._x...
../.._y..\..
.(..(._)..).
..\__y___/..
```

## Verification

| col | feature | drawn @ row | joins rows | `floor` | check |
|-----|---------|-------------|------------|---------|-------|
| 0 | `(0` | 4 | maxima (4,5) | 4 | ✅ |
| 1 | `(0` | 1 | maxima (0,3) | 1 | ✅ |
| 2 | *transfer* | — | lower 1→0, upper 2→3 | — | separation 2, split 1/1 |
| 3 | `\1` | 3 | 2, 4 | 3 | ✅ **non-adjacent**, gap 1, split 1/0 |
| 4 | `(1` | 1 | maxima (1,2) | 1 | ✅ |
| 5 | `/0` | 0 | 0, 1 | 0 | ✅ |
| 6 | `/2` | 2 | 2, 3 | 2 | ✅ |
| 7 | `)1` | 1 | 1, 2 | 1 | ✅ |
| 8 | `\1` | 3 | 3, 4 | 3 | ✅ |
| 9 | *transfer* | — | lower 0→1, upper 3→2 | — | separation 2, split 1/1 |
| 10 | `)0` | 1 | 0, 3 | 1 | ✅ **non-adjacent**, gap 2, split 1/1 |
| 11 | `)0` | 4 | 4, 5 | 4 | ✅ |

## What this fixture adds

**The first divergent opening.** Pair 2's maxima are `(0, 3)`, so its cap at row
1 is at neither strand's maximum — the lower descends 1→0 and the upper rises
2→3 in column 2. This is the case `rotated-5_1` could not exercise, and it
confirms FR-002 and FR-015 where the two differ.

**An odd separation.** The `\1` at column 3 joins rows 2 and 4 — a gap of 1,
which cannot be split evenly. The lower strand takes the single step (2→3) and
the upper does not move. See the split rule in FR-002.
