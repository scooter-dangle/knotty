# Golden Fixture: square knot with links, encircled

Supplied by the feature owner 2026-09-03.

> ## ⚠️ NOT YET USABLE AS A GOLDEN FIXTURE
>
> 19 of its 20 features verify. **One cell is unresolved** — see *Discrepancy*
> below. Do not land this as a test (task T018) until the owner confirms which
> of the two readings is correct.

## Input

```text
(0 (1 (3 (3 (7 (7 )7 \5 (4 /3 /5 )4 \4 )3 )3 )3 (1 )3 )1 )0
```

20 features: 8 openings, 4 crossings, 8 closings.

## Expected — Component A

Per-strand maxima, in opening order:

```text
(0, 15), (3, 4), (9, 10), (5, 8), (13, 14), (11, 12), (6, 7), (1, 2)
```

Labelling the openings `A`–`H` in that order.

## Expected — Component B

16 rows × 36 columns (20 feature columns + 16 transfer columns), read top-down:

```text
........____________________........
......./.....___________....\.......
....../.....(___________)....\......
...../........................\.....
..../........()................\....
.../......_____________.........\...
../......(_____.____.__).........\..
./..........___x__._x.............\.
(........../....._y..\.............)
.\........(.....(._)..).........../.
..\........\_____y___/.........../..
...\....._________________....../...
....\...(_________________)..../....
.....\...................._.../.....
......\..................(_)./......
.......\____________________/.......
```

## Verification

All 8 openings match FR-002. Of the 12 crossings and closings, 11 match:

| col | feature | strands | rows | `floor` | drawn | check |
|-----|---------|---------|------|---------|-------|-------|
| 0 | `(0` | opens A | maxima (0,15) | 7 | 7 | ✅ |
| 8 | `(1` | opens B | maxima (3,4) | 3 | 3 | ✅ |
| 9 | `(3` | opens C | maxima (9,10) | 9 | 9 | ✅ |
| 10 | `(3` | opens D | maxima (5,8) | 6 | 6 | ✅ |
| 12 | `(7` | opens E | maxima (13,14) | 13 | 13 | ✅ |
| 13 | `(7` | opens F | maxima (11,12) | 11 | 11 | ✅ |
| 14 | `)7` | F_lo, F_hi | 11, 12 | 11 | 11 | ✅ |
| **15** | **`\5`** | **C_lo, C_hi** | **9, 10** | **9** | **8** | ❌ |
| 16 | `(4` | opens G | maxima (6,7) | 6 | 6 | ✅ |
| 17 | `/3` | D_lo, G_lo | 5, 6 | 5 | 5 | ✅ |
| 18 | `/5` | G_hi, D_hi | 7, 8 | 7 | 7 | ✅ |
| 19 | `)4` | G_lo, G_hi | 6, 7 | 6 | 6 | ✅ |
| 20 | `\4` | D_hi, C_lo | 8, 9 | 8 | 8 | ✅ |
| 22 | `)3` | D_lo, D_hi | 5, 8 | 6 | 6 | ✅ gap 2, split 1/1 |
| 23 | `)3` | C_lo, C_hi | 9, 10 | 9 | 9 | ✅ |
| 24 | `)3` | E_lo, E_hi | 13, 14 | 13 | 13 | ✅ |
| 25 | `(1` | opens H | maxima (1,2) | 1 | 1 | ✅ |
| 26 | `)3` | B_lo, B_hi | 3, 4 | 3 | 3 | ✅ |
| 27 | `)1` | H_lo, H_hi | 1, 2 | 1 | 1 | ✅ |
| 35 | `)0` | A_lo, A_hi | 0, 15 | 7 | 7 | ✅ gap 14, split 7/7 |

## Discrepancy — column 15

Simulating the logical stack with strand identities, at column 15 the live
levels are:

```text
logical:  0     1     2     3     4     5     6     7     8     9
strand:  A_lo  B_lo  B_hi  D_lo  D_hi  C_lo  C_hi  E_lo  E_hi  A_hi
row:      0     3     4     5     8     9    10    13    14    15
```

`\5` joins logical levels 5 and 6 — `C_lo` and `C_hi`, at rows 9 and 10 — so
FR-011 puts it at `floor((9+10)/2) = 9`. The diagram draws it at **row 8**.

Row 8 is where `\4` belongs, joining `D_hi` (row 8) and `C_lo` (row 9) — and
indeed the later `\4` at column 20 is drawn there and verifies.

The grid is *internally* consistent with row 8: row 9 of column 15 is empty,
which is the shadow a glyph at row 8 casts. So this is not a single mistyped
character; the whole cell sits one row low.

Two readings, both plausible:

| | Reading | Consequence |
|---|---|---|
| **A** | The **diagram** is off: the `x` at column 15 belongs at row 9, with its shadow at row 10. | `\5` is a self-crossing (kink) of pair C. |
| **B** | The **encoding** is off: the 8th feature should be `\4`, not `\5`. | Two crossings between `D_hi` and `C_lo` — a clasp, which suits a "square knot with links" diagram. |

Reading B looks more likely for a link diagram, but that is a guess about intent
and this file records the question rather than resolving it. **Owner
confirmation needed.**

The other four fixtures are unaffected — this discrepancy is local to this one
column.
