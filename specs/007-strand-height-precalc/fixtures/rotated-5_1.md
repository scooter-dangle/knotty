# Golden Fixture: rotated 5₁

Supplied by the feature owner 2026-09-03. **Authoritative** — expected values are
the owner's, not derived from running any implementation. Feeds tasks T012
(Component A) and T018 (Component B); see
[../contracts/strand-heights.md](../contracts/strand-heights.md).

## Input

```text
(0 (2 (4 (6 /1 /3 /5 )4 )2 \0 \2 )1 )0
```

13 features: 4 openings, 5 crossings, 4 closings.

## Expected — Component A

Opening placement rows (the floored midpoints of FR-002):

```text
0, 2, 4, 6
```

Per-strand maxima, from which those rows derive. **Inferred, not supplied** —
see *Open point* below:

| Opening | lower_max | upper_max | `floor((l+u)/2)` |
|---------|-----------|-----------|------------------|
| `(0`    | 0         | 1         | **0** |
| `(2`    | 2         | 3         | **2** |
| `(4`    | 4         | 5         | **4** |
| `(6`    | 6         | 7         | **6** |

No pair diverges in this fixture, so each pair's strands stay adjacent and the
midpoint lands on the lower strand's row.

## Expected — Component B

8 rows × 18 columns (13 feature columns + 5 transfer columns). Text reads
top-down, so the first line is row 7:

```text
....______.___....
...(__.___x...\...
...___y....\...\..
..(__._)....\...\.
..___y__.....)...)
.(__.___).../.../.
.___y____._/.../..
(________x____/...
```

## Verification

Decomposed by column (`.`/`_` omitted; `(`/`)` cap and cup, `y` = abbreviated
`/`, `x` = abbreviated `\`, `/`/`\` = transfers):

| col | glyph @ row | feature | check |
|-----|-------------|---------|-------|
| 0–3 | `(` @ 0, 2, 4, 6 | `(0 (2 (4 (6` | each opens above the last, so nothing is displaced |
| 4–6 | `y` @ 1, 3, 5 | `/1 /3 /5` | partners already adjacent |
| 7 | `)` @ 4 | `)4` | rows 6,7 **stay at 6,7** — no pull-down. This is the feature working |
| 8 | `)` @ 2 | `)2` | live rows now 0,1 and 6,7 |
| 9 | `x` @ 0 | `\0` | partners adjacent |
| 10 | `x` @ **6** | `\2` | logical level 2 renders at **row 6** |
| 11–12 | `/` @ 1,2 and `\` @ 5,4 | — | 2 up, 2 down |
| 13 | `)` @ **3** | `)1` | joins rows 1 and 6 → `floor((1+6)/2) = 3` ✓ |
| 14–16 | `/` @ 0,1,2 and `\` @ 6,5,4 | — | 3 up, 3 down |
| 17 | `)` @ **3** | `)0` | joins rows 0 and 7 → `floor((0+7)/2) = 3` ✓ |

Confirms:

- **FR-002** — all four opening rows equal the floored midpoint of their pair's maxima.
- **FR-016** — both divergent closings land on the floored midpoint.
- **FR-015 invariant** — `)1` bridges a separation of `6−1−1 = 4`, split 2/2;
  `)0` bridges `7−0−1 = 6`, split 3/3. Even in both cases.
- **The displacement removal itself** — `)4` and `)2` retire rows beneath a live
  pair without pulling it down, which is what the default placement cannot do.

## Two facts this fixture establishes

### 1. A closing joins strands from *different* openings

`)1` retires logical levels 1 and 2, which at that column are `(0`'s **upper**
strand (row 1) and `(6`'s **lower** strand (row 6). `)0` then retires `(0`'s
lower and `(6`'s upper. So an opening's two strands are retired by two
*different* closings, and a closing's two strands come from two *different*
openings.

### 2. Logical level ≠ rendered row

`\2` is drawn at **row 6**, and `)1` at row 3 rather than row 1. The notation
index names a *logical level* among the currently-live strands; the rendered row
is wherever that strand actually sits. In the default placement the two always
coincide, because every close pulls the stack back down. Under precalculated
placement they diverge, so **Component B must maintain a logical-level →
rendered-row mapping**. This is not a detail of the height calculation —
Component A never sees it — but it is central to B.

## Open point

The supplied expectation is the four **opening placement rows** (0, 2, 4, 6),
which are the *derived* value of FR-002. The contract specifies Component A's
output as the **per-strand maxima**, since Component B needs both to emit the two
boundary transfers. Here the maxima are recoverable because no pair diverges;
in a divergent fixture they would not be. A part-1 fixture exercising divergence
should therefore supply the maxima, not the derived row.
