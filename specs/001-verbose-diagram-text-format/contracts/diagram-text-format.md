# Contract: Diagram Text Format

Two contracts are in scope: the **text format** itself (what users write) and the **public Rust API**
that reads and writes it. Both are stable surfaces — the format because people will commit it in
tests and bug reports, the API because it is library-first.

## 1. Text format grammar

```text
diagram     := row (LF row)* LF?
row         := cell*
cell        := "_" | "-" | "\" | "/" | "A" | "a" | "(" | "." | ")" | ","
             | "j" | "i" | "r" | "2" | "k" | "L"
LF          := "\n" | "\r\n"
```

**Reading rules beyond the grammar**

| Rule | Behavior |
|---|---|
| Row order | First row is the **top** of the picture. |
| Ragged rows | Missing trailing cells inferred as `Empty`, up to the widest row. Always accepted. |
| Empty input | A diagram with no rows. Not an error. |
| Blank line | A row of entirely `Empty` cells. Preserved, including a trailing one past the terminator. |
| Trailing `LF` | A single one terminates the last row and is not itself a row. |
| Unknown byte | Error. Includes every whitespace character. |
| Semantics | None. The format describes a picture; it is never checked against knot validity. |

**Canonical form**: every row padded to the diagram's full width with `_`, nothing trimmed, each row
terminated by `\n`. Writing always produces canonical form; reading accepts canonical and ragged
alike.

**Error message contract**: names the offending character and its **1-based line and column as the
user typed them** — not as stored internally, which is reversed. Only the first offending character
is reported.

### Examples

Unknot — 2 rows:

```text
()
.,
```

Trefoil (`(0 (2 /1 \0 /1 )2 )0`) — 4 rows of 7, canonical:

```text
_(---)_
_./-/,_
(-A\A-)
.--a--,
```

The same trefoil written raggedly. Accepted; reads to the identical diagram; writes back as the
canonical form above:

```text
_(---)
_./-/,
(-A\A-)
.--a--,
```

## 2. Public Rust API

Added to the existing `knotty` public surface. No existing signature changes.

```rust
impl Horiz {
    /// The format character for this cell.
    pub const fn as_byte(&self) -> u8;

    /// The cell for a format character, or `None` if unrecognized.
    pub const fn from_byte(byte: u8) -> Option<Self>;
}

impl std::str::FromStr for VerboseDiagram {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, String>;
}

impl std::fmt::Display for VerboseDiagram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}
```

`Horiz`, `VerboseLine`, and `VerboseDiagram` are already re-exported from `src/lib.rs`; no change is
needed there.

### Guarantees the API must uphold

| ID | Guarantee |
|---|---|
| C-1 | `from_byte(h.as_byte()) == Some(h)` for all 16 `Horiz` variants. |
| C-2 | The 16 bytes are pairwise distinct. |
| C-3 | `d.to_string().parse::<VerboseDiagram>().unwrap() == d` for any renderable `d`. |
| C-4 | `d.to_string().parse::<VerboseDiagram>().unwrap().to_string() == d.to_string()`. |
| C-5 | For any accepted `t`, `t.parse::<VerboseDiagram>().unwrap().to_string()` is canonical, and re-reading it is a fixed point. |
| C-6 | `"".parse::<VerboseDiagram>()` is `Ok` with zero rows. |
| C-7 | Every row of `to_string()` output has equal length. |
| C-8 | An error message contains the offending character and its 1-based input line and column. |
| C-9 | Parsing never panics on arbitrary input, including non-UTF-8-shaped byte sequences reachable through `&str`. |

### Rendering contract (unchanged, now reachable from text)

`VerboseDiagram::display::<false>()` is already public and yields the ASCII picture as newline-
terminated lines. A parsed diagram renders through exactly the same path as one built from notation —
this feature adds no second renderer.

## 3. App-facing contract

The example app consumes only the API above. It must not gain a path from diagram text back to
abbreviated notation: the format is deliberately one-directional, because a valid picture need not be
a valid knot (spec FR-014).
