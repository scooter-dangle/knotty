knot-so-good
============

## Non-comprehensive list of prerequisites

### `trunk`

```
cargo install --locked trunk
```

## Run

From this directory on your machine, run

```
trunk serve --port 3000
```

and then navigate to `localhost:3000` in your browser.

## Manual diagram mode

The app has a second mode, reached with the "manual" option of the mode
toggle, for describing a rendered diagram directly rather than deriving one
from knot notation. Each line of the text box is one row of the picture,
read top-down, and each character is one cell:

```
. empty                  ( opened
_ line                   ) closed
x cross down, over       / transfer up
y cross down, under      \ transfer down
```

The trefoil, for instance, is

```
..___..
.(._.).
._y.y_.
(__x__)
```

Short lines are padded with empty cells, so there is no need to balance
line lengths while typing. The same reference is available in the app
itself under "character reference".

The "view" toggle, plain or bordered, redraws the picture with the cell
boundaries included, so each character you typed owns one visible box:

```
+---+---+---+---+---+---+---
|   |   |___|___|___|   |
+---+---+---+---+---+---+---
|   |  /|   |   |   |\  |
|   | ( |   |   |   | ) |
|   |  \|   |___|   |/  |
+---+---+---+---+---+---+---
```

Useful for tracking down which character drew the part of the picture you
did not mean. The setting is remembered, and it affects only how the
picture is drawn.

Manual mode describes a *picture*, not a knot — it will happily render
something no knot could produce, which is the point when you want to say
what a rendering should look like. Moves and rotation are unavailable
there, since they operate on knot notation.

## Drawing density

The "drawing" toggle, full or compact, is available in notation mode only
(for both the picture and the characters display) and redraws the diagram
with the opening and closing parentheses centred vertically in their cell.
Every feature is then whole inside one cell — a crossing is a complete `X`,
an opening carries both of its arms, and a strand climbs one level per cell
— where the full rendering splits each of them across two cells:

```
+---+---+---+---+---+---+---     +---+---+---+---+---+---+---
|   |   |___|___|___|   |        |   |   |   |   |   |   |
|   |  /|   |   |   |\  |        |   |   |___|___|___|   |
|   | ( |   |   |   | ) |        +---+---+---+---+---+---+---
+---+---+---+---+---+---+---     |   |  /|   |   |   |\  |
           full                  |   | ( |   |   |   | ) |
                                 |   |  \|   |___|   |/  |
                                 +---+---+---+---+---+---+---
                                          compact
```

For a diagram with no vertical strand transfers the two draw the *same*
picture, so the setting can be flipped back and forth to check one against
the other; where a strand does climb between levels the pictures differ,
because compact spends one cell per level where full spends three per two.
The setting is remembered.

## Immediate, obvious limitations

Aside from how ugly and snowflakey this example is, it also doesn't list
the possible moves in a useful way. This should be fixed next so that
users don't have stumble at blind guesses of the required syntax.

## Examples of vastly better tools

* [KnotFolio](https://kmill.github.io/knotfolio/). See its
  [README](https://github.com/kmill/knotfolio#related-programs) for a
  listing of other vastly better tools.
