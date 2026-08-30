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

The app has a second mode, reached with the "switch to manual diagram
mode" button, for describing a rendered diagram directly rather than
deriving one from knot notation. Each line of the text box is one row of
the picture, read top-down, and each character is one cell:

```
_ empty                  , closed above
- line                   j transfer up, start
\ cross down, over       i transfer up
/ cross down, under      r transfer up, finish
A cross up, over         2 transfer down, start
a cross up, under        k transfer down
( opened below           L transfer down, finish
. opened above
) closed below
```

The trefoil, for instance, is

```
_(---)_
_./-/,_
(-A\A-)
.--a--,
```

Short lines are padded with empty cells, so there is no need to balance
line lengths while typing. The same reference is available in the app
itself under "character reference".

The mode describes a *picture*, not a knot — it will happily render
something no knot could produce, which is the point when you want to say
what a rendering should look like. Moves and rotation are unavailable
there, since they operate on knot notation.

## Immediate, obvious limitations

Aside from how ugly and snowflakey this example is, it also doesn't list
the possible moves in a useful way. This should be fixed next so that
users don't have stumble at blind guesses of the required syntax.

## Examples of vastly better tools

* [KnotFolio](https://kmill.github.io/knotfolio/). See its
  [README](https://github.com/kmill/knotfolio#related-programs) for a
  listing of other vastly better tools.
