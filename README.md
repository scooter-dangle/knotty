knotty
======

Custom encoding of knot diagrams and diagram manipulations.

```knot
# square knot
(0
(2
\1
(3
/2
/4
)3
\1
)2
)0
```

```knot
                 ____   _
                /    \ / \
               /      /   \
              /   ___/ \   \
             /   /      \   \
            /   (        )   \
      _____/     \   ___/     \_____
     /            \ /               \
    (              /                 )
     \   _________/ \____________   /
      \ /                        \ /
       \                          \
   ___/ \________________________/ \___
  /                                    \
 (                                      )
  \____________________________________/
```

Examples for playing around with them so much
---------------------------------------------

### `ascii_print`

The \[for now] inaptly named `ascii_print` example program takes a knot diagram and, optionally, moves as input(s) and renders
the ASCII version of the resulting diagram.

### `knot-so-good`

An ugly, simple mini browser-based app for more quickly working with knot diagrams. See it's README:
./examples/knot-so-good/README.md
