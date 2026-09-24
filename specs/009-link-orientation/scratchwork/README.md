# Spec 009 scratchwork: link orientation reference model

This is a throwaway Python model of the spec's Definitions, used to check the
worked examples while specifying. It is **not** part of the deliverable and is
expected to be deleted before the feature lands. Needs Python 3, no packages.

| File | What it does |
|---|---|
| `orient.py` | The model: a sweep that links strand segments and validates the diagram, then a walk around each loop. Also a crossing-sign helper, used only for checks. Reads diagrams from stdin, one per line. `--debug` prints the stack after every change during the sweep, then the partial outputs (2) and (3) after every assignment during the walk. The sweep assigns nothing to (2) and (3); they are filled only by the walk. |
| `user_examples.py` | Checks the six examples from the feature request against hard-coded expected values. |
| `check_spec.py` | Parses W1–W13 and M1–M7 out of `../spec.md` and checks them against the model. Also prints the SC-004 crossing signs. |
| `exhaustive.py` | Enumerates every well-formed diagram of up to 8 features (175,537). For each it checks the self-checks, the component count against an independent union-find, reference openings reading `(Pos, Neg)`, and invariance under swapping `/` and `\`. For diagrams of up to 6 features it also checks invariants under every Reidemeister II insertion. Takes about 10 s. |

Run the scripts from any directory:

```sh
echo '(0 (2 /1 \0 /1 )2 )0' | python3 specs/009-link-orientation/scratchwork/orient.py
echo '(0 /0 )0' | python3 specs/009-link-orientation/scratchwork/orient.py --debug
python3 specs/009-link-orientation/scratchwork/user_examples.py
python3 specs/009-link-orientation/scratchwork/check_spec.py
python3 specs/009-link-orientation/scratchwork/exhaustive.py
```

The crossing-sign convention in `orient.py` is `(+1 if '\' else -1) × d_upper × d_lower`, with `d` = ±1 for Pos/Neg. It follows from the right-hand rule, with `/` meaning the ascending strand is over, as the renderer draws it. The checks only need the convention to be applied consistently, so the model's results don't depend on which handedness is chosen.
