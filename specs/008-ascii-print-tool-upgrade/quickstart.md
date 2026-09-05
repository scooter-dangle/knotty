# Quickstart: Validating the ASCII Print Tool Upgrade

Prerequisites: Rust toolchain per `rust-toolchain.toml` (1.94.0), repo root.

## Setup

```sh
cargo build --example ascii_print
```

## Example diagram (trefoil, from `src/diagram/tests.rs`)

```sh
cat > /tmp/trefoil.txt <<'EOF'
(0 (2 \1 /0 \1 )2 )0
EOF
```

## Scenario 1 — succinct + precalculated-heights by default (Story 1, SC-001)

```sh
cargo run --example ascii_print -- /tmp/trefoil.txt
```

**Expected**: prints the compact (succinct) rendering with no environment
variables set — matches
`AbbreviatedDiagram::from_str(...)?.with_mode(PlacementMode::PrecalculatedHeights).ascii_print_compact::<false>()`.
Contrast with the pre-upgrade tool, which required
`KNOTTY_COMPACT=true KNOTTY_PRECALC=true cargo run --example ascii_print -- /tmp/trefoil.txt`
to get the same result.

## Scenario 2 — explicit fully-spaced style (Story 2, SC-002)

```sh
cargo run --example ascii_print -- /tmp/trefoil.txt --style full-spaced
```

**Expected**: the same diagram, fully spaced (no blank-run collapsing).
Running the same command without `--style full-spaced` must go back to
Scenario 1's succinct output (default never silently changes).

## Scenario 3 — expand succinct text back to fully-spaced (Story 3, SC-003)

```sh
cargo run --example ascii_print -- /tmp/trefoil.txt > /tmp/trefoil.succinct.txt
cargo run --example ascii_print -- /tmp/trefoil.succinct.txt \
  --input-format succinct --style full-spaced
```

**Expected**: output is identical to Scenario 2's — the succinct file
carries a hidden `# ascii_print-grid: ` trailer (see `data-model.md`) with
the exact rendered grid, which the tool reads back automatically; no
`--placement` flag is meaningful or accepted here (research R11). Diff the
two outputs to confirm:

```sh
cargo run --example ascii_print -- /tmp/trefoil.txt --style full-spaced \
  > /tmp/expected.txt
diff /tmp/expected.txt <(cargo run --example ascii_print -- \
  /tmp/trefoil.succinct.txt --input-format succinct --style full-spaced)
```

## Scenario 4 — long-only flags and shell completions (Story 4, SC-004/SC-005)

```sh
cargo run --example ascii_print -- --help
```

**Expected**: every listed option is long-form (`--foo`, never `-f`) except
clap's own built-in `-h`/`-V`.

```sh
cargo run --example ascii_print -- --completions zsh > /tmp/_ascii_print
zsh -n /tmp/_ascii_print  # syntax-checks the generated script
```

**Expected**: exits 0 — the generated script is syntactically valid for the
requested shell.

## Edge cases to spot-check manually

- `echo "not a diagram" | cargo run --example ascii_print -- -` → non-zero
  exit, descriptive error on stderr, nothing on stdout (C6).
- `cargo run --example ascii_print -- /tmp/trefoil.succinct.txt --input-format succinct /tmp/some-moves.txt`
  → non-zero exit, error naming the conflict (C5).
- `cargo check --target wasm32-unknown-unknown` (library only, no
  `--examples`) still succeeds — confirms `clap`/`clap_complete` never reach
  `src/` (constitution II).
