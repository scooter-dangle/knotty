# Pre-change baseline

**Captured**: 2026-09-02 | **Commit**: `ee3030c433eb067a9fceb67f69bd32facf56a829`

Everything below is the state this feature is measured against (SC-002, SC-003, SC-006).

## Tests

| Suite | Command | Result |
|---|---|---|
| Library | `cargo test` | **92 passed**, 0 failed |
| App | `cargo test --manifest-path examples/knot-so-good/Cargo.toml` | **26 passed**, 0 failed |
| | | **118 total** |

## WASM (constitution II)

| Target | Command | Result |
|---|---|---|
| Library | `cargo check --target wasm32-unknown-unknown` | clean |
| App | `cargo check --manifest-path examples/knot-so-good/Cargo.toml --target wasm32-unknown-unknown` | clean |

**Correction to the plan's artifacts**: `examples/knot-so-good` is a *separate crate*, not a workspace
member — the root `Cargo.toml` declares no `[workspace]`, and the app's manifest depends on the
library by path. So `cargo --package knot-so-good` fails with "package ID specification did not match
any packages"; `--manifest-path examples/knot-so-good/Cargo.toml` is the working form, and matches
what CI does (`working-directory: examples/knot-so-good` in `.github/workflows/test.yml`).
`quickstart.md` has been corrected accordingly.

This also means the root `cargo test` does **not** cover the app. Every phase gate must run both
suites.

## Recorded pictures

The eight opening-centered recordings are copied to `baseline-pictures/` so the Phase 5 rename
(T022) can be verified byte-for-byte rather than trusted.
