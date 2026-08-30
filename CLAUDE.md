# Claude guidance for knotty

## Git

- All action pins use full commit SHA with a `# vX.Y.Z` or `# master` comment
- Branch names: `claude/<description>-tNrKs`
- Commit messages: conventional commits style (`feat:`, `fix:`, `ci:`, `build:`, `refactor:`, `doc:`)
- Never skip hooks; never force-push without `--force-with-lease`
- Prefer `git reset --hard <sha>` + clean commit over amend when cleaning up history

## CI (`.github/workflows/deploy.yml`)

- `dtolnay/rust-toolchain` pinned to `efa25f7f...` **requires** explicit `toolchain:` input — it does not fall back to `rust-toolchain.toml`
- `targets: wasm32-unknown-unknown` must be passed as an action input (not via a separate `rustup target add` step)
- `rust-toolchain.toml` declares `targets` too, for local development benefit — keep both in sync

## Rust toolchain

- Channel pinned in `rust-toolchain.toml` (currently `1.94.0`)
- `targets = ["wasm32-unknown-unknown"]` declared in `rust-toolchain.toml`

## Style

- Minimal commits — one logical change per commit, no noise
- Remove redundant/hacky commits rather than piling on fixes; force-push with lease to clean history
- Don't add comments, docstrings, or type annotations to code that wasn't changed
- Don't introduce extra abstractions or error handling beyond what's needed

<!-- SPECKIT START -->
For additional context about technologies to be used, project structure,
shell commands, and other important information, read the current plan:
`specs/002-diagram-boundary-view/plan.md`
<!-- SPECKIT END -->
