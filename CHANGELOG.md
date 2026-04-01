# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
where versions are tagged.

## [Unreleased]

## [0.0.2] - 2026-04-01

### Changed

- `README.md` — aligned with the tree as it exists: actual filenames (`t-connector-16cf.lsp`, `sleeve-bracket-16cf.lsp`), no references to root `3dprint-drawing-setup` files (not in repo), expanded per-command dimensions and **SB16** options (`sb16-od-bite`, `sb16-rotate-y-deg`), documented `.vscode/settings.json`.

### Fixed

- `adapter-16x20-part1.lsp` header comment no longer points at missing `3dprint-drawing-setup.lsp`.
- `sleeve-bracket-16cf.lsp` — file header and load `princ` use the real filename/path; `endcaps-16cf.lsp` header references `t-connector-16cf.lsp`.

## [0.0.1] - 2026-04-01

### Added

- `README.md` — project overview, requirements, layout, and command reference.
- `CHANGELOG.md` — change history for the repository.
