# AutoCAD automation (3D-print parts)

AutoLISP routines for **AutoCAD 2012+** that build parametric 3D solids (cylinders, boxes, booleans) in **WCS**, aimed at **millimeter** drawings and **3D printing**. No COM; geometry is driven through standard commands (`CYLINDER`, `BOX`, `UNION`, `SUBTRACT`, etc.).

## Repository contents

| Path | Notes |
|------|--------|
| [`automation-radio-tree/`](automation-radio-tree/) | All part generators (`.lsp`). |
| [`README.md`](README.md) | This file. |
| [`CHANGELOG.md`](CHANGELOG.md) | Version history. |
| [`.vscode/`](.vscode/) | Minimal workspace config: `*.lsp` → Lisp grammar; optional [AutoLISP language extension](.vscode/extensions.json). |

There is **no** shared root `3dprint-drawing-setup` script in this tree; each routine expects a **new drawing** with **decimal units** and **1 unit = 1 mm** (see **Drawing setup** below).

## Requirements

- AutoCAD **2012** or newer.
- **INSUNITS** / insertion scale and modeling in **millimeters** so slicer export stays **1:1** with the numbers in the code.

## Drawing setup (before APPLOAD)

1. **File → New** (e.g. `acad3d.dwt` for 3D), then **Save As** your part `.dwg`.
2. Set **UNITS** to **decimal** and use **millimeters** for insertion scale (or equivalent so geometry is modeled in mm).
3. Work in **WCS** unless a command temporarily moves UCS (e.g. **EC16** offsets for the second cap).
4. **APPLOAD** the desired `.lsp` from `automation-radio-tree/` and run the command (see table).
5. Export a mesh for your slicer (**STLOUT** / **EXPORT**, depending on your install).

## Part commands (`automation-radio-tree/`)

| File | Command | Summary |
|------|---------|--------|
| [`adapter-16x20.lsp`](automation-radio-tree/adapter-16x20-part1.lsp) | **`ADP16P1`** | Sleeve **20 mm OD**, **16 mm ID**: **30.68 mm** hollow bore + **4 mm** solid cap (full OD); **14 mm OD** shaft **3 in** (**76.2 mm**) along **+Z**. |
| [`endcaps-16cf.lsp`](automation-radio-tree/endcaps-16cf.lsp) | **`EC16`** | **Left + right** end caps for **16 mm OD** CF tube; bore **16.2 mm**, OD **22.2 mm**; **25.4 mm** cap length, **2 mm** end plug; **80 mm** separation on **X**; USB clip, slot **+Z**. Matches **`TCF16`** geometry. |
| [`sleeve-bracket-16cf.lsp`](automation-radio-tree/sleeve-bracket-16cf.lsp) | **`SB16`** | **2 in** (**50.8 mm**) sleeve (same bore/OD as **TCF16**) + side bracket; built sleeve **+Y**, bracket **+X**, then **ROTATE3D** about WCS **Y** (default **90°**). Optional: `(setq sb16-od-bite 0.05)` before loading; `(setq sb16-rotate-y-deg 270.0)` for bracket on **+Z**. |
| [`t-connector-16cf.lsp`](automation-radio-tree/t-connector-16cf.lsp) | **`TCF16`** | **T** for **16 mm OD** tubes: **16.2 mm** ID, **22.2 mm** OD, **25.4 mm** leg engagement; legs **±X** and **+Y**. Optional USB U-clip on flat top (**Yes/No** prompt). |

### Tuning geometry

- Edit the `setq` block at the start of each `c:…` command (e.g. `c:TCF16`, `c:EC16`, `c:SB16`, `c:ADP16P1`) or the helper functions referenced in the file header.
- **SB16:** set `sb16-od-bite` and `sb16-rotate-y-deg` **before** `APPLOAD` (or `load`), then reload the file so top-level `setq`s apply.

## License / usage

Use and modify for your own drawings. No warranty; verify critical dimensions and booleans before manufacturing.
