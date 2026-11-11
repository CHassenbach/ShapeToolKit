# ShapeToolKit Clean-Up Project

Master plan and work log for the full package overhaul. This file is the
single source of truth for the clean-up: what is planned, what is decided,
what is done. Update the checkboxes and the work log with every work session.

Branch: `clean-up`

---

## 1. Goals

1. Meet CRAN standards (target: `R CMD check` with 0 errors / 0 warnings / 0 notes;
   "check-clean GitHub package", no actual CRAN submission planned).
2. Clean all R files, remove dead code.
3. Tests for every feature and function; verify every app feature works.
4. Restructure the app into 4 sections:
   1. **Image Processing** - PNG conversion, cropping, resizing, splitting,
      merging asymmetrical halves, shape completion, morphing.
   2. **Shape Analysis** - EFA/PCA shape analysis, reconstruction,
      PC contribution plots.
   3. **Statistics** - data explorer (tests, correlation, regression,
      disparity), morphospace stability, gap analysis, PCA saturation.
   4. **Visualization** - data import, plotting module, shape panel,
      overview, distribution/boxplots.
5. Unify styling so the app no longer feels stitched together
   (keep shinydashboard, add a unified custom theme + shared UI helpers).
6. Overhaul README; consolidate the ~20 ad-hoc root markdown docs into
   vignettes, then delete them.

## 2. Decisions

| Decision | Choice | Date |
|---|---|---|
| Ad-hoc root *.md docs | Merge useful content into vignettes, then delete | 2026-11-11 |
| UI framework | Keep shinydashboard + unified custom theme | 2026-11-11 |
| CRAN goal | Check-clean GitHub package (no submission) | 2026-11-11 |
| Dead code | Delete outright (git history is the archive) | 2026-11-11 |

### Open decisions (need user input)

- [ ] **License conflict**: DESCRIPTION says `MIT + file LICENSE`, but 15
  vendored `R/momocs_*.R` files are GPL-2|3 derived (see ATTRIBUTION.md).
  Options: (a) relicense package GPL-3, (b) remove vendored code and depend
  on Momocs from CRAN, (c) rewrite the small used subset from scratch.
  Note: only ~14 of ~417 vendored functions are called directly by package code.
- [ ] **`Haug_panel()`**: exported programmatic API, no app/module/test usage.
  Keep as public API or delete?
- [ ] **`cluster_plot()` / `elbow_plot()`**: exported, zero usage anywhere.
  Delete, or wire into the Statistics section as a feature?

## 3. Inventory findings (2026-11-11)

### App wiring

Registered in `app.R`: data_import, image_processing, shape_analysis,
shape_reconstruction, plotting, gap_detection, morphospace_stability,
shape_panel, data_explorer.

**Orphaned modules** (complete ui/server pairs, never instantiated):

| Module | Purpose |
|---|---|
| `pca_saturation_module.R` | Bootstrap PCA variance saturation curves |
| `overview_module.R` | Shiny wrapper around `Haug_overview()` |
| `pc_contribution_plots_module.R` | PC contribution plots |
| `complete_shapes_module.R` | Batch completion of halved shapes |
| `morph_shapes_module.R` | Morphing/interpolation between shapes |

### Redundancy checks (user questions)

- **PCA saturation vs morphospace stability**: ~25-30 percent conceptual
  overlap only. Saturation = variance-explained vs sample size (works on a
  PCA object). Stability = full EFA+PCA recomputation under resampling of
  the image set (axis shifts, morphospace similarity). Verdict: keep both as
  separate features in Statistics; optionally extract a shared resampling
  helper later.
- **Overview rebuild**: `overview_module` wraps `Haug_overview()`, which
  internally calls `shape_plot()` - the same engine the plotting module
  uses. The overview UI can be rebuilt on plotting-module infrastructure;
  `Haug_overview()` stays as the programmatic batch/PDF API.

### Dead-code candidates

- `cluster_plot()`, `elbow_plot()` - exported, never called anywhere.
- `shape_plot_momocs()` - older duplicate of `shape_plot()`, documentation-only.
- `Haug_panel()` - no app/module/test usage.
- `run_app()`, `run_haug_app()` - deprecated launcher wrappers;
  `run_shape_app()` is canonical.
- Vendored Momocs code: ~403 of ~417 functions have no direct caller in
  package code (S3 dispatch means pruning must be careful and test-driven).
- `install_packages.R` - does global-env assignments (check NOTE); should be
  slimmed or removed (installation belongs in README/setup docs).

### CRAN audit (latest check: 0 E / 0 W / 6 NOTEs)

1. Hidden `.vscode` directory included in build.
2. Invalid license stub (`LICENSE` is a 2-line MIT stub; conflict with GPL
   vendored code).
3. Many non-standard top-level files (22 check logs, ~20 ad-hoc md docs,
   tmp_/test_ scripts, setup.bat, .Rhistory).
4. Global-env assignments in `R/install_packages.R`.
5. Rd problem: lost braces in `man/morph_shapes.Rd:59`.
6. Generated `my_overview.pdf` during check.

Also: legacy `Author`/`Maintainer` instead of `Authors@R`; trailing comma in
Suggests; `LazyData: true` with empty `data/`; no `Config/testthat/edition: 3`;
stray `tests/testthat/Rplots.pdf` and two `.cjs` files; 258 exports (238 S3
methods) - overly broad namespace; non-ASCII characters in 4 R files
(data_explorer_module, gap_detection_module, gap_detection_ndim,
plotting_module); 29+ Imports, several questionable (`av`, `webshot2`,
`chromote`, `sf`, `alphahull`, `nnet`, ...).

## 4. Target app architecture

```
dashboardSidebar
|- 1. Image Processing
|  |- Convert & Resize        (image_processing_module: png->jpg/other,
|  |                           white border, crop, resize)
|  |- Split / Cut Images      (split_image, cut_image_right_half)
|  |- Complete Halved Shapes  (complete_shapes_module)
|  `- Morph Shapes            (morph_shapes_module)
|- 2. Shape Analysis
|  |- Run Analysis            (shape_analysis_module)
|  |- Reconstruct Shapes      (shape_reconstruction_module)
|  `- PC Contributions        (pc_contribution_plots_module)
|- 3. Statistics
|  |- Data Explorer           (data_explorer_module - stats parts)
|  |- Morphospace Stability   (morphospace_stability_module)
|  |- Gap Analysis            (gap_detection_module)
|  `- PCA Saturation          (pca_saturation_module)
`- 4. Visualization
   |- Data Import             (data_import)
   |- Plotting                (plotting_module)
   |- Shape Panel             (shape_panel_module)
   |- Overview                (overview_module, rebuilt on plotting infra)
   `- Distributions           (boxplot/violin/histogram from data explorer)
```

Shared infrastructure to add:
- `R/app_theme.R`: one CSS theme (colors, spacing, box styles) +
  `stk_box()` / `stk_sidebar_layout()` style helpers used by every module.
- Central data store: one imported dataset + one analysis result shared
  across tabs (currently every module loads its own files).

## 5. Phases

### Phase 0 - Project setup
- [x] Inventory (modules, CRAN audit, dead code) - 2026-11-11
- [x] This plan document

### Phase 1 - Repository hygiene
- [x] Delete all `check_*.log` files (22)
- [x] Delete `.Rhistory`, `tmp_coo_*.R`, `test_gap_detection.R`,
      `test_morph_module.R`, `dev_script.R`, `setup.bat`, `setup.R`,
      `launch.R`, root `run_app.R` script
- [x] Delete `tests/testthat/Rplots.pdf`. The `.cjs` files stay: they are
      live test fixtures run via node from `test-3d-colour-groups.R:158`
      and `test-specimen-viewer.R:207`
- [x] Update `.gitignore` (Rplots.pdf; logs/.Rhistory already covered)
- [x] Update `.Rbuildignore` (.Rhistory, CLEANUP_PROJECT.md, Rplots.pdf)

### Phase 2 - Package metadata and check hygiene
- [ ] DESCRIPTION: `Authors@R`, remove trailing comma, remove `LazyData`,
      tighten Title/Description, add `Config/testthat/edition: 3`
- [ ] Resolve license decision (see open decisions)
- [ ] Fix `man/morph_shapes.Rd` lost-braces (fix roxygen source)
- [ ] Remove/rework `install_packages.R` (no global assignments)
- [ ] Remove non-ASCII chars from the 4 affected R files
- [ ] Audit Imports; move optional ones (av, webshot2, chromote, ...) to
      Suggests with `requireNamespace()` guards

### Phase 3 - Dead code removal
- [ ] Delete `cluster_plot.R`, `elbow_plot.R` (pending open decision)
- [ ] Delete `shape_plot_momocs.R`
- [ ] Delete deprecated `run_app.R` (R/) and `run_haug_app` alias
- [ ] Decide `Haug_panel.R` fate
- [ ] Prune vendored Momocs code to the used subset (test-driven: full test
      suite + app smoke test after each pruning step)
- [ ] Reduce NAMESPACE exports to intended public API
- [ ] Remove commented-out code blocks and stale TODOs

### Phase 4 - App restructure (4 sections)
- [ ] Rewrite `app.R` / `inst/app/app.R` sidebar to the target architecture
- [ ] Wire orphaned modules: pca_saturation, pc_contribution_plots,
      complete_shapes, morph_shapes, overview
- [ ] Split data explorer: stats features stay in Statistics; distribution
      plots move to Visualization
- [ ] Rebuild Overview on plotting-module infrastructure
- [ ] Central shared data store across tabs
- [ ] Single app source of truth (root `app.R` vs `inst/app/app.R`)

### Phase 5 - Unified styling
- [ ] Create `R/app_theme.R` (CSS + layout helpers)
- [ ] Migrate every module to shared helpers (consistent box usage,
      spinners, buttons, download controls, notification style)
- [ ] Consistent naming/wording in UI labels

### Phase 6 - Tests for every feature
- [ ] Unit tests per exported function (currently 15 test files)
- [ ] `shiny::testServer()` tests per module server
- [ ] Smoke test: app starts, all tabs render
- [ ] Verify every app feature manually; log results here

### Phase 7 - Documentation
- [ ] Overhaul README (move deep detail to vignettes)
- [ ] Merge useful ad-hoc md content into vignettes; delete the md files
- [ ] Update NEWS.md
- [ ] Regenerate roxygen docs

### Phase 8 - Final validation
- [ ] `R CMD check` clean (0/0/0)
- [ ] Full test suite green
- [ ] App walkthrough of all 4 sections

## 6. Work log

| Date | Phase | Work done |
|---|---|---|
| 2026-11-11 | 0 | Full inventory: module map, CRAN audit, dead-code audit. Decisions recorded (docs->vignettes, keep shinydashboard, check-clean goal, delete dead code). Created this plan. |
| 2026-11-11 | 1 | Repo hygiene complete: deleted 22 check logs, .Rhistory, 7 scratch scripts (dev_script.R, setup.R/.bat, launch.R, root run_app.R, test_gap_detection.R, test_morph_module.R, tmp_coo_*.R), tests/testthat/Rplots.pdf. Kept .cjs test fixtures (used by node-based tests). Updated .gitignore/.Rbuildignore. |
