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

All resolved 2026-11-11:

- [x] **License conflict**: relicense package to GPL (>= 3) AND prune the
  vendored `momocs_*.R` code down to the subset actually used.
- [x] **`Haug_panel()`**: delete.
- [x] **`cluster_plot()` / `elbow_plot()`**: keep; wire a clustering feature
  into the Statistics section of the app.

## 3. Inventory findings (2026-11-11)

### App wiring

The initial inventory inspected the now-deleted root `app.R`, not the installed
app. The authoritative `inst/app/app.R` already registers all 14 module pairs.
The following modules were absent only from the stale root copy, not orphaned
from the installed app:

**Modules missing from the former root copy**:

| Module | Purpose |
|---|---|
| `pca_saturation_module.R` | Bootstrap PCA variance saturation curves |
| `overview_module.R` | Shiny wrapper around `Haug_overview()` |
| `pc_contribution_plots_module.R` | PC contribution plots |
| `complete_shapes_module.R` | Batch completion of halved shapes |
| `morph_shapes_module.R` | Morphing/interpolation between shapes |

### Redundancy checks (user questions)

- **PCA saturation vs morphospace stability**: conceptually related but
  different analyses; no quantitative redundancy measurement has been made.
  Saturation = variance-explained vs sample size (works on a
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
- Vendored Momocs code: the preliminary static scan found few direct calls
  from non-vendored code. This does NOT establish that the remaining functions
  are unused: transitive calls, S3 dispatch, exported APIs and examples must
  be traced before pruning. The earlier approximately 97 percent estimate
  must not be used as a deletion target.
- `install_packages.R` - does global-env assignments (check NOTE); should be
  slimmed or removed (installation belongs in README/setup docs).

### Historical CRAN audit (old check logs: 0 E / 0 W / 6 NOTEs)

These are historical findings, not the current check status. See the latest
validation below. The `.cjs` files were subsequently verified to be live tests.

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
- [x] DESCRIPTION: `Authors@R` (incl. Momocs authors as ctb), remove trailing
      comma, remove `LazyData`, tighten Title/Description, add
      `Config/testthat/edition: 3`
- [x] Resolve license decision: `License: GPL (>= 3)`, deleted MIT stub,
      added LICENSE.md (full GPL-3 text, buildignored), updated ATTRIBUTION.md
- [x] Fix `man/morph_shapes.Rd` lost-braces (escaped in roxygen source)
- [x] Remove `install_packages.R` (redundant: install_github resolves Imports)
- [x] Remove non-ASCII chars from the 4 affected R files (verified 0 remain)
- [ ] Audit Imports; move optional ones (av, webshot2, chromote, ...) to
      Suggests with `requireNamespace()` guards
      - [x] Initial audit: moved av/webshot2/chromote to Suggests, removed
            unused nnet (41 -> 37 Imports), retained dependencies with active
            core-analysis/app calls. Review further reductions after pruning.
      - [x] Removed video-package auto-installation at plotting startup;
            UI and direct helper report missing packages before side effects.
      - [ ] Review remaining runtime auto-installers in other modules
            (including optional readODS paths); do not silently install.
      - [ ] Revisit alphahull optionality and its explicit fallback policy
- [x] Repair malformed stability/N-dimensional gap Rd markup and raw percent
      signs in stability CI descriptions; document shape_plot's gradient
      argument; regenerate and validate help pages
- [ ] Resolve the fresh code-analysis NOTE: missing explicit namespace imports
      and genuine global-variable/function findings (coordinate with pruning)

### Phase 3 - Dead code removal
- [x] Keep `cluster_plot.R`, `elbow_plot.R` (decision only; wiring pending in
      Phase 4)
- [x] Delete `shape_plot_momocs.R`
- [x] Delete `Haug_panel.R`
- [x] Delete deprecated `run_app.R` (R/) and `run_haug_app` alias
- [ ] Prune vendored Momocs code to the used subset (test-driven: full test
      suite + app smoke test after each pruning step)
- [ ] Reduce NAMESPACE exports to intended public API
- [ ] Remove commented-out code blocks and stale TODOs

### Phase 4 - App restructure (4 sections)
- [x] Finding: `inst/app/app.R` (launched by `run_shape_app()`) already wired
      all modules; only the stale root `app.R` dev copy had them orphaned
- [x] Rewrite `inst/app/app.R` sidebar to the 4-section target architecture
      (smoke-tested: HTTP 200, all sections render)
- [x] Single app source of truth: deleted stale root `app.R`; removed the
      root-app fallback from `run_shape_app()`
- [ ] Split data explorer: stats features stay in Statistics; distribution
      plots move to Visualization
- [ ] Rebuild Overview on plotting-module infrastructure
- [ ] Build UI for `split_image`/`cut_image_right_half` in Image Processing
      (currently programmatic API only)
- [ ] Central shared data store across tabs
- [ ] Wire clustering tab in Statistics using `cluster_plot()`/`elbow_plot()`

### Phase 5 - Unified styling
- [ ] Create `R/app_theme.R` (CSS + layout helpers)
- [ ] Migrate every module to shared helpers (consistent box usage,
      spinners, buttons, download controls, notification style)
- [ ] Consistent naming/wording in UI labels

### Phase 6 - Tests for every feature
- [ ] Triage the 67 pre-existing test warnings (suite: 779 pass / 0 fail as
      of 2026-11-11)
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
| 2026-11-11 | 2+3 | Relicensed to GPL (>= 3) (LICENSE.md, DESCRIPTION, ATTRIBUTION.md). DESCRIPTION: Authors@R with Momocs ctb credits, new Description, no LazyData, testthat edition 3, fixed Suggests comma. Fixed morph_shapes Rd braces. Removed all non-ASCII chars from R/. Deleted dead code: install_packages.R, R/run_app.R, run_haug_app alias, shape_plot_momocs.R, Haug_panel.R (+ man pages). Regenerated roxygen docs/NAMESPACE. Test suite: 779 pass / 0 fail / 2 skip / 67 warn (pre-existing). |
| 2026-11-11 | 4 | Restructured inst/app/app.R into the 4 target sections (Image Processing / Shape Analysis / Statistics / Visualization). Deleted stale root app.R duplicate; simplified run_shape_app() fallback. Smoke test passed (HTTP 200, all sections render). |
| 2026-11-11 | 2, continued | Audited dependencies: moved av/webshot2/chromote to Suggests, removed unused nnet, removed plotting startup auto-installs for video dependencies. Added clear UI/helper guards, retry checks and mocked successful video-export coverage. Updated README requirements/video instructions and corrected its stale MIT license statement; updated NEWS. |
| 2026-11-11 | 2, check follow-up | Fresh source-package check: 0 errors / 4 warnings / 2 notes; installed-package tests 846 pass / 0 fail / 67 warnings / 2 skips. Fixed doubled Rd macro backslashes in stability/N-dimensional gap docs, raw percent signs breaking stability argument parsing, and missing gradient documentation. Recheck: 0 errors / 2 warnings / 1 note; documentation, examples and dependency checks pass. Both warnings concern unbuilt vignettes; the note is code-analysis/namespace findings. Final expanded video tests: 71 pass / 0 fail / 1 environment warning. |

## 7. Latest validation and handoff (2026-11-11, continuation)

- Full installed-package suite ran via `rcmdcheck` with `--no-manual` and
  `--no-vignettes`, building with `--no-build-vignettes`: 846 passing assertions,
  no failures, 67 test warnings, 2 skips.
- After documentation repairs, reran source-package check with the same flags
  plus `--no-tests` to avoid repeating the successful suite: **0 errors,
  2 warnings, 1 note**. These results are not a CRAN-clean certification.
- Final additional retry assertions were validated with the focused video test
  file: **71 passing assertions, no failures**. Its standalone warning is that
  Shiny was built under R 4.5.3 while the active interpreter is R 4.5.1.
- Pandoc is unavailable in this environment. Vignettes and the PDF manual were
  not validated; build/check vignettes once Pandoc is available. The two check
  warnings concern the missing built `getting-started` vignette.
- The remaining NOTE lists missing explicit imports from stats/graphics/
  grDevices/utils and global references, including vendored code. Fix real
  namespace issues, rather than suppressing them with blanket globals.
- The successful video-export test mocks browser rendering and encoding; a real
  browser/MP4 export still needs a manual feature walkthrough.
- Next: trace vendored functions transitively (including S3 methods and public
  APIs) before any deletion; address namespace findings on the retained code.
  Remaining runtime auto-installers, app feature wiring, styling and broader
  feature coverage remain open in the phases above.
