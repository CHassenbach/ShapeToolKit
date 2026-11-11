# News

## ShapeToolKit 0.1.0

* Rotation video export now uses optional dependencies (`av`, `webshot2`, and
  `chromote`) instead of requiring them for every installation. Missing packages
  are reported with installation instructions; the plotting module no longer
  installs video dependencies automatically. Removed the unused `nnet` import.

* Automatic EFA harmonic selection now calibrates all specimens using at most
  100 harmonics (or the shortest outline's limit) to reduce calibration time.
  The 99 percent harmonic-power criterion is relative to this ceiling, excluding
  the first harmonic. Explicit harmonic counts are unchanged.

* Fixed Shape Analysis import messages, result metadata, and printed summaries to
  report the actual number of specimens instead of the three outline-container
  components.

* Fixed automatic EFA harmonic calibration to use every specimen rather than
  only the first three. Harmonic selection no longer depends on specimen order,
  and calibration results use the actual specimen names. Automatic counts may
  differ from previous runs; explicitly supplied harmonic counts are unchanged.

* Added an imported-outline overlay to Shape Analysis results, with optional
  common-size scaling and individual shape highlighting to inspect faulty imports.

* Added Clear saved foreground points to Shape Analysis so consecutive runs can
  use different manual selections without restarting the app. Clearing cancels
  pending clicks and clears diagnostic checks while preserving completed results.

* Shape analysis now uses random seed 42 by default for reproducible foreground
  searching, with a selectable seed in the app and the R API. Use `seed = NULL`
  in the API for the previous unseeded behavior. The seed is saved in analysis
  and reconstruction metadata without changing the caller's random state.

* Initial release of ShapeToolKit package
* Added comprehensive shape analysis functions
* Implemented Elliptical Fourier Analysis (EFA) and PCA
* Created visualization tools for hull plots, contour plots, and boxplots
* Added Shiny application for interactive analysis
* Included functions for shape preprocessing and data manipulation