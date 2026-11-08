# News

## ShapeToolKit 0.1.0

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