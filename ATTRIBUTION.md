# Attribution and Licensing Information for HaugShapeV2

## Package Information

**HaugShapeV2** (version 0.1.0)  
**Author & Maintainer:** Colin Hassenbach <c.hassenbach@gmail.com>  
**License:** MIT License  
**Repository:** https://github.com/TestoKlaus/HaugShape_v2

## How to Cite HaugShapeV2

If you use HaugShapeV2 in publications, please cite:

> Hassenbach, C. (2025). HaugShapeV2: Morphometric Shape Analysis and Visualization Tools. R package version 0.1.0. https://github.com/TestoKlaus/HaugShape_v2

You can also retrieve citation information in R:
```r
citation("HaugShapeV2")
```

## Attribution for Adapted Code

### Momocs Package

HaugShapeV2 incorporates substantial code adapted from the **Momocs** package (https://github.com/MomX/Momocs). The following files contain code derived from or substantially based on Momocs:

- `R/momocs_babel-bridges.R`
- `R/momocs_babel-import.R`
- `R/momocs_cl-def-Out.R`
- `R/momocs_cl-helpers.R`
- `R/momocs_coo-shapedescriptors.R`
- `R/momocs_coo-utilities.R`
- `R/momocs_core-calibrate.R`
- `R/momocs_core-out-efourier.R`
- `R/momocs_core-utils.R`
- `R/momocs_gr-morphospaces.R`
- `R/momocs_gr-morphospaces2.R`
- `R/momocs_gr-PCA.R`
- `R/momocs_mult-PCA.R`
- `R/momocs_pkg-internals.R`

**Please cite Momocs when using HaugShapeV2:**

> Bonhomme, V., Picq, S., Gaucherel, C., & Claude, J. (2014). Momocs: Outline Analysis Using R. Journal of Statistical Software, 56(13), 1-24. https://doi.org/10.18637/jss.v056.i13

**Momocs License:** GPL-2 | GPL-3  
**Momocs Repository:** https://github.com/MomX/Momocs  
**Momocs Authors:** Vincent Bonhomme, Sandrine Picq, Cedric Gaucherel, Julien Claude

### Additional References

For Elliptical Fourier Analysis methods, also cite:

> Claude, J. (2008). Morphometrics with R. Springer-Verlag, New York. ISBN: 978-0-387-77789-4

## Required R Package Dependencies

HaugShapeV2 depends on the following R packages. When publishing research using HaugShapeV2, consider citing the packages most relevant to your analysis:

### Core Dependencies

- **ggplot2** - Grammar of graphics plotting
  - Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. ISBN 978-3-319-24277-4, https://ggplot2.tidyverse.org
  
- **dplyr** - Data manipulation
  - Wickham, H., François, R., Henry, L., & Müller, K. (2023). dplyr: A Grammar of Data Manipulation. R package. https://CRAN.R-project.org/package=dplyr

- **tidyr** - Data tidying
  - Wickham, H., Vaughan, D., & Girlich, M. (2023). tidyr: Tidy Messy Data. R package. https://CRAN.R-project.org/package=tidyr

- **scales** - Scale functions for visualization
  - Wickham, H., & Seidel, D. (2022). scales: Scale Functions for Visualization. R package. https://CRAN.R-project.org/package=scales

- **magrittr** - Pipe operator
  - Bache, S. M., & Wickham, H. (2022). magrittr: A Forward-Pipe Operator for R. R package. https://CRAN.R-project.org/package=magrittr

### Data I/O

- **readxl** - Read Excel files
  - Wickham, H., & Bryan, J. (2023). readxl: Read Excel Files. R package. https://CRAN.R-project.org/package=readxl

- **openxlsx** - Write Excel files
  - Schauberger, P., & Walker, A. (2023). openxlsx: Read, Write and Edit xlsx Files. R package. https://CRAN.R-project.org/package=openxlsx

### Image Processing

- **magick** - Advanced image processing
  - Ooms, J. (2023). magick: Advanced Graphics and Image-Processing in R. R package. https://CRAN.R-project.org/package=magick

- **jpeg** - JPEG image I/O
  - Urbanek, S. (2022). jpeg: Read and write JPEG images. R package. https://CRAN.R-project.org/package=jpeg

### Shiny Application

- **shiny** - Interactive web applications
  - Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y., Allen, J., McPherson, J., Dipert, A., & Borges, B. (2023). shiny: Web Application Framework for R. R package. https://CRAN.R-project.org/package=shiny

- **shinydashboard** - Dashboard framework
  - Chang, W., & Borges Ribeiro, B. (2021). shinydashboard: Create Dashboards with 'Shiny'. R package. https://CRAN.R-project.org/package=shinydashboard

- **shinyFiles** - File system browser
  - Pedersen, T. L., Nijs, V., & Schaffner, D. W. (2023). shinyFiles: A Server-Side File System Viewer For Shiny. R package. https://CRAN.R-project.org/package=shinyFiles

- **shinyWidgets** - Custom widgets
  - Perrier, V., Meyer, F., & Granjon, D. (2023). shinyWidgets: Custom Inputs Widgets for Shiny. R package. https://CRAN.R-project.org/package=shinyWidgets

- **shinycssloaders** - Loading animations
  - Sali, A., & Attali, D. (2020). shinycssloaders: Add Loading Animations to a 'shiny' Output While It's Recalculating. R package. https://CRAN.R-project.org/package=shinycssloaders

- **colourpicker** - Color picker widget
  - Attali, D. (2021). colourpicker: A Colour Picker Tool for Shiny and for Selecting Colours in Plots. R package. https://CRAN.R-project.org/package=colourpicker

- **DT** - Interactive data tables
  - Xie, Y., Cheng, J., & Tan, X. (2023). DT: A Wrapper of the JavaScript Library 'DataTables'. R package. https://CRAN.R-project.org/package=DT

- **plotly** - Interactive plots
  - Sievert, C. (2020). Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC. ISBN 9781138331457, https://plotly-r.com

### Base R Packages

HaugShapeV2 also uses the following base R packages that are part of the R distribution:
- **grDevices** - Graphics devices
- **graphics** - Base graphics
- **stats** - Statistical functions
- **utils** - Utility functions

## License Compatibility

HaugShapeV2 is released under the MIT License, which is compatible with:
- GPL-2 and GPL-3 (used by Momocs)
- All permissive licenses used by the dependencies listed above

The MIT License allows commercial use, modification, distribution, and private use, with the requirement that the license and copyright notice be included with any distributions.

## Full MIT License Text

```
MIT License

Copyright (c) 2025 HaugShape

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## Momocs License Information

The Momocs package code adapted in HaugShapeV2 is licensed under GPL-2 | GPL-3:

- **GPL-2:** https://www.gnu.org/licenses/old-licenses/gpl-2.0.html
- **GPL-3:** https://www.gnu.org/licenses/gpl-3.0.html

The GPL licenses require that derivative works also be distributed under GPL terms. Since HaugShapeV2 incorporates GPL-licensed code, it effectively operates under GPL compatibility requirements.

## Acknowledgments

We are grateful to the Momocs development team, especially Vincent Bonhomme, for creating and maintaining the excellent Momocs package that forms the foundation of many morphometric analyses in HaugShapeV2.

We also thank all the developers and maintainers of the R packages that HaugShapeV2 depends on.

## For Package Authors Publishing Papers

When publishing research that uses HaugShapeV2, we recommend:

1. **Cite HaugShapeV2** using the citation information provided above
2. **Cite Momocs** (Bonhomme et al., 2014) as core functionality is derived from this package
3. **Cite specific methodological papers** such as Claude (2008) for Elliptical Fourier Analysis
4. **Consider citing key dependencies** that are central to your analysis (e.g., ggplot2 for visualizations, shiny for interactive applications)

### Example Acknowledgment in Methods Section

> "Morphometric analyses were performed using the HaugShapeV2 package (Hassenbach, 2025) in R (R Core Team, 2025), which incorporates Elliptical Fourier Analysis functions adapted from the Momocs package (Bonhomme et al., 2014). Data visualization was performed using ggplot2 (Wickham, 2016)."

### Example References Section

```
Bonhomme, V., Picq, S., Gaucherel, C., & Claude, J. (2014). Momocs: Outline Analysis 
  Using R. Journal of Statistical Software, 56(13), 1-24. 
  https://doi.org/10.18637/jss.v056.i13

Claude, J. (2008). Morphometrics with R. Springer-Verlag, New York.

Hassenbach, C. (2025). HaugShapeV2: Morphometric Shape Analysis and Visualization Tools. 
  R package version 0.1.0. https://github.com/TestoKlaus/HaugShape_v2

R Core Team (2025). R: A language and environment for statistical computing. 
  R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/

Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York.
```

## Getting Citation Information in R

To get properly formatted citations in R:

```r
# Cite HaugShapeV2
citation("HaugShapeV2")

# Cite Momocs
citation("Momocs")

# Cite other packages
citation("ggplot2")
citation("dplyr")
# etc.

# Cite R itself
citation()
```

## Contact

For questions about attribution, licensing, or collaboration:
- **Email:** c.hassenbach@gmail.com
- **Issues:** https://github.com/TestoKlaus/HaugShape_v2/issues

---

Last updated: December 2025
