# Guide for Publishing Research with HaugShapeV2

## Quick Reference for Researchers

This guide provides citation information for researchers publishing papers that use the HaugShapeV2 package.

## Essential Citations

### 1. Cite HaugShapeV2

**Always cite the package itself:**

```
Hassenbach, C. (2025). HaugShapeV2: Morphometric Shape Analysis and 
  Visualization Tools. R package version 0.1.0. 
  https://github.com/TestoKlaus/HaugShape_v2
```

### 2. Cite Momocs (REQUIRED)

**HaugShapeV2 incorporates substantial code from Momocs - this citation is mandatory:**

```
Bonhomme, V., Picq, S., Gaucherel, C., & Claude, J. (2014). 
  Momocs: Outline Analysis Using R. Journal of Statistical Software, 
  56(13), 1-24. https://doi.org/10.18637/jss.v056.i13
```

### 3. Cite Methodological References

**If you use Elliptical Fourier Analysis (EFA):**

```
Claude, J. (2008). Morphometrics with R. Springer-Verlag, New York.
```

**If you use Principal Component Analysis on shape data:**

```
Claude, J. (2008). Morphometrics with R. Springer-Verlag, New York.
```

## Example Methods Section

Here's a complete example of how to describe your methods in a paper:

> "Morphometric analyses were conducted using HaugShapeV2 version 0.1.0 (Hassenbach, 2025), an R package for morphometric shape analysis that incorporates Elliptical Fourier Analysis functions adapted from the Momocs package (Bonhomme et al., 2014). Shape outlines were extracted and analyzed using Elliptical Fourier descriptors following the methods described in Claude (2008). Principal Component Analysis was performed on the Fourier coefficients to visualize morphological variation. Data visualization was performed using ggplot2 (Wickham, 2016). All analyses were conducted in R version 4.x.x (R Core Team, 2025)."

## Example References Section

```
Bonhomme, V., Picq, S., Gaucherel, C., & Claude, J. (2014). 
  Momocs: Outline Analysis Using R. Journal of Statistical Software, 
  56(13), 1-24. https://doi.org/10.18637/jss.v056.i13

Claude, J. (2008). Morphometrics with R. Springer-Verlag, New York.

Hassenbach, C. (2025). HaugShapeV2: Morphometric Shape Analysis and 
  Visualization Tools. R package version 0.1.0. 
  https://github.com/TestoKlaus/HaugShape_v2

R Core Team (2025). R: A language and environment for statistical computing. 
  R Foundation for Statistical Computing, Vienna, Austria. 
  https://www.R-project.org/

Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. 
  Springer-Verlag New York. ISBN 978-3-319-24277-4.
```

## Optional Citations (Choose Relevant Ones)

### If you extensively use the Shiny interface:

```
Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y., 
  Allen, J., McPherson, J., Dipert, A., & Borges, B. (2023). 
  shiny: Web Application Framework for R. R package. 
  https://CRAN.R-project.org/package=shiny
```

### If you use the morphing features:

```
Ooms, J. (2023). magick: Advanced Graphics and Image-Processing in R. 
  R package. https://CRAN.R-project.org/package=magick
```

### If you create interactive plots:

```
Sievert, C. (2020). Interactive Web-Based Data Visualization with R, 
  plotly, and shiny. Chapman and Hall/CRC. ISBN 9781138331457.
```

## Getting Citations in R

You can get properly formatted citations directly in R:

```r
# Get citation for HaugShapeV2
citation("HaugShapeV2")

# Get citation for R itself
citation()

# Get citations for specific packages
citation("ggplot2")
citation("dplyr")
citation("shiny")
```

## License Information

**HaugShapeV2 License:** MIT License

**Important Note:** HaugShapeV2 incorporates code from Momocs, which is licensed under GPL-2 | GPL-3. This means the adapted code retains its original GPL license terms. Users should be aware that GPL requires derivative works to also be distributed under GPL-compatible terms.

## BibTeX Format

For LaTeX/BibTeX users:

```bibtex
@Manual{haugshape2025,
  title = {HaugShapeV2: Morphometric Shape Analysis and Visualization Tools},
  author = {Colin Hassenbach},
  year = {2025},
  note = {R package version 0.1.0},
  url = {https://github.com/TestoKlaus/HaugShape_v2},
}

@Article{bonhomme2014momocs,
  title = {Momocs: Outline Analysis Using {R}},
  author = {Vincent Bonhomme and Sandrine Picq and Cedric Gaucherel and Julien Claude},
  journal = {Journal of Statistical Software},
  year = {2014},
  volume = {56},
  number = {13},
  pages = {1--24},
  doi = {10.18637/jss.v056.i13},
  url = {https://www.jstatsoft.org/v56/i13/},
}

@Book{claude2008morphometrics,
  title = {Morphometrics with {R}},
  author = {Julien Claude},
  year = {2008},
  publisher = {Springer-Verlag},
  address = {New York},
  isbn = {978-0-387-77789-4},
}
```

## Summary Checklist

When preparing your manuscript, make sure you:

- [ ] Cite HaugShapeV2 (Hassenbach, 2025)
- [ ] Cite Momocs (Bonhomme et al., 2014) - **REQUIRED**
- [ ] Cite Claude (2008) if using EFA methods
- [ ] Cite R itself (R Core Team)
- [ ] Cite other relevant packages (ggplot2, shiny, etc.)
- [ ] Describe the version numbers of software used
- [ ] Mention specific methods/algorithms used

## Questions?

For questions about citations, attribution, or licensing:
- Email: c.hassenbach@gmail.com
- GitHub Issues: https://github.com/TestoKlaus/HaugShape_v2/issues
- Full attribution details: See `ATTRIBUTION.md` in the package repository

## Acknowledgment Example

If you want to include an acknowledgment section:

> "We thank Colin Hassenbach for developing the HaugShapeV2 package, and the developers of Momocs (Vincent Bonhomme, Sandrine Picq, Cedric Gaucherel, and Julien Claude) for creating the foundational morphometric analysis tools."

---

**Last Updated:** December 2025  
**Package Version:** 0.1.0
