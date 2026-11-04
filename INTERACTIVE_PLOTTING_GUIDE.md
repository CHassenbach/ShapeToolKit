# Interactive Plotting Guide

## Overview

The interactive plotting feature allows you to explore morphospace in real-time with hover-based shape reconstruction. When you hover over the plot:
- **Over data points**: See the specimen ID and actual shape (if available)
- **Over empty morphospace**: See a reconstructed hypothetical shape at those PC coordinates

## Requirements

- `plotly` package (already included in dependencies)
- PCA model CSV files (automatically created by `shape_analysis()`)

## Usage in Shiny App

### 1. Run Shape Analysis

First, analyze your shapes using the **Shape Analysis** module:

```r
# This automatically creates:
# - shape_analysis.xlsx (PC scores)
# - shape_analysis_pca_rotation.csv (PCA model)
# - shape_analysis_pca_center.csv (PCA model)
# - shape_analysis_pca_sdev.csv (PCA model)
# - shape_analysis_reconstruction_metadata.txt (PCA model)
```

### 2. Import Data

In the **Data Import** module, load your `shape_analysis.xlsx` file.

### 3. Enable Interactive Mode

In the **Plotting** module:

1. Configure your plot as usual (X/Y columns, styling, features, etc.)
2. Expand the **Interactive Mode** box
3. Check **"Enable Interactive Mode"**
4. Verify that the PCA model status shows ✓ (model loaded successfully)
5. Optionally adjust the preview shape size (200-800 pixels)
6. Click **"Render plot"**

### 4. Explore the Morphospace

Once rendered:

- The plot will appear in the **Interactive Plot** section
- Hover over data points to see:
  - Specimen ID
  - PC coordinates
  - Actual shape from your data (if shape column exists)
  
- Hover over empty morphospace areas to see:
  - PC coordinates
  - Reconstructed hypothetical shape at that location
  - Shape updates in real-time as you move your mouse

The **Shape Preview** panel below the plot shows:
- The shape at your current hover position
- Information about whether it's a data point or reconstructed shape
- PC coordinates

## Usage in R Scripts (Standalone)

### Switching 3D colouring modes offline

In **Data mapping**, enable **3D Mode** and select **Additional 3D colouring
modes**, for example genus, subfamily and bioregion. Click **Render plot**.
The **Colour by** dropdown above the plot switches point colours, the legend
and enabled 3D hulls together. It also works in the expanded plot window and
exported HTML without Shiny, R or internet access.
The dropdown and **Clear all** / **Show all** share a wrapping toolbar that
reserves space above the plot, including in narrow windows.

The original grouping (or gradient) remains available. Extra modes use automatic
colours and the configured hull type, opacity and wireframe settings; hulls are
computed for all groups in each additional mode. Groups with insufficient points
cannot form hulls. Missing or blank grouping values appear as a separate
**(Missing)** group. Switching does not add specimens excluded by the original
group selection or invalid axis coordinates. The camera view and specimen
details are preserved, and **Clear all** / **Show all** apply to the current mode.

Only select columns suitable for sharing: their group values are embedded in
the HTML. Each mode adds point traces and precomputed hulls, increasing file size;
specimen shapes and metadata are shared, not duplicated for each mode.

For scripts, add `features = list(colour_groups = list(columns =
c("subfamily", "bioregion")))` to a 3D `shape_plot()` call.

### Clearing and restoring 3D plot components

Every 3D scatter plot has **Clear all** and **Show all** buttons above the plot.
**Clear all** hides all points, hulls, wireframes, and gap surfaces without
deleting data or resetting plot settings. The axes and legend remain available;
click a legend entry to show an individual component, or **Show all** to show
every component again. These controls also work in the expanded plot window
and exported interactive HTML, without Shiny or internet access.

### Standalone 3D specimen viewer (no reconstruction)

1. Import your specimen table and use **Map Shapes to Data** to match shape
   filenames to specimen IDs. Keep the mapped shape list-column in the data.
   A progress bar shows the current row and advances for imported, missing,
   and failed shapes while mapping runs.
2. In **Plotting**, enable **3D Mode** and select the three plot axes.
3. In **3D Specimen Viewer**, enable **Enable specimen details on click**.
   Select the mapped shape column and your ID, genus, species, latitude,
   longitude, and accession ID columns. The optional accession ID column holds
   the original collector's specimen identifier and is displayed as plain text,
   without external links. Optional extra columns appear in the details.
   If species already contains the full name, leave the genus column unselected.
   All mappings are optional: leave the shape column unselected to view points
   and details without outlines. Missing or empty values are marked unavailable
   and do not prevent rendering. The location map needs both latitude and longitude.
4. Click **Render plot**. Click a specimen marker to show its outline, species,
   metadata, and a map of its collection location in the left-hand panel.
   The legend stays accessible on the right to toggle point groups.
   Hulls do not select specimens.
5. Under the 3D plot, choose an export folder and filename, then click
   **Export as Interactive HTML**. Share the resulting single HTML file.

The viewer turns off reconstruction mode. Shape outlines and selected metadata
and an Equal Earth world map are embedded and work without Shiny, R, or internet.
The selected specimen's coordinates are marked on this offline map.
Only selected metadata columns and
displayed specimens are embedded; check that they are suitable for sharing.
Missing or invalid shapes and locations are explicitly identified in the panel.
Coordinates must be decimal degrees, with latitude in [-90, 90] and longitude
in [-180, 180], including polar locations. The map uses public-domain Natural
Earth 1:110m boundaries and is intended for global context, not street-level detail.
Self-contained HTML export requires Pandoc (included with RStudio).

For scripts, use the same viewer through `features$specimen_viewer`:

```r
data_with_shapes <- map_shapes_to_data(
  specimen_data, id_col = "specimen_id", shape_folder = "shapes"
)
plot <- shape_plot(
  data_with_shapes, "PC1", "PC2", z_col = "PC3",
  group_col = "Genus",
  features = list(specimen_viewer = list(
    show = TRUE, shape_col = "shape", id_col = "specimen_id",
    genus_col = "Genus", species_col = "Species",
    latitude_col = "latitude", longitude_col = "longitude",
    accession_col = "accession_id", metadata_cols = c("country", "locality")
  ))
)
htmlwidgets::saveWidget(plot, "specimens_3d.html", selfcontained = TRUE)
```

### Reconstruction-based interactive plots

You can also create interactive plots outside Shiny:

```r
library(ShapeToolKit)

# Load your data
data <- readxl::read_xlsx("shape_analysis.xlsx")

# Load PCA model
pca_model <- load_pca_model_for_plotting("shape_analysis.xlsx")

# Create interactive plot
plot <- shape_plot(
  data = data,
  x_col = "PC1",
  y_col = "PC2",
  group_col = "species",
  interactive = TRUE,
  pca_model = pca_model,
  features = list(
    hulls = list(show = TRUE)
  )
)

# Display in viewer (RStudio)
plot

# Save as HTML widget
htmlwidgets::saveWidget(plot, "interactive_morphospace.html")
```

**Note**: Standalone interactive plots show IDs on hover but do not support real-time reconstruction (Shiny-only feature). Use the Shiny app for full interactivity.

## Performance

Shape reconstruction is extremely fast:
- Matrix multiplication: ~40 × 10 = 400 operations
- Inverse Fourier transform: ~10 harmonics × 120 points = 1,200 calculations
- **Total time: < 1ms per shape** on modern hardware

This means reconstruction happens instantly as you move your mouse across the morphospace!

## Tips

1. **Missing PCA model?** Make sure the CSV files are in the same directory as your Excel file with the same base name.

2. **Reconstruction looks wrong?** Check that:
   - The correct Fourier method is detected (efourier, rfourier, etc.)
   - The number of harmonics matches your original analysis
   - You're using data from the same shape analysis that created the model

3. **Want to adjust other PCs?** Currently, reconstruction sets PC3+ to 0 (mean). Future versions may add sliders to adjust these while hovering.

4. **Performance issues?** Try:
   - Reducing the number of points in your dataset
   - Lowering the preview shape size
   - Disabling other features (hulls, contours) while in interactive mode

## Limitations

- Interactive reconstruction requires PCA model CSV files (created by `shape_analysis()`)
- Currently reconstructs with PC3+ at 0 (mean values)
- Full reconstruction features only available in Shiny app
- Standalone plotly plots show IDs but don't reconstruct shapes

## Technical Details

### Data Flow

```
User hovers → plotly_hover event → Extract PC1, PC2 coordinates
    ↓
Check if over data point or empty space
    ↓
If data point: Extract shape from data$shape column
If empty space: Reconstruct shape from PCA model
    ↓
PC coordinates → PCA rotation → Fourier coefficients → Inverse Fourier → Shape outline
    ↓
Display in Shape Preview panel
```

### Reconstruction Formula

```r
# 1. Construct PC scores vector (PC1, PC2 from hover, PC3+ = 0)
pc_scores <- c(PC1 = hover_x, PC2 = hover_y, PC3 = 0, ...)

# 2. Reconstruct Fourier coefficients
contribution <- pc_scores %*% t(pca_model$rotation)
reconstructed_coefs <- pca_model$center + contribution

# 3. Split into harmonics
coef_list <- coeff_split(reconstructed_coefs, cph = 4)  # 4 for efourier

# 4. Inverse Fourier transform
coords <- efourier_i(coef_list, nb.h = n_harmonics, nb.pts = 120)
```

## Future Enhancements

Planned features:
- [ ] Sliders for PC3-PC10 to adjust while hovering
- [ ] Click to "lock" a reconstructed shape for comparison
- [ ] Batch export of shapes along morphospace grid
- [ ] Animation along PC axes
- [ ] Support for reconstructing at specific variance percentiles
- [ ] Custom color schemes for reconstructed vs. data shapes

## Troubleshooting

| Issue | Solution |
|-------|----------|
| "No PCA model loaded" warning | Ensure CSV files exist alongside your data file |
| Shapes appear garbled | Check that Fourier method matches (efourier, rfourier, etc.) |
| Hover not working | Verify plotly is installed: `install.packages("plotly")` |
| Reconstruction is slow | This shouldn't happen (<1ms). Check for large dataset or disable other features |
| Interactive mode checkbox missing | Update to latest version of ShapeToolKit |

## See Also

- [SHAPE_RECONSTRUCTION_GUIDE.md](SHAPE_RECONSTRUCTION_GUIDE.md) - Batch reconstruction module
- [CSV_RECONSTRUCTION_IMPLEMENTATION.md](CSV_RECONSTRUCTION_IMPLEMENTATION.md) - Technical details of CSV model format
- [MORPH_SHAPES_GUIDE.md](MORPH_SHAPES_GUIDE.md) - General morphometric analysis workflow
