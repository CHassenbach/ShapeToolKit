# Morph Shapes Feature Guide

## Overview

The Morph Shapes module in HaugShape v2 provides an interactive interface for:
1. **Uploading** an image for processing
2. **Previewing** the uploaded image with detailed information
3. **Splitting** the image at a specified position (horizontal or vertical)
4. **Saving** the split halves to a designated directory
5. **Morphing** the two halves together using advanced morphing algorithms

## Getting Started

### Launch the App

```r
library(HaugShapeV2)
run_haug_app()
```

Navigate to the **"2. Morph Shapes"** tab in the sidebar.

## Workflow

### Step 1: Upload Image

1. Click **"Choose an image file"** to upload your image
2. Supported formats: PNG, JPEG, JPG, TIFF, BMP
3. Once uploaded, you'll see:
   - A preview of your image
   - Image information (format, dimensions, colorspace, file size)

### Step 2: Configure Split Settings

**Split Direction:**
- **Vertical (left/right)**: Splits the image into left and right halves
- **Horizontal (top/bottom)**: Splits the image into top and bottom halves

**Split Position:**
- Slider from 0.1 to 0.9
- 0.5 = exact middle
- Lower values = split closer to left/top
- Higher values = split closer to right/bottom

**Mirror Options:**
- **None**: No mirroring applied
- **First part**: Mirror the left/top half
- **Second part**: Mirror the right/bottom half (default)
- **Both parts**: Mirror both halves

**Output Directory:**
- Choose where to save the split images
- Specify a subfolder name (auto-generated with timestamp by default)

**Split Image:**
- Click the **"Split Image"** button to process

### Step 3: Review Split Results

After splitting, you'll see:
- Preview of the first half (left or top)
- Preview of the second half (right or bottom)
- File paths where the halves were saved

### Step 4: Configure Morphing

**Morphing Method:**
- **Distance Transform**: Advanced morphing using distance transforms (recommended)
- **Linear**: Simple linear interpolation
- **Spline**: Smooth spline-based morphing

**Number of Steps:**
- How many intermediate images to generate between the two halves
- Range: 1-20 (default: 5)

**Processing Options:**
- **Binary threshold**: Controls edge detection (0-1, default: 0.1)
- **Gamma correction**: Adjusts brightness/contrast (0.1-3.0, default: 1.0)
- **Blur sigma**: Smoothing factor (0-5, default: 0)
- **Auto-align shapes**: Automatically align shapes before morphing

**Output Settings:**
- Specify a subfolder name for morphed images
- Click **"Generate Morphed Shapes"** to start

### Step 5: View Morphing Results

After morphing completes, you'll see:
- A summary of the morphing process
- Total number of images created
- Processing settings used
- An interactive slider to preview each morphing step
- Animation controls to play through the sequence

## Technical Details

### Required Packages

The module will attempt to automatically install missing packages:
- `magick`: For image manipulation and splitting
- `imager`: For advanced morphing algorithms
- `shinyFiles`: For directory chooser (optional)

### File Organization

```
output_directory/
├── split_YYYYMMDD_HHMMSS/
│   ├── image_left.png
│   └── image_right.png
└── morphed_YYYYMMDD_HHMMSS/
    ├── morph_1.png
    ├── morph_2.png
    ├── morph_3.png
    ├── morph_4.png
    └── morph_5.png
```

### Morphing Algorithms

**Distance Transform Method:**
- Computes distance transforms of both images
- Blends the distance fields using specified interpolation
- Most effective for binary or high-contrast images

**Linear Method:**
- Simple pixel-by-pixel linear interpolation
- Fastest but may produce less smooth results

**Spline Method:**
- Uses cubic spline interpolation for smoother transitions
- Good balance between quality and speed

## Tips and Best Practices

1. **Image Quality**: Higher resolution images produce better results
2. **Binary Images**: Work best with distance transform morphing
3. **Split Position**: Experiment with different positions for unique effects
4. **Mirroring**: Use mirroring to create symmetric morphs
5. **Steps**: More steps = smoother animation but more files
6. **Threshold**: Adjust if morphed images appear too light or dark
7. **Gamma**: Use to enhance or reduce contrast in morphed images

## Troubleshooting

**"Package 'magick' is required"**
- Install manually: `install.packages("magick")`

**"Package 'imager' is required"**
- Install manually: `install.packages("imager")`

**Split image is blank:**
- Check split position isn't too extreme (0.1-0.9 recommended)
- Ensure original image has content in both halves

**Morphed images look incorrect:**
- Adjust binary threshold value
- Try different morphing methods
- Ensure split images are valid

**Cannot write to output directory:**
- Check folder permissions
- Choose a different output directory

## Examples

### Basic Workflow
```r
# Launch app
run_haug_app()

# In the app:
# 1. Upload "butterfly.png"
# 2. Select "Vertical" split at position 0.5
# 3. Mirror "Second part"
# 4. Click "Split Image"
# 5. Use "Distance Transform" with 5 steps
# 6. Click "Generate Morphed Shapes"
# 7. Use slider to view the morphing sequence
```

### Advanced Morphing
```r
# For high-quality results:
# - Use "Spline" method
# - Set 10-15 morphing steps
# - Adjust gamma to 1.2 for enhanced contrast
# - Enable auto-align
# - Set blur sigma to 0.5 for smoother edges
```

## Related Functions

You can also use the underlying functions programmatically:

```r
# Split images
result <- split_image(
  input_paths = "image.png",
  output_dir = "output/",
  split_options = list(
    direction = "vertical",
    split_position = 0.5,
    mirror_parts = "second"
  )
)

# Morph shapes
morph_result <- morph_shapes(
  input_paths = c("left.png", "right.png"),
  output_dir = "morphed/",
  morphing_options = list(
    method = "distance_transform",
    n_steps = 5
  )
)
```

## Support

For issues or questions:
- Check function documentation: `?morph_shapes_ui`
- Review examples: `?morph_shapes`
- Report bugs on GitHub

---

**Version**: HaugShape v2
**Last Updated**: December 2025
