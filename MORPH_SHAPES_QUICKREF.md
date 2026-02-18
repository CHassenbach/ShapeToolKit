# Morph Shapes Quick Reference

## Launch App
```r
library(HaugShapeV2)
run_haug_app()
```
Navigate to **"2. Morph Shapes"** tab

---

## Quick Workflow

### 1️⃣ Upload Image
- Click "Choose an image file"
- Formats: PNG, JPEG, JPG, TIFF, BMP

### 2️⃣ Split Configuration
| Setting | Options | Default |
|---------|---------|---------|
| **Direction** | Vertical / Horizontal | Vertical |
| **Position** | 0.1 - 0.9 | 0.5 (middle) |
| **Mirror** | None / First / Second / Both | Second |

### 3️⃣ Morphing Settings
| Parameter | Range | Default | Purpose |
|-----------|-------|---------|---------|
| **Method** | Distance Transform, Linear, Spline | Distance Transform | Algorithm quality |
| **Steps** | 1 - 20 | 5 | Number of intermediate images |
| **Threshold** | 0 - 1 | 0.1 | Edge detection sensitivity |
| **Gamma** | 0.1 - 3.0 | 1.0 | Brightness correction |
| **Blur** | 0 - 5 | 0 | Smoothing amount |

---

## Common Use Cases

### Symmetric Shape Completion
```
Split: Vertical, Position: 0.5
Mirror: Second
Method: Distance Transform
```

### Gradual Transition
```
Steps: 10-15
Method: Spline
Blur: 0.5
```

### High Contrast Morphing
```
Method: Distance Transform
Threshold: 0.2
Gamma: 1.5
```

---

## Keyboard Shortcuts in Preview

- **Space**: Play/Pause animation
- **Left/Right Arrow**: Previous/Next step

---

## Output Structure
```
output_directory/
├── split_[timestamp]/
│   ├── image_left.png
│   └── image_right.png
└── morphed_[timestamp]/
    ├── morph_1.png
    ├── morph_2.png
    └── ...
```

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| Package error | `install.packages(c("magick", "imager"))` |
| Blank images | Adjust split position to 0.3-0.7 |
| Dark results | Increase threshold to 0.2-0.3 |
| Rough edges | Add blur (0.5-1.0) |

---

## Programmatic Usage

### Split Only
```r
split_image(
  input_paths = "image.png",
  output_dir = "output/",
  split_options = list(
    direction = "vertical",
    split_position = 0.5
  )
)
```

### Morph Only
```r
morph_shapes(
  input_paths = c("left.png", "right.png"),
  output_dir = "morphed/",
  morphing_options = list(
    method = "distance_transform",
    n_steps = 5
  )
)
```

---

## Tips
✓ Higher resolution = better quality  
✓ Binary images work best  
✓ Experiment with different methods  
✓ Use animation to review results  
✓ Save successful settings for future use  

---

**Full Guide**: See `MORPH_SHAPES_GUIDE.md`  
**Documentation**: `?morph_shapes_ui`
