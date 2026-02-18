# Morph Shapes Workflow Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    MORPH SHAPES MODULE WORKFLOW                  │
└─────────────────────────────────────────────────────────────────┘

┌───────────────────┐
│  1. UPLOAD IMAGE  │
│                   │
│  • Choose file    │
│  • View preview   │
│  • Check info     │
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│  2. CONFIGURE     │
│     SPLIT         │
│                   │
│  • Direction      │
│  • Position       │
│  • Mirroring      │
│  • Output dir     │
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│  3. SPLIT IMAGE   │
│                   │
│  [Process]        │
└─────────┬─────────┘
          │
          ▼
┌───────────────────────────────────┐
│  4. SPLIT RESULTS                 │
│                                   │
│  ┌──────────┐    ┌──────────┐   │
│  │  FIRST   │    │  SECOND  │   │
│  │   HALF   │    │   HALF   │   │
│  └──────────┘    └──────────┘   │
│                                   │
│  Saved to: output_dir/split_*/   │
└─────────┬─────────────────────────┘
          │
          ▼
┌───────────────────┐
│  5. CONFIGURE     │
│     MORPHING      │
│                   │
│  • Method         │
│  • Steps          │
│  • Threshold      │
│  • Gamma/Blur     │
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│  6. GENERATE      │
│     MORPHS        │
│                   │
│  [Process]        │
└─────────┬─────────┘
          │
          ▼
┌───────────────────────────────────────────────────┐
│  7. MORPHING RESULTS                              │
│                                                   │
│  ┌────┐  ┌────┐  ┌────┐  ┌────┐  ┌────┐        │
│  │ 1  │→ │ 2  │→ │ 3  │→ │ 4  │→ │ 5  │        │
│  └────┘  └────┘  └────┘  └────┘  └────┘        │
│                                                   │
│  Interactive preview with animation              │
│  Saved to: output_dir/morphed_*/                │
└───────────────────────────────────────────────────┘
```

---

## Split Direction Examples

### VERTICAL SPLIT (Left/Right)
```
┌─────────────────┐       ┌────────┐  ┌────────┐
│                 │       │        │  │        │
│                 │  -->  │  LEFT  │  │ RIGHT  │
│   FULL IMAGE    │       │  HALF  │  │  HALF  │
│                 │       │        │  │        │
└─────────────────┘       └────────┘  └────────┘
```

### HORIZONTAL SPLIT (Top/Bottom)
```
┌─────────────────┐       ┌─────────────────┐
│                 │       │    TOP HALF     │
│   FULL IMAGE    │  -->  └─────────────────┘
│                 │       ┌─────────────────┐
└─────────────────┘       │   BOTTOM HALF   │
                          └─────────────────┘
```

---

## Split Position Examples

### Position = 0.3 (30% from left/top)
```
┌────┬───────────┐
│ 30%│    70%    │
└────┴───────────┘
```

### Position = 0.5 (Middle, default)
```
┌────────┬────────┐
│  50%   │   50%  │
└────────┴────────┘
```

### Position = 0.7 (70% from left/top)
```
┌──────────────┬────┐
│     70%      │30% │
└──────────────┴────┘
```

---

## Mirroring Examples

### Original Split (No Mirror)
```
┌────────┬────────┐
│   L    │    R   │
└────────┴────────┘
```

### Mirror Second (Default)
```
┌────────┬────────┐
│   L    │    R'  │  (R flipped horizontally)
└────────┴────────┘
```

### Mirror Both
```
┌────────┬────────┐
│   L'   │    R'  │  (Both flipped)
└────────┴────────┘
```

---

## Morphing Process

### Distance Transform Method
```
Image 1          Distance         Blend          Distance         Image 2
               Transform 1                    Transform 2
   ●  ──────►    [DT1]    ──────►  [Morph]  ◄──────  [DT2]    ◄────── ■
```

### Step Sequence (n_steps = 5)
```
IMG1 ──► Step1 ──► Step2 ──► Step3 ──► Step4 ──► Step5 ──► IMG2
  0%      16.7%     33.3%     50.0%     66.7%     83.3%     100%

Alpha:  0.17       0.33       0.50       0.67       0.83
```

---

## File Organization

```
output_directory/
│
├── split_20251208_143052/           ← Split subfolder
│   ├── image_left.png               ← First half
│   └── image_right.png              ← Second half
│
└── morphed_20251208_143125/         ← Morph subfolder
    ├── morph_1.png                  ← Step 1
    ├── morph_2.png                  ← Step 2
    ├── morph_3.png                  ← Step 3
    ├── morph_4.png                  ← Step 4
    └── morph_5.png                  ← Step 5
```

---

## Parameter Effects

### Threshold
```
Low (0.1)  ────────────────  More detail, softer edges
            ↕
High (0.5) ────────────────  Less detail, sharper edges
```

### Gamma
```
< 1.0  ────────────────  Darker, more contrast
 1.0   ────────────────  No change (default)
> 1.0  ────────────────  Brighter, less contrast
```

### Blur Sigma
```
0    ────────────────  No smoothing (sharp)
0.5  ────────────────  Light smoothing
2.0  ────────────────  Heavy smoothing (very smooth)
```

### Number of Steps
```
Few (2-3)    ────────────────  Fast, abrupt transitions
Medium (5-8) ────────────────  Balanced
Many (10+)   ────────────────  Smooth, gradual transitions
```

---

## Decision Tree

```
Do you need symmetric shapes?
│
├─ YES ──► Use Vertical split at 0.5
│          Mirror: Second
│          Method: Distance Transform
│
└─ NO
    │
    Do you want smooth transitions?
    │
    ├─ YES ──► Method: Spline
    │          Steps: 10-15
    │          Blur: 0.5-1.0
    │
    └─ NO ──► Method: Linear
              Steps: 3-5
              Blur: 0
```

---

## Common Workflows

### 1. Mirror-Complete a Half Image
```
Input: Half butterfly (right side)
   ↓
Split: Vertical, Position 0.5
   ↓
Mirror: Second half
   ↓
Morph: Distance Transform, 5 steps
   ↓
Output: Complete symmetric butterfly
```

### 2. Create Transition Animation
```
Input: Circle and Square
   ↓
Upload each separately (or use pre-split)
   ↓
Morph: Spline, 20 steps
   ↓
Output: Smooth circle-to-square animation
```

### 3. Blend Two Specimens
```
Input: Specimen A and Specimen B
   ↓
Split each vertically
   ↓
Morph: Distance Transform, 10 steps
   ↓
Output: Gradual A-to-B transformation
```

---

## Quick Command Reference

### Launch
```r
run_haug_app()
```

### Test Installation
```r
source("test_morph_module.R")
```

### View Documentation
```r
?morph_shapes_ui
?split_image
?morph_shapes
```

---

**Tip**: Print this diagram for quick reference while working!
