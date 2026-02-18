# PCA Saturation Curve - Quick Start Guide

## What does it do?

Answers the question: **"Do I need to collect more specimens?"**

- ✅ **Saturated** (curve plateaus) → You have enough specimens
- ⚠️ **Unsaturated** (curve keeps rising) → More specimens would help

---

## Quick Workflow (5 Steps)

### 1️⃣ Run Shape Analysis First

1. Go to **Shape Analysis** → **Run Analysis**
2. Load your shapes and run the analysis
3. **Download the Excel file** with PC scores
4. Note where you saved it

### 2️⃣ Open Saturation Analysis

1. Go to **Shape Analysis** → **PCA Saturation Curve**
2. Click **"Select Excel file from Run Analysis"**
3. Upload the Excel file you just saved
4. You'll see: ✓ File loaded successfully (with # specimens and PCs)

### 3️⃣ Set Parameters (Optional)

**Quick settings** (can skip this step and use defaults):
- Bootstrap Iterations: **200** (default is fine)
- Minimum Sample Size: **5** (default is fine)
- Sample Size Steps: **10** (default is fine)
- Number of PCs: **10** (or all PCs if you have fewer)

**Advanced users only:**
- ☑️ Enable "Use Parallel Processing" for datasets >100 specimens
- Change Random Seed if you want different randomization

### 4️⃣ Run Analysis

1. Click **"Compute Saturation Curve"** (big green button)
2. Wait for progress bar to complete (typically 1-5 minutes)
3. You'll see a notification when done

### 5️⃣ Interpret the Plot

**Look at the curve shape:**

**Option A: Plateau (Flat at the end)**
```
Variance |     ___________
         |   /
         | /
         +---------------
            Sample Size
```
✅ **You're good!** Morphospace is saturated.  
→ Current sample size is sufficient

**Option B: Still Rising**
```
Variance |              /
         |           /
         |        /
         +---------------
            Sample Size
```
⚠️ **Need more specimens** Morphospace not saturated.  
→ Consider collecting more specimens

**Option C: Gradual Curve**
```
Variance |        ____/
         |     __/
         |  __/
         +---------------
            Sample Size
```
✓ **Mostly saturated** Approaching saturation.  
→ Current sample is likely adequate

---

## Export Results

**For your manuscript/presentation:**

1. **Adjust plot appearance:**
   - X-axis Type: "Absolute" or "Proportion"
   - Theme: "Haug" (light), "inverted_Haug" (dark), or "publication"
   - Toggle confidence intervals and points

2. **Download:**
   - **Download Plot (PDF)** → For publications (vector graphics)
   - **Download Plot (PNG)** → For presentations (high-res image)
   - **Download Data (CSV)** → For custom analysis

---

## Common Questions

**Q: How long does it take?**  
A: Usually 1-5 minutes. Depends on:
- Number of specimens (more = longer)
- Bootstrap iterations (more = longer)
- Parallel processing (enabled = faster)

**Q: What if the curve doesn't plateau?**  
A: Two possibilities:
1. You genuinely need more specimens
2. You need to increase "Number of PCs" analyzed

**Q: Can I run multiple analyses?**  
A: Yes! Run separate analyses for different:
- Species/groups
- Morphological regions
- Time periods

**Q: What's a good bootstrap iteration count?**  
A: 
- **Quick check**: 100 iterations
- **Standard**: 200 iterations (default)
- **Publication**: 500-1000 iterations

**Q: My Excel file won't load?**  
A: Make sure it's the file from "Run Analysis" with:
- First column: ID (specimen names)
- Other columns: PC1, PC2, PC3, ... (numbers)

**Q: How do I know if I'm analyzing enough PCs?**  
A: Generally:
- **Small datasets (<50 specimens)**: Use 5-10 PCs
- **Medium datasets (50-200)**: Use 10-15 PCs
- **Large datasets (>200)**: Use 15-20 PCs
- Check the scree plot from your original analysis

---

## Typical Parameter Settings

| Dataset Size | Bootstrap Iter. | Steps | PCs | Parallel |
|--------------|----------------|-------|-----|----------|
| Small (<50)  | 200            | 10    | 5-10| No       |
| Medium (50-200) | 200-500     | 10    | 10-15| Optional |
| Large (>200) | 500            | 15    | 15-20| Yes      |

---

## Example Interpretation

**Scenario:** You have 75 arthropod specimens

**Results:**
- At 50% specimens (38): Variance = 12.5 (CI: 11.8-13.2)
- At 100% specimens (75): Variance = 12.8 (CI: 12.6-13.0)

**Interpretation:**
- Small increase (12.5 → 12.8 = 2.4% increase)
- Narrow confidence intervals at 100%
- **Conclusion**: Morphospace is saturated ✓
- **Action**: Current sample size is sufficient

---

## Need More Help?

See the full guide: **PCA_SATURATION_GUIDE.md**

Covers:
- Detailed parameter explanations
- Advanced interpretation
- Troubleshooting
- Multiple use cases
- Technical background

---

**HaugShape v2** - Morphometric Analysis Toolkit  
Last updated: December 26, 2025
