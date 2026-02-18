# Installation and Verification Checklist

Use this checklist to verify that the Morph Shapes feature has been successfully integrated into your HaugShape v2 package.

---

## ✅ Pre-Installation Checklist

- [ ] R version 4.0.0 or higher installed
- [ ] RStudio installed (recommended)
- [ ] Internet connection available (for package installation)
- [ ] Write permissions in your R library directory

---

## ✅ File Verification

### New Files Created
- [ ] `R/morph_shapes_module.R` exists
- [ ] `man/morph_shapes_module.Rd` exists
- [ ] `MORPH_SHAPES_GUIDE.md` exists
- [ ] `MORPH_SHAPES_QUICKREF.md` exists
- [ ] `WORKFLOW_DIAGRAM.md` exists
- [ ] `IMPLEMENTATION_SUMMARY.md` exists
- [ ] `test_morph_module.R` exists

### Modified Files
- [ ] `inst/app/app.R` includes morph_shapes tab
- [ ] `NAMESPACE` exports morph_shapes_ui and morph_shapes_server
- [ ] `README.md` mentions the new feature

---

## ✅ Package Installation

### Step 1: Reload Package
```r
# If using devtools
devtools::load_all()

# Or reinstall
devtools::install()
```
- [ ] Package loads without errors
- [ ] No warning messages appear

### Step 2: Check Exports
```r
library(HaugShapeV2)

# Check that functions are available
exists("morph_shapes_ui")
exists("morph_shapes_server")
exists("morph_shapes")
exists("split_image")
```
- [ ] All functions return `TRUE`

### Step 3: Check Dependencies
```r
# Run the test script
source("test_morph_module.R")
```
- [ ] All required packages are available or install successfully
- [ ] Test completes without fatal errors

---

## ✅ Dependency Installation

### Core Dependencies (Required)
```r
install.packages(c("magick", "imager"))
```
- [ ] `magick` installs successfully
- [ ] `imager` installs successfully

### Optional Dependencies
```r
install.packages("shinyFiles")
```
- [ ] `shinyFiles` installs (or fallback works)

### Test Package Loading
```r
library(magick)
library(imager)
```
- [ ] Both load without errors

---

## ✅ Shiny App Verification

### Step 1: Launch App
```r
library(HaugShapeV2)
run_haug_app()
```
- [ ] App launches in browser/viewer
- [ ] No errors in R console

### Step 2: Check UI Elements
- [ ] Sidebar shows "2. Morph Shapes" menu item
- [ ] Can navigate to Morph Shapes tab
- [ ] Tab displays without errors

### Step 3: Verify UI Components
On the Morph Shapes tab, verify these elements are visible:
- [ ] "1. Upload Image" box
- [ ] File input button
- [ ] "2. Split Configuration" box
- [ ] Direction dropdown
- [ ] Split position slider
- [ ] Mirror options radio buttons
- [ ] Output directory chooser
- [ ] "Split Image" button

---

## ✅ Functional Testing

### Test 1: Image Upload
1. Click "Choose an image file"
2. Select any PNG/JPG image
- [ ] Image preview appears
- [ ] Image information displays
- [ ] No error messages

### Test 2: Image Splitting
1. With an image uploaded
2. Configure split settings:
   - Direction: Vertical
   - Position: 0.5
   - Mirror: Second
3. Click "Split Image"
- [ ] Progress indicator appears
- [ ] Success notification shows
- [ ] Split results section appears
- [ ] Two preview images display
- [ ] File paths are shown

### Test 3: Shape Morphing
1. After successful split
2. Configure morphing:
   - Method: Distance Transform
   - Steps: 5
3. Click "Generate Morphed Shapes"
- [ ] Progress indicator appears
- [ ] Success notification shows
- [ ] Morph results section appears
- [ ] Summary information displays
- [ ] Preview slider appears

### Test 4: Morph Preview
1. With morphed results
2. Use the slider to change steps
- [ ] Image updates for each step
- [ ] Animation controls work
- [ ] No errors occur

---

## ✅ Error Handling Tests

### Test 1: Missing Packages
1. Temporarily uninstall `magick`: `remove.packages("magick")`
2. Try to split an image
- [ ] Clear error message appears
- [ ] Instructions for installation provided

### Test 2: Invalid Input
1. Try to split without uploading an image
- [ ] Appropriate warning appears

### Test 3: File Permissions
1. Try to save to a restricted directory
- [ ] Error is caught gracefully
- [ ] User is informed

---

## ✅ Output Verification

### Test File Structure
1. After splitting and morphing, check output directory
```
output_directory/
├── split_[timestamp]/
│   ├── *_left.png (or *_right.png)
│   └── *_right.png (or *_left.png)
└── morphed_[timestamp]/
    ├── morph_1.png
    ├── morph_2.png
    └── ...
```
- [ ] Split folder exists with correct structure
- [ ] Morphed folder exists with correct structure
- [ ] Images are valid and openable
- [ ] File sizes are reasonable

---

## ✅ Documentation Verification

### User Guides
- [ ] Can open `MORPH_SHAPES_GUIDE.md`
- [ ] Content is readable and formatted correctly
- [ ] Examples are clear

### Quick Reference
- [ ] Can open `MORPH_SHAPES_QUICKREF.md`
- [ ] Tables display properly
- [ ] Information is accurate

### Workflow Diagram
- [ ] Can open `WORKFLOW_DIAGRAM.md`
- [ ] ASCII diagrams render correctly
- [ ] Steps are logical

### Function Documentation
```r
?morph_shapes_ui
?morph_shapes_server
?morph_shapes
?split_image
```
- [ ] All help pages display
- [ ] Parameters are documented
- [ ] Examples are provided

---

## ✅ Integration Testing

### Test with Other Modules
1. Launch app
2. Navigate between tabs:
   - Image Processing → Morph Shapes
   - Morph Shapes → Shape Analysis
   - Morph Shapes → Data Import
- [ ] No errors when switching tabs
- [ ] Module states are maintained
- [ ] No conflicts between modules

---

## ✅ Performance Testing

### Test with Various Image Sizes
Test with images of different sizes:
- Small: 100x100 pixels
- Medium: 500x500 pixels
- Large: 2000x2000 pixels

For each size:
- [ ] Upload completes successfully
- [ ] Split completes in reasonable time
- [ ] Morph completes in reasonable time
- [ ] Preview displays correctly

### Test with Different Formats
- [ ] PNG images work
- [ ] JPEG images work
- [ ] TIFF images work
- [ ] Other formats show appropriate errors

---

## ✅ Cleanup and Finalization

### Remove Test Files
- [ ] Remove any test images created
- [ ] Clean up temporary directories
- [ ] Remove test output folders

### Final Package Build
```r
# Document the package
devtools::document()

# Run checks
devtools::check()
```
- [ ] Documentation builds successfully
- [ ] No errors in package check
- [ ] Warnings are acceptable or resolved

---

## 🎉 Final Verification

If all items above are checked, your installation is complete!

### Launch the App
```r
library(HaugShapeV2)
run_haug_app()
```

### Quick Test
1. Navigate to "Morph Shapes"
2. Upload a test image
3. Split it
4. Morph the halves
5. Preview the results

**Everything working?** ✅ You're ready to use the morph shapes feature!

---

## 🐛 Troubleshooting Guide

### Issue: Package won't load
**Solution:**
```r
# Reinstall dependencies
install.packages(c("shiny", "shinydashboard", "magick", "imager"))

# Reload package
devtools::load_all()
```

### Issue: Functions not found
**Solution:**
```r
# Re-document and reload
devtools::document()
devtools::load_all()
```

### Issue: App won't launch
**Solution:**
1. Check R console for error messages
2. Verify all dependencies are installed
3. Try running test script: `source("test_morph_module.R")`

### Issue: Images won't display
**Solution:**
1. Verify `magick` is installed: `library(magick)`
2. Check file format is supported
3. Try with a simple PNG image first

### Issue: Morphing fails
**Solution:**
1. Verify `imager` is installed: `library(imager)`
2. Check split images were created successfully
3. Try with simpler parameters (fewer steps, linear method)

---

## 📞 Support

Still having issues? Check:
- [ ] `IMPLEMENTATION_SUMMARY.md` for technical details
- [ ] `MORPH_SHAPES_GUIDE.md` for usage instructions
- [ ] R console for specific error messages
- [ ] Package versions are compatible

---

## 📝 Notes

Record any issues or observations during installation:

```
Date: _______________
Tester: _______________

Notes:
_____________________________________________________
_____________________________________________________
_____________________________________________________
_____________________________________________________
_____________________________________________________

```

---

**Checklist Version**: 1.0  
**Date**: December 8, 2025  
**Package**: HaugShape v2
