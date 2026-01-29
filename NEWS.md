# ClassiPyR (development version)

## Features

### Sample Management
- Load samples from ROI files with automatic year/month filtering
- Support for validation mode (existing classifications) and annotation mode (new samples)
- Resume previous annotations from saved MAT files
- Navigate between samples with previous/next/random buttons
- Filter samples by classification status (all/classified/annotated/unannotated)
- Samples with both manual annotations AND auto-classifications can switch between modes

### Classification Loading
- Load classifications from CSV files (recursive folder search)
- Load classifications from MATLAB classifier output (.mat files)
- Option to apply classification threshold for MATLAB results
- Automatic sample status indicators in dropdown:
  - ✎ = Has manual annotation
  - ✓ = Has auto-classification
  - ✎✓ = Has both (can switch between modes)
  - * = Unannotated

### Image Gallery
- Paginated image display (50/100/200/500 images per page)
- Images grouped by class on consecutive pages for efficient review
- Filter images by class
- Click to select/deselect individual images
- Drag-select to select multiple images at once
- Visual indicators for selected and relabeled images
- Unmatched class detection with yellow warning highlighting

### Annotation Tools
- Relabel selected images to any class
- Select all / deselect all buttons
- Quick class search in relabel dropdown
- Changes tracked and displayed in statistics tab

### Class List Management
- Load class lists from .mat or .txt files
- Create class lists from scratch directly in the app
- Edit class names (with warnings about index preservation for ifcb-analysis)
- Add new classes to end of list
- Sort class list by ID or alphabetically (view only)
- Export class list as .mat or .txt
- Visual warnings for classes in classifications not in class2use list

### Output
- Save annotations as MATLAB-compatible .mat files (using iRfcb)
- Save validation statistics as CSV (in `validation_statistics/` subfolder)
- Organize output PNGs by class folder (for CNN training)
- Auto-save when navigating between samples

### Settings & Persistence
- Configurable folder paths via settings modal
- Settings persisted between sessions (`.classipyr_settings.json`)
- Class list file path remembered and auto-loaded on startup
- Annotator name tracking for statistics

### User Interface
- Clean, modern interface using bslib (Flatly theme)
- Mode indicator showing current sample and progress/accuracy
- Validation Statistics tab shows appropriate content based on mode
- Switch between annotation/validation modes for dual-mode samples

## Technical Notes
- Requires Python with scipy for MAT file writing (optional - only for ifcb-analysis compatibility)
- Uses iRfcb package for IFCB data handling
- Session cache preserves work when switching samples
- Security: Input validation, XSS prevention, path traversal protection

## Development
This application was developed through human-AI collaboration:
- **Anders Torstensson**: Project vision, requirements, testing, and guidance
- **Claude Code (Anthropic)**: Implementation, code generation, and iterative refinement
