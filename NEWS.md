# ClassiPyR (development version)

## Features

### SQLite Database Backend
- Annotations are now stored in a local SQLite database (`annotations.sqlite`) by default
- Works out of the box with no Python dependency - only R packages (RSQLite, DBI) are needed
- MATLAB `.mat` file export is still available as an opt-in for ifcb-analysis compatibility
- Storage format configurable in Settings: "SQLite" (default), "MAT file", or "Both"
- Existing `.mat` annotations continue to work and can be loaded as before
- `import_mat_to_db()` utility for bulk migration of existing `.mat` files to SQLite
- Sample discovery scans both `.mat` files and the SQLite database
- When loading a sample, SQLite is checked first (faster), with `.mat` fallback

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

### File Index Cache
- Disk-based file index cache for faster app startup on subsequent launches
- Avoids expensive recursive directory scans when folder contents haven't changed
- Sync button in sidebar to manually refresh the file index
- Cache age indicator shows when folders were last scanned
- `rescan_file_index()` function for headless use (e.g. cron jobs)
- Cache stored in platform-appropriate config directory alongside settings
- Auto-sync option (enabled by default) to control whether app scans on startup

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
- Save annotations to SQLite database (default, no Python needed)
- Optional: save annotations as MATLAB-compatible .mat files (using iRfcb, requires Python)
- Configurable storage format: SQLite only, MAT only, or both
- Save validation statistics as CSV (in `validation_statistics/` subfolder)
- Organize output PNGs by class folder (for CNN training)
- Auto-save when navigating between samples
- Support for non-standard folder structures via direct ADC path resolution
- Graceful handling of empty (0-byte) ADC files

### Settings & Persistence
- Configurable folder paths via settings modal
- Cross-platform web-based folder browser (shinyFiles)
- Settings persisted between sessions (`.classipyr_settings.json`)
- Class list file path remembered and auto-loaded on startup
- Annotator name tracking for statistics
- Cache invalidation when folder paths change in settings

### User Interface
- Clean, modern interface using bslib (Flatly theme)
- Mode indicator showing current sample and progress/accuracy
- Validation Statistics tab shows appropriate content based on mode
- Switch between annotation/validation modes for dual-mode samples

## Technical Notes
- SQLite is the default annotation storage - works out of the box with RSQLite (no external dependencies)
- Python with scipy is optional - only needed for MAT file export (ifcb-analysis compatibility)
- Uses iRfcb package for IFCB data handling
- Session cache preserves work when switching samples
- File index cache reduces startup time by avoiding redundant folder scans
- Security: Input validation, XSS prevention, path traversal protection

## Development
This application was developed through human-AI collaboration:
- **Anders Torstensson**: Project vision, requirements, testing, and guidance
- **Claude Code (Anthropic)**: Implementation, code generation, and iterative refinement
