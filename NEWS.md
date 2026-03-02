# ClassiPyR 0.2.0

## New features

- **Live Prediction**: Added a "Predict" button in Sample Mode that classifies all images in the loaded sample using a remote CNN model via `iRfcb::ifcb_classify_images()`. Configure the Gradio API URL and model in Settings > Live Prediction. The model dropdown is populated dynamically from the Gradio server. Predictions respect the classification threshold setting, skip manually reclassified images, and new class names from the model are added to the class list automatically. A per-image progress bar shows classification progress.
- **IFCB Dashboard support**: Connect directly to remote IFCB Dashboard instances (e.g. `https://habon-ifcb.whoi.edu/`) without downloading data locally. Toggle between "Local Folders" and "IFCB Dashboard" in Settings, enter a Dashboard URL (with optional `?dataset=` parameter), and browse samples from the API. Images are downloaded on demand and cached locally. Optionally load dashboard auto-classifications for validation mode. Supports MAT export by downloading ADC files on demand, with graceful fallback to SQLite-only when ADC is unavailable.
- **Dashboard class review optimization**: Class review mode with a dashboard source now downloads individual PNG images instead of entire zip archives, making it much faster when reviewing a single class across many samples.
- **Configurable dashboard download settings**: Dashboard mode now exposes parallel downloads, sleep time, timeout, and max retries in an "Advanced Download Settings" section in Settings. Previously these were hardcoded.
- **Local classification files in dashboard mode**: The Classification Folder setting is now available in dashboard mode. When configured, local CSV/H5/MAT classification files take priority over dashboard auto-classifications, with dashboard autoclass as a fallback.
- New exported functions: `parse_dashboard_url()`, `list_dashboard_bins()`, `download_dashboard_images()`, `download_dashboard_images_bulk()`, `download_dashboard_image_single()`, `download_dashboard_images_individual()`, `download_dashboard_adc()`, `download_dashboard_autoclass()`, and `get_dashboard_cache_dir()` for programmatic dashboard access.
- **Class Review Mode**: View and reclassify all annotated images of a specific class across the entire database. Switch to class review via the mode toggle in the sidebar, select a class, and load all matching images from all samples at once. Changes are saved as row-level updates to the database.
- New exported functions: `list_classes_db()`, `load_class_annotations_db()`, and `save_class_review_changes_db()` for programmatic class review operations.
- Added **Import PNG → SQLite** button in Settings > Import / Export. Imports annotations from a folder of PNG images organized in class-name subfolders (e.g. exported by ClassiPyR or other tools). Folder names follow the iRfcb convention where trailing `_NNN` suffixes are stripped.
- When importing PNG folders with class names not in the current class list, a **class mapping dialog** lets users remap unmatched classes to existing ones or add them as new classes.
- Overwrite warning dialog shown when imported samples already exist in the database.
- New exported functions: `scan_png_class_folder()` for scanning PNG class folder structures, and `import_png_folder_to_db()` for programmatic bulk import.
- **HDF5 classification support**: Load classifications from `.h5` files produced by [iRfcb](https://github.com/EuropeanIFCBGroup/iRfcb) (>= 0.8.0). Requires the optional `hdf5r` package.
- **Classification threshold toggle**: New "Apply classification threshold" checkbox in Settings controls whether thresholded or raw predictions are used, for all classification formats (CSV, H5, MAT).
- **Skip class from PNG export**: New option in Settings to exclude a specific class (e.g. "unclassified") from PNG output.

## UI improvements

- The **class list editor** now shows the number of annotated images per class in parentheses, queried from the SQLite database.

# ClassiPyR 0.1.1

## New features

- Added **"Export validation statistics"** checkbox in Settings (below the output folder path). When unchecked, per-sample CSV files are not written to the `validation_statistics/` subfolder. Useful when annotating from scratch where validation statistics are not relevant (#9).
- Added a **confirmation dialog** before bulk export of SQLite annotations to `.mat` files. The dialog explains that existing `.mat` files in the output folder will be overwritten, preventing accidental data loss (#10).

# ClassiPyR 0.1.0

Initial release of ClassiPyR, a Shiny application for manual classification and validation of Imaging FlowCytobot (IFCB) plankton images.

## Features

### Sample Management
- Load samples from ROI files with automatic year/month filtering
- Support for validation mode (existing classifications) and annotation mode (new samples)
- Resume previous annotations from saved files
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

### Annotation Storage
- SQLite database backend (default) — no Python dependency required
- Optional MATLAB `.mat` file export for ifcb-analysis compatibility (requires Python/scipy)
- Configurable storage format in Settings: "SQLite", "MAT file", or "Both"
- `import_mat_to_db()` and `export_db_to_mat()` for migration between formats
- Sample discovery scans both `.mat` files and the SQLite database
- When loading a sample, SQLite is checked first (faster), with `.mat` fallback
- Separate database folder setting (defaults to output folder)

### Output
- Save validation statistics as CSV
- Organize output PNGs by class folder (for CNN training)
- Auto-save when navigating between samples
- Support for non-standard folder structures via direct ADC path resolution

### File Index Cache
- Disk-based file index cache for faster app startup on subsequent launches
- Avoids expensive recursive directory scans when folder contents haven't changed
- Sync button in sidebar to manually refresh the file index
- Cache age indicator shows when folders were last scanned
- `rescan_file_index()` function for headless use (e.g. cron jobs)
- Auto-sync option (enabled by default) to control whether app scans on startup

### Settings & Persistence
- Configurable folder paths via settings modal
- Cross-platform web-based folder browser (shinyFiles)
- Settings persisted between sessions
- Class list file path remembered and auto-loaded on startup
- Annotator name tracking for statistics

### User Interface
- Clean, modern interface using bslib (Flatly theme)
- Mode indicator showing current sample and progress/accuracy
- Validation Statistics tab shows appropriate content based on mode
- Switch between annotation/validation modes for dual-mode samples

## Pre-releases

- **v0.1.0-beta.2** (2026-02-04): File index cache, cross-platform folder browser, annotation mode sorting, and notification improvements.
- **v0.1.0-beta.1** (2026-01-29): First beta version.

## Technical Notes
- SQLite is the default annotation storage — works out of the box with RSQLite
- Python with scipy is optional — only needed for MAT file export
- Uses iRfcb package for IFCB data handling
- Session cache preserves work when switching samples
- Input validation, XSS prevention, and path traversal protection
