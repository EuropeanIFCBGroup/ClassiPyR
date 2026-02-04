# User Guide

Complete documentation for all `ClassiPyR` features.

------------------------------------------------------------------------

## Interface Overview

[![ClassiPyR interface showing the title bar, sidebar, and main image
gallery
area.](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/interface-overview.png)](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/interface-overview.png)

*ClassiPyR interface showing the title bar, sidebar, and main image
gallery area. Click to enlarge.*

### Title Bar

- **App name and version**
- **Mode indicator**: Shows current state and mode
  - No sample loaded: Initial state before selecting a sample
  - Validation mode: Shows accuracy percentage
  - Annotation mode: Shows progress (X/Y classified)

### Sidebar

- **Annotator name**: Your name for statistics tracking
- **Settings**: Configure folders and options
- **Sample selection**: Year, month, status filters
- **Navigation**: Load, previous, next, random, sync
- **Cache age**: Shows when folders were last scanned
- **Save button**: Manual save trigger

### Main Area (Tabs)

1.  **Image Gallery**: View and annotate images
2.  **Summary Table**: Class distribution statistics
3.  **Validation Statistics**: Accuracy metrics and change log

------------------------------------------------------------------------

## Validation vs Annotation Mode

### Validation Mode

- Activated when loading samples with existing auto-classifications
- Original classifications shown with confidence scores
- Statistics track how many you’ve changed
- Accuracy percentage calculated

### Annotation Mode

- Activated for samples without classifications
- All images start as “unclassified”
- Progress shows classified vs remaining
- Validation statistics tab shows annotation progress instead

### Samples with Both Modes

Some samples may have both manual annotations AND auto-classifications
(e.g., you previously annotated a sample, then ran a classifier on it).
For these samples:

- The sample dropdown shows ✎✓ indicator
- When loaded, you can switch between modes using the button in the
  header
- Each mode maintains its own state independently

------------------------------------------------------------------------

## Working with Images

### Image Cards

Each image card displays:

- The plankton image
- ROI number
- Classification score (if available)
- Original class (if relabeled)

**Border colors:**

- Default (gray): Unchanged
- Yellow: Relabeled in this session
- Blue: Currently selected

[![Image card border colors: gray (unchanged), yellow (relabeled), blue
(selected).](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/image-card-states.png)](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/image-card-states.png)

*Image card border colors: gray (unchanged), yellow (relabeled), blue
(selected). Click to enlarge.*

### Selecting Images

| Method     | Action                              |
|------------|-------------------------------------|
| Click      | Toggle single image selection       |
| Drag       | Draw rectangle to select multiple   |
| Select All | Select all images in current filter |
| Deselect   | Clear all selections                |

[![Drag-select: draw a rectangle to select multiple images at
once.](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/drag-select.png)](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/drag-select.png)

*Drag-select: draw a rectangle to select multiple images at once. Click
to enlarge.*

### Relabeling

1.  Select target images
2.  Choose new class in “Relabel to” dropdown
3.  Click **Relabel**

The dropdown supports type-ahead search - just start typing the class
name.

------------------------------------------------------------------------

## Measuring Images

The measure tool allows you to measure distances in images.

### Using the Measure Tool

1.  Click the **Measure** button (ruler icon) in the toolbar to activate
    measure mode
2.  Click and drag on any image to draw a measurement line
3.  The distance is displayed in both micrometers (µm) and pixels
4.  Click elsewhere on the image to clear the measurement
5.  Click the Measure button again to deactivate measure mode

[![Measure tool showing distance in micrometers and
pixels.](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/measure-tool.png)](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/measure-tool.png)

*Measure tool showing distance in micrometers and pixels. Click to
enlarge.*

### Configuring Scale

The default scale is 3.4 pixels per micrometer (standard for IFCB). To
adjust:

1.  Open **Settings** (gear icon)
2.  Find **Pixels per Micrometer** field
3.  Enter your instrument’s calibration value
4.  Click **Save Settings**

------------------------------------------------------------------------

## Classification Sources

`ClassiPyR` supports multiple classification input formats.

### CSV Files

Standard classification CSV output. The CSV file must be named after the
sample it describes (e.g., `D20230101T120000_IFCB134.csv`).

Required columns (exact names):

- `file_name`: Image filename including `.png` extension (e.g.,
  `D20230101T120000_IFCB134_00001.png`)
- `class_name`: Predicted class name

Optional columns:

- `score`: Classification confidence (0-1)

**Minimal example:**

    file_name,class_name
    D20230101T120000_IFCB134_00001.png,Diatom
    D20230101T120000_IFCB134_00002.png,Ciliate

**Example with confidence scores:**

    file_name,class_name,score
    D20230101T120000_IFCB134_00001.png,Diatom,0.95
    D20230101T120000_IFCB134_00002.png,Ciliate,0.87
    D20230101T120000_IFCB134_00003.png,Dinoflagellate,0.72

**Different CNN pipelines**: If your classifier produces different
column names, rename them to `file_name` and `class_name` before placing
the CSV in the Classification Folder.

Files are looked up from the file index cache (see [File Index
Cache](#file-index-cache) below).

### MATLAB Classifier Output

Files matching `*_class*.mat` pattern containing:

- `roinum`: ROI numbers
- `TBclass_above_threshold`: With threshold
- `TBclass`: Without threshold

**Threshold option**: Enable in Settings to include unclassified
predictions below confidence threshold.

### Existing Annotations

Previously saved annotations (in output folder) are automatically
detected and can be resumed.

------------------------------------------------------------------------

## File Index Cache

To avoid slow startup from scanning large folder hierarchies,
`ClassiPyR` maintains a file index cache on disk. The cache stores the
locations of all ROI, classification, and annotation files found in your
configured folders.

### How it Works

- On first launch (or after changing folder paths in Settings), the app
  scans all configured folders and saves the results to a JSON cache
  file
- On subsequent launches, the app loads the cached index instantly
  instead of re-scanning
- The cache is stored alongside your settings in the platform config
  directory (see [Settings Persistence](#settings-persistence))

### Sync Button

The **Sync** button (circular arrow icon) in the sidebar navigation row
triggers a manual rescan of all folders. Use this when:

- You’ve added new IFCB data files to your folders
- The sample dropdown seems out of date
- You want to force a fresh scan

The **cache age indicator** below the navigation buttons shows when the
folders were last scanned (e.g. “synced just now”, “synced 2 hours
ago”).

### Auto-Sync

By default, the app checks whether the cache matches your current folder
settings on startup and rescans automatically if needed. You can disable
auto-sync in Settings to always load from the existing cache, which
provides the fastest possible startup.

### Headless Rescan

You can update the file index cache without launching the app using
[`rescan_file_index()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/rescan_file_index.md).
This is useful for scheduled updates (e.g. cron jobs) on servers where
new data arrives regularly:

``` r
# Rescan using saved settings
ClassiPyR::rescan_file_index()

# Or specify folder paths explicitly
ClassiPyR::rescan_file_index(
  roi_folder = "/data/ifcb/raw",
  csv_folder = "/data/ifcb/classified",
  output_folder = "/data/ifcb/manual"
)
```

------------------------------------------------------------------------

## Output Files

When you save, the app creates:

### Annotation MAT File

`output/[sample_name].mat`

MATLAB-compatible format with:

- `classlist`: ROI numbers and class indices
- Compatible with
  [ifcb-analysis](https://github.com/hsosik/ifcb-analysis) toolbox

> **Note**: Saving MAT files requires Python with scipy.

### Statistics Files

`output_folder/validation_statistics/[sample_name]_validation_stats.csv`

- Summary: total, correct, incorrect, accuracy

`output_folder/validation_statistics/[sample_name]_validation_detailed.csv`

- Per-image: original class, validated class, correct flag

### Organized PNGs

`png_output_folder/[class_name]/[image_files]`

Images organized into class folders for training CNN models or other
classifiers.

------------------------------------------------------------------------

## Settings Reference

### Folder Paths

| Setting               | Description                       |
|-----------------------|-----------------------------------|
| Classification Folder | Source of CSV/MAT classifications |
| ROI Data Folder       | IFCB raw files (ROI/ADC/HDR)      |
| Output Folder         | Where MAT and CSV output goes     |
| PNG Output Folder     | Where organized images go         |

Folder paths are configured using a web-based folder browser that works
on all platforms (Linux, macOS, Windows). Changing folder paths in
Settings automatically invalidates the file index cache, triggering a
fresh scan.

### Auto-Sync

| Setting                      | Description                                                                                                                          |
|------------------------------|--------------------------------------------------------------------------------------------------------------------------------------|
| Auto-sync folders on startup | When enabled (default), the app checks and refreshes the file index on launch. Disable for instant startup using the existing cache. |

### Python Configuration

The Python virtual environment path is configured when launching the
app:

``` r
run_app(venv_path = "/path/to/your/venv")
```

The path is remembered for future sessions. **Priority order**:
`run_app(venv_path=)` argument \> saved settings \> default (`./venv`).

### Classifier Options

**Apply classification threshold**: When loading MATLAB classifier
output, use `TBclass_above_threshold` (checked) or `TBclass`
(unchecked).

------------------------------------------------------------------------

## Statistics and Reporting

### Summary Table Tab

Shows class distribution:

- Class name
- Image count
- Average/min/max confidence scores

### Validation Statistics Tab

**Classification Performance**:

- Total images
- Correct/incorrect counts
- Overall accuracy
- Per-class breakdown

**Changes Made**:

- Table of all relabeling actions
- Original class → New class

------------------------------------------------------------------------

## Session Cache

The app maintains two types of caches:

**In-memory session cache** (per session):

- Switching samples saves work automatically
- Returning to a sample restores your changes
- Cache persists until you close the app

**Note**: Always click Save before closing for permanent storage.

**File index cache** (persistent on disk):

- Stores the locations of all IFCB files across your configured folders
- Persists between sessions for fast startup
- See [File Index Cache](#file-index-cache) for details

------------------------------------------------------------------------

## Settings Persistence

`ClassiPyR` stores your settings in a configuration file that follows R
standards:

- **Linux**: `~/.config/R/ClassiPyR/settings.json`
- **macOS**:
  `~/Library/Preferences/org.R-project.R/R/ClassiPyR/settings.json`
- **Windows**: `%APPDATA%/R/config/R/ClassiPyR/settings.json`

Settings are loaded automatically when you start the app, so your folder
paths, class list location, and Python venv path are remembered between
sessions. Settings can be reset by specifying
`run_app(reset_settings = TRUE)`.

------------------------------------------------------------------------

## Dependencies

`ClassiPyR` relies on
**[`iRfcb`](https://github.com/EuropeanIFCBGroup/iRfcb)** for all IFCB
data operations:

- Extracting images from ROI files
- Reading ADC metadata (dimensions, timestamps)
- Reading and writing MATLAB .mat files
- Class list handling

`iRfcb` is installed automatically as a dependency when you install
`ClassiPyR`.
