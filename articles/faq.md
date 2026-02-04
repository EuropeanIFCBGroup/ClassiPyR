# FAQ & Troubleshooting

## Frequently Asked Questions

------------------------------------------------------------------------

### General

**Q: What IFCB data formats are supported?**

A: The app reads standard IFCB files:

- ROI files (.roi) - Image data
- ADC files (.adc) - Metadata (dimensions, timestamps)
- HDR files (.hdr) - Header information

**Q: Can I use this for non-IFCB data?**

A: The app is specifically designed for IFCB data format. For other
imaging systems, you would need to adapt the data loading functions.

**Q: Is my data modified?**

A: No. The app only reads your original files. All output is written to
separate folders.

------------------------------------------------------------------------

## Installation Issues

**Q: I see “Python not available” warning**

A: This warning affects saving .mat files. Python is required for:

- Saving annotations as .mat files for
  [ifcb-analysis](https://github.com/hsosik/ifcb-analysis)

Reading .mat files (annotations, classifier output, class lists) does
not require Python. If you do not need to save .mat files, you can
ignore this warning.

To enable .mat support:

``` r
library(iRfcb)
ifcb_py_install()  # Creates venv in current working directory
```

Then restart the app.

**Q: Where is the Python virtual environment created?**

A: By default,
[`ifcb_py_install()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_py_install.html)
creates a `venv` folder in your home directory. You can specify a
different location:

``` r
ifcb_py_install("/path/to/your/venv")
```

You can also specify the venv path when launching the app:

``` r
run_app(venv_path = "/path/to/your/venv")
```

**Q: How is the Python virtual environment path resolved?**

A: The app uses the following priority order:

1.  **`venv_path` argument** passed to
    [`run_app()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/run_app.md)
    (highest priority)
2.  **Saved settings** from a previous session (stored in
    `settings.json`)
3.  **Default** `./venv` in the working directory

When you specify `run_app(venv_path = "/path/to/venv")`, that path is
used for Python initialization and pre-filled in the Settings dialog,
overriding any previously saved path.

**Q: Package installation fails**

A: Make sure you have remotes installed and try:

``` r
install.packages("remotes")
remotes::install_github("EuropeanIFCBGroup/ClassiPyR")
```

**Q: “Could not find app directory” error**

A: Try reinstalling the package:

``` r
install.packages("iRfcb")
```

**Q: iRfcb won’t install**

A: [iRfcb](https://github.com/EuropeanIFCBGroup/iRfcb) is the core
dependency for `ClassiPyR` and is installed automatically. If you
encounter issues:

``` r
install.packages("iRfcb")
```

------------------------------------------------------------------------

## Data Loading

**Q: No samples appear in the dropdown**

A: Check that:

1.  ROI Data Folder points to your data
2.  ROI files exist and are readable
3.  Click the **Sync** button (circular arrow icon) to rescan folders if
    you recently added new data

**Q: “ROI file not found” error**

A: The app scans the ROI Data Folder recursively, so any subfolder
layout works (including flat). Check that:

1.  The ROI Data Folder path is correct
2.  Each `.roi` file has a matching `.adc` file in the same directory
3.  Filenames follow the IFCB naming convention
    (`DYYYYMMDDTHHMMSS_IFCBNNN`)
4.  Click the **Sync** button to rescan if you recently moved or added
    files

**Q: Classifications not loading**

A: For CSV files:

- Must have columns named `file_name` and `class_name` (exact names
  required)
- Optionally include a `score` column (confidence value between 0 and 1)
- The CSV file must be named after the sample (e.g.,
  `D20230101T120000_IFCB134.csv`)
- File should be in the Classification Folder (indexed via file cache;
  click Sync to refresh)

For MAT files:

- Must match pattern `*_class*.mat`
- Must contain `roinum` and `TBclass` variables
- Must contain `roinum` and `TBclass` variables

------------------------------------------------------------------------

## CSV Format

**Q: What should my classification CSV look like?**

A: The CSV must have columns named `file_name` and `class_name`. The
file must be named after the sample (e.g.,
`D20230101T120000_IFCB134.csv`).

Minimal example:

    file_name,class_name
    D20230101T120000_IFCB134_00001.png,Diatom
    D20230101T120000_IFCB134_00002.png,Ciliate

With optional `score` column (confidence values between 0 and 1):

    file_name,class_name,score
    D20230101T120000_IFCB134_00001.png,Diatom,0.95
    D20230101T120000_IFCB134_00002.png,Ciliate,0.87
    D20230101T120000_IFCB134_00003.png,Dinoflagellate,0.72

**Q: My CNN classifier outputs different column names**

A: The column names must be exactly `file_name` and `class_name`. If
your classifier uses different names, rename the columns before loading.
For example in R:

``` r
df <- read.csv("my_classifications.csv")
names(df)[names(df) == "predicted_class"] <- "class_name"
names(df)[names(df) == "filename"] <- "file_name"
write.csv(df, "D20230101T120000_IFCB134.csv", row.names = FALSE)
```

------------------------------------------------------------------------

## Annotation

**Q: Drag-select isn’t working**

A: Make sure you’re:

1.  Starting the drag in the gallery area (not on an image)
2.  Using left mouse button
3.  Dragging far enough (\>5 pixels)

**Q: Images show as “Not found”**

A: The ROI might be empty (no actual image data). These are filtered out
automatically on load.

**Q: Changes aren’t being saved**

A: Check that:

1.  Output folder is writable
2.  Python is available (required for saving .mat files)
3.  Click “Save Annotations” before closing

------------------------------------------------------------------------

## Class List

**Q: Do I need to upload a class list file to start annotating?**

A: No! You can create a class list from scratch directly in the app:

1.  Open Settings → Edit Class List (no file upload needed)
2.  Add classes one at a time or paste multiple classes in the text area
3.  Click Apply Changes - a temporary file is created automatically
4.  Start annotating immediately
5.  Remember to Save as .mat or .txt for future sessions

**Q: How do I create a class list from scratch?**

A:

1.  Open Settings → Edit Class List
2.  Add classes using “Add new class” field, or type/paste classes in
    the text area
3.  Click Apply Changes
4.  Save as .mat or .txt for future use

**Q: Can I import a class list from MATLAB?**

A: Yes, load your existing `class2use.mat` file via Settings.

**Q: My class names look different after loading**

A: The app may truncate trailing numbers from class names (e.g.,
“Diatom_01” → “Diatom”). This matches iRfcb behavior.

**Q: Why are some characters in my class names changed?**

A: Class names are used as folder names when exporting PNGs, so certain
characters must be sanitized:

| Character   | Reason              | Action            |
|-------------|---------------------|-------------------|
| `/` or `\`  | Path separators     | Replaced with `_` |
| `< > " ' &` | HTML/security risks | Removed           |
| `: * ? \|`  | Windows filesystem  | Removed           |
| `..`        | Path traversal risk | Removed           |

**Example**: `Snowella/Woronichinia` becomes `Snowella_Woronichinia`

Common taxonomic characters like hyphens (`-`), underscores (`_`),
periods (`.`), and spaces are preserved.

------------------------------------------------------------------------

## Output

**Q: Where are my annotations saved?**

A: In the Output Folder you configured:

- MAT annotation files are saved directly in the output folder (one per
  sample)
- `validation_statistics/` subfolder contains CSV statistics
- PNGs are in the PNG Output Folder, organized by class name

**Q: Can I import annotations back to MATLAB?**

A: Yes, the MAT files are compatible with the
[ifcb-analysis](https://github.com/hsosik/ifcb-analysis) toolbox (Sosik
& Olson, 2007). Use the list in `startMC`, or load the list in MATLAB
using:

``` matlab
load('sample_name.mat');
% classlist contains [roi_number, class_index]
```

Note: Python with `scipy` must be installed to save .mat files.

**Q: What’s in the statistics CSV?**

A: Two files per sample:

1.  `*_validation_stats.csv` - Summary statistics
2.  `*_validation_detailed.csv` - Per-image details

------------------------------------------------------------------------

## Measurement

**Q: How do I measure images?**

A: Click the Measure button (ruler icon) in the toolbar to activate
measure mode. Then click and drag on any image to draw a measurement
line. The distance is displayed in micrometers and pixels.

**Q: How do I change the scale calibration?**

A: Open Settings and find “Pixels per Micrometer”. The default is 3.4
(standard for IFCB). Enter your instrument’s calibration value.

------------------------------------------------------------------------

## Settings & Persistence

**Q: Are my settings saved between sessions?**

A: Yes! Settings are stored in a configuration file:

- **Linux**: `~/.config/R/ClassiPyR/settings.json`
- **macOS**:
  `~/Library/Preferences/org.R-project.R/R/ClassiPyR/settings.json`
- **Windows**: `%APPDATA%/R/config/R/ClassiPyR/settings.json`

Folder paths, class list location, and Python venv path are
automatically restored when you restart the app.

**Q: How do I reset all settings to defaults?**

A: Use the `reset_settings` argument when launching the app:

``` r
run_app(reset_settings = TRUE)
```

This deletes the saved `settings.json` file and starts the app with
default values. All folder paths, the class list reference, and the
Python venv path are cleared, so you will need to reconfigure them. The
class list file itself (`class2use_saved.*`) is not deleted from the
config directory but will not be loaded until you re-upload it. This is
useful if:

- The app fails to start due to invalid saved paths
- Folder paths point to locations that no longer exist
- You want a clean slate after changing your data layout

You can also combine it with other arguments:

``` r
# Reset settings and specify a new Python environment
run_app(reset_settings = TRUE, venv_path = "/path/to/your/venv")
```

**Q: What’s the yellow warning on some classes?**

A: Classes marked with a warning are in your classification data but not
in your class list. This can happen when:

- Loading classifications from a classifier trained with a different
  class list
- The class list has been modified since the classification was created

You can still work with these classes, but consider adding them to your
class list for consistency.

**Q: A sample shows both annotation and classification markers - what
does that mean?**

A: This sample has both manual annotations AND auto-classifications
available. When you load it:

- The app defaults to manual annotation mode
- You can switch to validation mode using the button in the header
- Each mode maintains its state independently

------------------------------------------------------------------------

## File Index Cache

**Q: What is the file index cache?**

A: The file index cache stores the locations of all IFCB files (ROI,
classification, annotation) found in your configured folders. It’s saved
to disk so the app doesn’t need to re-scan your entire folder hierarchy
every time it starts. This significantly speeds up startup for large
datasets.

**Q: How do I refresh the file cache?**

A: Click the **Sync** button (circular arrow icon) in the sidebar, next
to the sample navigation buttons. The cache age indicator below shows
when the last scan occurred.

**Q: New samples I added aren’t showing up**

A: The app loads from the cached file index. Click the **Sync** button
to rescan your folders and pick up new files.

**Q: Can I update the cache without opening the app?**

A: Yes. Use
[`rescan_file_index()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/rescan_file_index.md)
from the R console or a scheduled script:

``` r
ClassiPyR::rescan_file_index()
```

This reads folder paths from your saved settings and rebuilds the cache.
You can also pass paths explicitly:

``` r
ClassiPyR::rescan_file_index(
  roi_folder = "/data/ifcb/raw",
  csv_folder = "/data/ifcb/classified",
  output_folder = "/data/ifcb/manual"
)
```

**Q: Where is the cache file stored?**

A: In the same config directory as your settings:

- **Linux**: `~/.config/R/ClassiPyR/file_index.json`
- **macOS**:
  `~/Library/Preferences/org.R-project.R/R/ClassiPyR/file_index.json`
- **Windows**: `%APPDATA%/R/config/R/ClassiPyR/file_index.json`

------------------------------------------------------------------------

## Error Messages

| Error                      | Solution                                                                                                                              |
|----------------------------|---------------------------------------------------------------------------------------------------------------------------------------|
| “ROI file not found”       | Check ROI Data Folder path; ensure `.roi` files use IFCB naming and click Sync                                                        |
| “ADC file not found”       | ADC file must be alongside ROI file                                                                                                   |
| “Python not available”     | Affects saving .mat files. Run [`iRfcb::ifcb_py_install()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_py_install.html) |
| “Error loading class list” | Check file format (.mat or .txt)                                                                                                      |
| “No samples found”         | Check ROI Data Folder configuration                                                                                                   |
| App fails to start         | Try `run_app(reset_settings = TRUE)` to clear saved settings                                                                          |

------------------------------------------------------------------------

## Performance Tips

1.  **File index cache** - The app caches folder scan results for fast
    startup. Click Sync only when you’ve added new data.

2.  **Use pagination** - Lower images per page for faster loading

3.  **Filter by class** - Reduces rendering load

4.  **Close other apps** - Image extraction uses memory

5.  **SSD storage** - Faster file access

6.  **Scheduled rescans** - On servers with regularly arriving data, use
    [`ClassiPyR::rescan_file_index()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/rescan_file_index.md)
    in a cron job to keep the cache current without manual intervention

------------------------------------------------------------------------

## Getting Help

- [GitHub
  Issues](https://github.com/EuropeanIFCBGroup/ClassiPyR/issues) -
  Report bugs
- [iRfcb Documentation](https://github.com/EuropeanIFCBGroup/iRfcb) -
  Core dependency for IFCB data handling
