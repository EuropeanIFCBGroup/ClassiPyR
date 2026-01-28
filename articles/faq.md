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

A: This warning affects reading and writing .mat files. Python is
required for:

- Loading existing manual annotations (.mat files)
- Loading MATLAB classifier output (.mat files)
- Saving annotations as .mat files

If you only work with CSV files, you can ignore this warning.

To enable .mat support:

``` r
library(iRfcb)
ifcb_py_install()  # Creates venv in current working directory
```

Then restart the app.

**Q: Where is the Python virtual environment created?**

A: By default,
[`ifcb_py_install()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_py_install.html)
creates a `venv` folder in your current working directory. You can
specify a different location:

``` r
ifcb_py_install("/path/to/your/venv")
```

You can also configure the venv path in Settings or when launching the
app:

``` r
run_app(venv_path = "/path/to/your/venv")
```

**Q: Package installation fails**

A: Make sure you have remotes installed and try:

``` r
install.packages("remotes")
remotes::install_github("EuropeanIFCBGroup/ClassiPyR")
```

**Q: “Could not find app directory” error**

A: Try reinstalling the package:

``` r
remotes::install_github("EuropeanIFCBGroup/ClassiPyR", force = TRUE)
```

**Q: iRfcb won’t install**

A: [iRfcb](https://github.com/EuropeanIFCBGroup/iRfcb) is the core
dependency for `ClassiPyR` and is installed automatically. If you
encounter issues:

``` r
remotes::install_github("EuropeanIFCBGroup/iRfcb")
```

------------------------------------------------------------------------

## Data Loading

**Q: No samples appear in the dropdown**

A: Check that:

1.  ROI Data Folder points to your data
2.  Data is organized as: `folder/YYYY/DYYYYMMDD/files`
3.  ROI files exist and are readable

**Q: “ROI file not found” error**

A: The app expects this structure:

    roi_folder/
      2023/
        D20230101/
          D20230101T120000_IFCB134.roi
          D20230101T120000_IFCB134.adc

**Q: Classifications not loading**

A: For CSV files:

- Must have columns containing “file” and “class” in their names
- Recommended column names: `file_name` and `class_name`
- File should be in the Classification Folder (searched recursively)

For MAT files:

- Must match pattern `*_class*.mat`
- Must contain `roinum` and `TBclass` variables
- Requires Python to be available

------------------------------------------------------------------------

## CSV Format

**Q: What should my classification CSV look like?**

A: At minimum, your CSV needs:

    file_name,class_name
    D20230101T120000_IFCB134_00001.png,Diatom
    D20230101T120000_IFCB134_00002.png,Ciliate

Optional columns include `score` for confidence values (0-1).

**Q: My CNN classifier outputs different column names**

A: The app uses flexible column matching and looks for columns
containing “file” and “class”. These variants work:

- `filename`, `image_file`, `file_path` → matched as file column
- `class`, `predicted_class`, `classification` → matched as class column

If your format is different, rename the columns to `file_name` and
`class_name`.

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
2.  Python is available (required for .mat files)
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

A: Yes, load your existing `class2use.mat` file via Settings. Note: this
requires Python.

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
& Olson, 2007). Use:

``` matlab
load('sample_name.mat');
% classlist contains [roi_number, class_index]
```

Note: Python with scipy must be installed to save .mat files.

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

- **Linux**: `~/.local/share/ClassiPyR/settings.json`
- **macOS**: `~/Library/Application Support/ClassiPyR/settings.json`
- **Windows**: `%LOCALAPPDATA%/ClassiPyR/settings.json`

Folder paths, class list location, and Python venv path are
automatically restored when you restart the app.

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

## Error Messages

| Error                      | Solution                                                                                                                       |
|----------------------------|--------------------------------------------------------------------------------------------------------------------------------|
| “ROI file not found”       | Check ROI Data Folder path and file structure                                                                                  |
| “ADC file not found”       | ADC file must be alongside ROI file                                                                                            |
| “Python not available”     | Affects .mat files. Run [`iRfcb::ifcb_py_install()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_py_install.html) |
| “Error loading class list” | Check file format (.mat or .txt)                                                                                               |
| “No samples found”         | Check ROI Data Folder configuration                                                                                            |

------------------------------------------------------------------------

## Performance Tips

1.  **Use pagination** - Lower images per page for faster loading

2.  **Filter by class** - Reduces rendering load

3.  **Close other apps** - Image extraction uses memory

4.  **SSD storage** - Faster file access

------------------------------------------------------------------------

## Getting Help

- [GitHub
  Issues](https://github.com/EuropeanIFCBGroup/ClassiPyR/issues) -
  Report bugs
- [iRfcb Documentation](https://github.com/EuropeanIFCBGroup/iRfcb) -
  Core dependency for IFCB data handling
