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

**Q: How can I review all images of a specific class?**

A: Use **Class Review mode**. Switch to “Class Review” using the mode
toggle in the sidebar, select **Database** as the source, select a class
from the dropdown, and click Load. This loads all images annotated as
that class from every sample in the database. You can then reclassify
any mistakes using the normal relabeling tools.

**Q: Can I reclassify images across multiple samples at once?**

A: Yes. Class Review mode (Database source) loads images from all
samples and saves changes as row-level updates to the database. This
means only the images you reclassify are updated — other annotations in
those samples remain untouched.

**Q: Can I sort a folder of PNG images into class subfolders?**

A: Yes. In Class Review mode, select **External PNG Folder** as the
source, browse to your PNG folder, and click **Load Folder**. All images
are loaded with an initial class label (defaulting to the folder name).
Relabel images in the gallery, then set an export folder and click
**Export Split Folders** to copy images into class-name subfolders. This
works independently of the database.

**Q: Can I classify images without pre-computed classifier files?**

A: Yes. Configure a Gradio API URL and model in Settings \> Live
Prediction, then click the **Predict** button in the sidebar after
loading a sample. This sends images to a remote CNN model and applies
the predictions directly. See the [User
Guide](https://europeanifcbgroup.github.io/ClassiPyR/articles/user-guide.html#live-prediction)
for details.

------------------------------------------------------------------------

## Installation Issues

**Q: I see “Python not available” warning**

A: This warning only appears when your storage format includes `.mat`
files. Python is **not needed** for the default SQLite storage.

If you see this warning and don’t need `.mat` files, switch to SQLite in
Settings \> Annotation Storage. Otherwise, to enable `.mat` support:

``` r
library(iRfcb)
ifcb_py_install()  # Creates venv in current working directory
```

Then restart the app.

**Q: Do I need Python to use ClassiPyR?**

A: No. The default storage format is SQLite, which works out of the box
with no Python dependency. Python is only needed if you want to export
`.mat` files for
[ifcb-analysis](https://github.com/hsosik/ifcb-analysis) compatibility.

**Q: Where is the Python virtual environment created?**

A: By default,
[`ifcb_py_install()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_py_install.html)
creates a virtual environment at `~/.virtualenvs/iRfcb`. You can specify
a different location:

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
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("EuropeanIFCBGroup/ClassiPyR")
```

**Q: “Could not find app directory” error**

A: Try reinstalling the package:

``` r
remotes::install_github("EuropeanIFCBGroup/ClassiPyR")
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

1.  ROI/PNG Data Folder points to your data
2.  Either ROI files exist, or extracted PNG sample folders exist (for
    example
    `.../D20230313T004021_IFCB134/D20230313T004021_IFCB134_00002.png`)
3.  Click the **Sync** button (circular arrow icon) to rescan folders if
    you recently added new data

**Q: “ROI file not found” error**

A: The app scans the ROI/PNG Data Folder recursively, so any subfolder
layout works (including flat). Check that:

1.  The ROI/PNG Data Folder path is correct
2.  If using ROI files, each `.roi` file has a matching `.adc` file in
    the same directory
3.  If using extracted PNGs, sample folders are named
    `DYYYYMMDDTHHMMSS_IFCBNNN` and image files follow `sample_#####.png`
4.  Click the **Sync** button to rescan if you recently moved or added
    files

**Q: Classifications not loading**

A: For CSV files:

- Must have columns named `file_name` and `class_name` (exact names
  required)
- Optionally include `score` and `class_name_auto` columns
- The CSV file must be named after the sample (e.g.,
  `D20230101T120000_IFCB134.csv`)
- File should be in the Classification Folder (indexed via file cache;
  click Sync to refresh)

For H5 files:

- Must match pattern `*_class*.h5`
- Requires the `hdf5r` package (`install.packages("hdf5r")`)

For MAT files:

- Must match pattern `*_class*.mat`
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

With optional `score` and `class_name_auto` columns:

    file_name,class_name,class_name_auto,score
    D20230101T120000_IFCB134_00001.png,unclassified,Diatom,0.45
    D20230101T120000_IFCB134_00002.png,Ciliate,Ciliate,0.87
    D20230101T120000_IFCB134_00003.png,Dinoflagellate,Dinoflagellate,0.72

The `class_name_auto` column contains the raw prediction without
threshold. When “Apply classification threshold” is disabled in
Settings, ClassiPyR uses `class_name_auto` instead of `class_name`.

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
2.  If using MAT format: Python is available (not needed for default
    SQLite storage)
3.  Click “Save Annotations” before closing

------------------------------------------------------------------------

## Class List

**Q: Do I need to upload a class list file to start annotating?**

A: No! You can create a class list from scratch directly in the app:

1.  Open Settings → Edit Class List (no file upload needed)
2.  Add classes one at a time or paste multiple classes in the text area
3.  Click Apply Changes - a temporary file is created automatically
4.  Start annotating immediately

With SQLite storage (default), your class list is auto-saved to the
database and restored on next startup. You can still export as `.mat` or
`.txt` for sharing or backup.

**Q: How do I create a class list from scratch?**

A:

1.  Open Settings → Edit Class List
2.  Add classes using “Add new class” field, or type/paste classes in
    the text area
3.  Click Apply Changes
4.  With SQLite storage, the class list persists automatically.
    Optionally save as `.mat` or `.txt` for portability.

**Q: Is my class list saved automatically?**

A: Yes, when using SQLite storage (the default). Every change — adding
classes, renaming, applying WoRMS matches, uploading a file — is
auto-saved to the `global_class_list` table in the SQLite database. On
next startup, the class list is restored from the database. If you don’t
use SQLite storage, save your class list as a `.txt` or `.mat` file to
preserve it.

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

**Q: How do I match class names to WoRMS AphiaID values?**

A: In **Settings → Edit Class List**, click **Match WoRMS AphiaID**.

- The app sanitizes class names before querying WoRMS
- Long names (\>80 characters) are skipped automatically in auto-match
- Unmatched/skipped classes can be rematched manually by editing query
  fields and clicking **Rematch Unmatched**
- Click **Apply AphiaID Matches** to persist results

Matched AphiaIDs are shown in the class list as `[AphiaID: ...]`.

**Q: Where are AphiaID mappings stored?**

A: In the SQLite database (`annotations.sqlite`), table
`class_taxonomy`, in the configured **Database Folder**. They are not
embedded in `class2use.mat`/`.txt` files.

------------------------------------------------------------------------

## Output

**Q: Where are my annotations saved?**

A: Annotations are split across two locations:

- **SQLite database** (default): stored in the **Database Folder** (a
  local directory)
- **MAT files and statistics**: stored in the **Output Folder** (can be
  on a network drive)

&nbsp;

    db_folder/                           ← local drive (Database Folder)
    └── annotations.sqlite              ← single database for ALL samples

    output_folder/                       ← can be a network drive (Output Folder)
    ├── D20230101T120000_IFCB134.mat    ← only if storage format includes "MAT"
    ├── D20230202T080000_IFCB134.mat
    └── validation_statistics/
        ├── ..._validation_stats.csv
        └── ..._validation_detailed.csv

By default, the database is stored in a persistent local directory
(`tools::R_user_dir("ClassiPyR", "data")`). Back up `annotations.sqlite`
to preserve your work.

**Q: Where is the default database location?**

A: The default Database Folder is a platform-specific local directory:

- **Linux**: `~/.local/share/R/ClassiPyR/`
- **macOS**:
  `~/Library/Application Support/org.R-project.R/R/ClassiPyR/`
- **Windows**: `%LOCALAPPDATA%/R/data/R/ClassiPyR/`

You can find the exact path with:

``` r
ClassiPyR::get_default_db_dir()
```

You can change it in Settings \> Database Folder, but it should always
be a local drive.

**Q: Can I put the database on a network drive?**

A: No. SQLite databases are [not safe on network
filesystems](https://www.sqlite.org/useovernet.html) (NFS, SMB/CIFS)
because network file locking is unreliable, which can lead to database
corruption. Always keep the Database Folder on a local drive. The Output
Folder (for MAT files and statistics) can safely be on a network drive.

**Q: How do I transfer my annotations to another computer?**

A: Since the SQLite database is stored locally, you cannot simply share
it over a network drive. Instead, use `.mat` files as the interchange
format:

1.  **Export** from the source computer (requires Python with scipy):

``` r
library(ClassiPyR)
db_path <- get_db_path(get_default_db_dir())
# Export all annotations to .mat files in a shared output folder
result <- export_all_db_to_mat(db_path, "/shared/network/manual")
cat(result$success, "exported\n")
```

Or use the **Export SQLite → .mat** button in Settings.

2.  **Import** on the target computer:

``` r
library(ClassiPyR)
db_path <- get_db_path(get_default_db_dir())
# Import .mat files from the shared folder into the local database
result <- import_all_mat_to_db("/shared/network/manual", db_path)
cat(result$success, "imported,", result$skipped, "skipped\n")
```

Or use the **Import .mat → SQLite** button in Settings. Already-imported
samples are skipped automatically.

You can also simply copy the `annotations.sqlite` file directly between
machines if you prefer.

**Q: Can I import annotations back to MATLAB?**

A: Yes, if you save with the “MAT file” or “Both” storage format, the
MAT files are compatible with the
[ifcb-analysis](https://github.com/hsosik/ifcb-analysis) toolbox (Sosik
& Olson, 2007). Use the list in `startMC`, or load the list in MATLAB
using:

``` matlab
load('sample_name.mat');
% classlist contains [roi_number, class_index]
```

Note: Python with `scipy` must be installed to save .mat files. Change
the storage format in Settings \> Annotation Storage.

**Q: Can I migrate existing .mat annotations to the SQLite database?**

A: Yes. The easiest way is the **Import .mat → SQLite** button in
Settings \> Annotation Storage, which bulk-imports all `.mat` files in
your output folder.

You can also import programmatically — a single file:

``` r
library(ClassiPyR)
import_mat_to_db(
  mat_path = "/data/manual/D20230101T120000_IFCB134.mat",
  db_path = get_db_path(get_default_db_dir()),
  sample_name = "D20230101T120000_IFCB134"
)
```

Or bulk-import all `.mat` files in a folder:

``` r
result <- import_all_mat_to_db("/data/manual", get_db_path(get_default_db_dir()))
cat(result$success, "imported,", result$failed, "failed,", result$skipped, "skipped\n")
```

**Q: Can I export SQLite annotations back to .mat files?**

A: Yes. Use the **Export SQLite → .mat** button in Settings \>
Annotation Storage to export all annotated samples at once. This
requires Python with scipy.

You can also export programmatically:

``` r
# Single sample
export_db_to_mat(get_db_path(get_default_db_dir()), "D20230101T120000_IFCB134", "/data/manual")

# All samples
result <- export_all_db_to_mat(get_db_path(get_default_db_dir()), "/data/manual")
cat(result$success, "exported,", result$failed, "failed\n")
```

**Q: Can I create a MATLAB-format ZIP archive for sharing?**

A: Yes. Use the **Export SQLite → MATLAB ZIP** button in Settings \>
Export from SQLite. This bundles `.mat` annotation files, feature CSVs,
a `class2use.mat` config file, optional raw data, and README files into
a distributable ZIP archive via
[`iRfcb::ifcb_zip_matlab()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_zip_matlab.html).
You need to provide a features folder; a data folder with raw IFCB files
is optional.

If your storage format is SQLite-only, annotations are automatically
converted to temporary `.mat` files (requires Python with scipy). See
the [iRfcb image export
tutorial](https://europeanifcbgroup.github.io/iRfcb/articles/image-export-tutorial.html)
for more details on the archive format.

**Q: Can I import images classified in another tool?**

A: Yes. Organize your PNG images into subfolders named after each class
(e.g., `Diatom/`, `Ciliate_002/`). Then use **Import PNG → SQLite** in
Settings \> Import / Export. The app strips trailing `_NNN` suffixes
from folder names (following the iRfcb convention) and maps images to
class names based on which subfolder they are in.

If your folder class names don’t match the app’s current class list, a
mapping dialog will appear letting you remap them to existing classes or
add them as new classes.

Note: ROI files are needed for viewing images and re-exporting. Without
ROI files, annotations are stored in the database but images cannot be
displayed in the gallery.

You can also import programmatically:

``` r
library(ClassiPyR)
result <- import_png_folder_to_db(
  "/data/png_export",
  get_db_path(get_default_db_dir()),
  class2use = c("Diatom", "Ciliate", "Dinoflagellate"),
  annotator = "Jane"
)
cat(result$success, "imported,", result$failed, "failed\n")
```

**Q: Can I change the annotator name for existing annotations?**

A: Yes. Use
[`update_annotator()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/update_annotator.md)
from the R console:

``` r
library(ClassiPyR)
db_path <- get_db_path(get_default_db_dir())

# Update a single sample
update_annotator(db_path, "D20230101T120000_IFCB134", "Jane")

# Update several samples at once
update_annotator(db_path, c("sample_A", "sample_B"), "Jane")

# Update all annotated samples (e.g. after a bulk import)
all_samples <- list_annotated_samples_db(db_path)
update_annotator(db_path, all_samples, "Jane")
```

The function returns a named vector showing how many annotation rows
were updated per sample (0 means the sample was not found in the
database).

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
available. When you load it, the app defaults to manual annotation mode.
A toggle in the header lets you switch between annotation and validation
mode.

**Q: Can I switch to annotation mode for a sample that only has
auto-classifications?**

A: Yes. Any sample with auto-classification data (✓ or ✎✓) shows a mode
toggle in the header. Clicking **→ Manual** creates blank “unclassified”
annotations if no manual annotations exist yet, so you can annotate from
scratch while keeping the option to switch back to the
auto-classifications.

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

| Error                      | Solution                                                                                                                                                                |
|----------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| “ROI file not found”       | Check ROI/PNG Data Folder path. If no ROI exists, provide extracted PNG sample folders named by sample and click Sync                                                   |
| “ADC file not found”       | ADC file must be alongside ROI file                                                                                                                                     |
| “Python not available”     | Only affects `.mat` export. Switch to SQLite in Settings, or run [`iRfcb::ifcb_py_install()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_py_install.html) |
| “Error loading class list” | Check file format (.mat or .txt)                                                                                                                                        |
| “No samples found”         | Check ROI/PNG Data Folder configuration and naming                                                                                                                      |
| App fails to start         | Try `run_app(reset_settings = TRUE)` to clear saved settings                                                                                                            |

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

7.  **Dashboard: specify a dataset** - Large Dashboard instances (e.g.,
    `habon-ifcb.whoi.edu` with 900,000+ samples) will be very slow to
    load without a dataset filter. Always include `?dataset=` in the
    URL, e.g. `https://habon-ifcb.whoi.edu/timeline?dataset=tangosund`

------------------------------------------------------------------------

## Getting Help

- [GitHub
  Issues](https://github.com/EuropeanIFCBGroup/ClassiPyR/issues) -
  Report bugs
- [iRfcb Documentation](https://github.com/EuropeanIFCBGroup/iRfcb) -
  Core dependency for IFCB data handling
