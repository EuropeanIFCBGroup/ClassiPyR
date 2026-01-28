# Getting Started

This tutorial walks you through your first session with `ClassiPyR`.

## Before You Begin

Make sure you have:

1.  The package installed (see
    [Installation](https://europeanifcbgroup.github.io/ClassiPyR/))
2.  Your IFCB data files (ROI, ADC, HDR)
3.  Optionally: a class list file (.mat or .txt) - you can also create
    one from scratch in the app
4.  Optionally: existing classifications (CSV or classifier MAT files)

### Python Requirements

Python is required if you work with MATLAB .mat files:

- **Loading existing annotations** (.mat files from previous sessions)
- **Loading MATLAB classifier output** (.mat files)
- **Saving annotations** as .mat files for
  [ifcb-analysis](https://github.com/hsosik/ifcb-analysis)

If you only work with CSV classification files, Python is not required.

To set up Python:

``` r
library(iRfcb)
ifcb_py_install(envname = "./venv")  # Creates venv in current working directory
```

------------------------------------------------------------------------

## Step 1: Configure Settings

Launch the app:

``` r
library(ClassiPyR)
run_app()

# Or specify a custom Python virtual environment path
run_app(venv_path = "/path/to/your/venv")
```

Click the **gear icon** next to your username in the sidebar.

[![Settings dialog showing folder configuration
options.](../reference/figures/settings-dialog.png)](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/settings-dialog.png)

*Settings dialog showing folder configuration options. Click to
enlarge.*

Configure your folders:

| Setting               | Description                            | Example             |
|-----------------------|----------------------------------------|---------------------|
| Classification Folder | Where your CSV/MAT classifications are | `/ifcb/classified/` |
| ROI Data Folder       | Where your IFCB raw files are          | `/ifcb/raw/`        |
| Output Folder         | Where annotations will be saved        | `/ifcb/manual/`     |
| PNG Output Folder     | Where images will be organized         | `/ifcb/png/`        |

Click **Save Settings**.

> **Note**: You can also configure the Python virtual environment path
> in Settings if you didn’t specify it when launching the app.

------------------------------------------------------------------------

## Step 2: Set Up Your Class List

You have two options for setting up your class list:

### Option A: Load an Existing Class List

If you have an existing class list file:

1.  In Settings, click **Browse** next to “Class List File”
2.  Select your `.mat` or `.txt` file
3.  The app will confirm how many classes were loaded

### Option B: Create a Class List from Scratch

If you’re starting a new project without a class list:

1.  Click **Edit Class List** in Settings (no need to upload a file
    first)
2.  Add classes using one of these methods:
    - Type a class name in “Add new class” field and click **Add to
      End**
    - Type or paste multiple classes (one per line) in the text area and
      click **Apply Changes**
3.  The app will create a temporary class list automatically
4.  **Important**: Click **Save as .mat** or **Save as .txt** to save
    your class list for future sessions

> **Tip**: You can start annotating immediately after creating classes -
> the app handles the temporary file automatically.

------------------------------------------------------------------------

## Step 3: Select a Sample

Choose a **Year** from the dropdown.

Optionally filter by **Month**.

Select **Show**:

- *All samples*: See everything
- *Auto-classified (validation)*: Samples with existing
  auto-classifications
- *Manually annotated*: Samples you’ve previously annotated
- *Unannotated*: New samples (annotation from scratch)

Choose a sample from the dropdown:

- ✎ = Has manual annotation
- [x] = Has auto-classification
- ✎✓ = Has both (can switch between modes)
- \* = Unannotated (new sample)

[![Sample browser with year/month filters and status
indicators.](../reference/figures/sample-browser.png)](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/sample-browser.png)

*Sample browser with year/month filters and status indicators. Click to
enlarge.*

Click **Load**.

> **Tip**: Samples with ✎✓ let you switch between viewing your manual
> annotations and the auto-classifications using a button in the header.

------------------------------------------------------------------------

## Step 4: Review Images

Once loaded, you’ll see the Image Gallery:

[![Image gallery showing classified plankton images grouped by
class.](../reference/figures/gallery-view.png)](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/gallery-view.png)

*Image gallery showing classified plankton images grouped by class.
Click to enlarge.*

- Images are grouped by class
- Each image shows its ROI number
- Relabeled images have yellow borders
- Classification scores are shown (if available)

### Navigation

- Use **Filter by Class** to focus on one class
- Use **pagination** to navigate through pages
- Change images per page (50/100/200/500)

------------------------------------------------------------------------

## Step 5: Make Annotations

### Selecting Images

**Single click**: Select/deselect one image

**Drag select**:

1.  Click and hold in an empty area
2.  Drag to create a selection box
3.  All images in the box are selected

**Batch select**:

- **Select All**: Select all visible images
- **Deselect**: Clear selection

### Relabeling

1.  Select one or more images
2.  Type or search for a class in “Relabel to”
3.  Click **Relabel**

[![Relabeling workflow: selected images (blue borders) ready to be
assigned a new
class.](../reference/figures/relabel-workflow.png)](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/relabel-workflow.png)

*Relabeling workflow: selected images (blue borders) ready to be
assigned a new class. Click to enlarge.*

The images will move to their new class group.

------------------------------------------------------------------------

## Step 6: Save Your Work

Click **Save Annotations** to save:

- MAT file for MATLAB compatibility (requires Python; for use with
  [ifcb-analysis](https://github.com/hsosik/ifcb-analysis))
- Statistics CSV with accuracy metrics
- PNGs organized by class

### Auto-save

Work is automatically saved when:

- You navigate to another sample
- You close the app (attempts to save)

------------------------------------------------------------------------

## Tips for Efficient Annotation

1.  **Start with large classes** - Use “Filter by Class” to focus on
    abundant taxa

2.  **Use drag-select** - Much faster than clicking individual images

3.  **Sort by size** - Images are sorted by ROI area, grouping similar
    organisms

4.  **Check statistics** - The “Validation Statistics” tab shows your
    progress

------------------------------------------------------------------------

## Next Steps

- [User
  Guide](https://europeanifcbgroup.github.io/ClassiPyR/articles/user-guide.md) -
  Complete feature documentation
- [Class List
  Management](https://europeanifcbgroup.github.io/ClassiPyR/articles/class-management.md) -
  Managing class lists for ifcb-analysis
- [FAQ &
  Troubleshooting](https://europeanifcbgroup.github.io/ClassiPyR/articles/faq.md) -
  Common issues and solutions
