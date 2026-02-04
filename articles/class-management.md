# Class List Management

Understanding and managing your class list is important for maintaining
consistent annotations.

------------------------------------------------------------------------

## Why Class Indices Matter (ifcb-analysis Users)

> **Note**: This section is primarily relevant if you use the
> [ifcb-analysis](https://github.com/hsosik/ifcb-analysis) MATLAB
> toolbox (Sosik & Olson, 2007). If you only work with CSV exports,
> class indices are less critical.

IFCB .mat annotations use **numerical indices** to reference classes:

    Index 1 → unclassified
    Index 2 → Ciliate
    Index 3 → Dinoflagellate
    Index 4 → Diatom
    ...

When you save an annotation as a .mat file for ifcb-analysis, the file
stores:

- ROI number: 00042
- Class index: 2 (meaning “Ciliate”)

**If you change the order or remove classes, existing .mat annotations
become invalid for ifcb-analysis!**

------------------------------------------------------------------------

## Creating a Class List from Scratch

You can create a new class list directly in the app without uploading a
file:

[![Class list editor for adding, removing, and reordering
classes.](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/class-editor.png)](https://europeanifcbgroup.github.io/ClassiPyR/reference/figures/class-editor.png)

*Class list editor for adding, removing, and reordering classes. Click
to enlarge.*

1.  Open **Settings** → **Edit Class List**
2.  The editor opens with an empty class list
3.  Add classes using either method:

**Method 1 - One at a time:**

- Type a class name in the “Add new class” field
- Click **Add to End**
- Repeat for each class

**Method 2 - Bulk entry:**

- Type or paste class names in the text area (one per line)
- Click **Apply Changes**

4.  The app creates a temporary class list file automatically
5.  **Save your work**: Click **Save as .mat** or **Save as .txt** to
    keep your class list for future sessions

> **Note**: The temporary class list works for the current session, but
> you should save it to a file for future use.

------------------------------------------------------------------------

## Loading an Existing Class List

### From MAT File

Standard MATLAB class2use format:

1.  Open Settings
2.  Click Browse next to “Class List File”
3.  Select your `.mat` file

### From Text File

One class per line:

    unclassified
    Ciliate
    Dinoflagellate
    Diatom

------------------------------------------------------------------------

## Viewing Classes

1.  Open Settings → **Edit Class List**
2.  The left panel shows all classes with indices
3.  Toggle **By ID** / **A-Z** to sort the view

**Note**: Sorting is for viewing only - it doesn’t change actual
indices.

------------------------------------------------------------------------

## Editing Classes

### Renaming a Class

You can safely rename classes without breaking annotations:

1.  Find the class in the edit text area
2.  Change the name
3.  Click **Apply Changes**

The index stays the same, so existing annotations remain valid.

### Adding New Classes

New classes must go at the **end** of the list:

**Method 1 - Quick Add:**

1.  Type name in “Add new class” field
2.  Click **Add to End**

**Method 2 - Text Edit:**

1.  Add new classes at the bottom of the text area
2.  Click **Apply Changes**

### What NOT to Do (ifcb-analysis Users)

> **Note**: These restrictions apply if you export .mat files for use
> with [ifcb-analysis](https://github.com/hsosik/ifcb-analysis). CSV
> exports store class names directly and are not affected by index
> changes.

- **Never remove classes** - This shifts all subsequent indices
- **Never reorder classes** - This changes index-to-name mapping
- **Never insert in the middle** - This shifts indices below

### Example: What Goes Wrong (ifcb-analysis)

Original list:

    1: unclassified
    2: Ciliate
    3: Dinoflagellate
    4: Diatom

If you remove “Ciliate”:

    1: unclassified
    2: Dinoflagellate  ← Was index 3, now index 2!
    3: Diatom  ← Was index 4, now index 3!

Now all your “Ciliate” (index 2) .mat annotations become
“Dinoflagellate” when loaded in `ifcb-analysis`!

------------------------------------------------------------------------

## Exporting Class Lists

### Save as MAT

Creates MATLAB-compatible `class2use.mat` for use with
[ifcb-analysis](https://github.com/hsosik/ifcb-analysis):

1.  Click **Save as .mat**
2.  Choose location in browser download

> **Note**: Saving .mat files requires Python with scipy.

### Save as TXT

Creates simple text file:

1.  Click **Save as .txt**
2.  Choose location in browser download

------------------------------------------------------------------------

## Best Practices

1.  **Backup your class list** before making changes

2.  **Add classes at the end** only (required for ifcb-analysis
    compatibility)

3.  **Never delete** - mark deprecated classes with prefix like “OLD\_”
    (for ifcb-analysis users)

4.  **Document changes** in your project notes

5.  **Use consistent naming** - decide on convention (underscores,
    capitalization)

> **Tip**: If you don’t use
> [ifcb-analysis](https://github.com/hsosik/ifcb-analysis) and only work
> with CSV exports, you have more flexibility with class list management
> since CSV files store class names directly.

------------------------------------------------------------------------

## Class Naming Conventions

### Allowed Characters

Most characters commonly used in taxonomic names are allowed:

- Letters (a-z, A-Z)
- Numbers (0-9)
- Underscores (`_`)
- Hyphens (`-`) - e.g., “Strombidium-like”
- Periods (`.`)
- Spaces

### Characters That Are Modified

Since class names are used as folder names when exporting PNGs, some
characters are automatically sanitized:

| Character   | Why                                         | What Happens      |
|-------------|---------------------------------------------|-------------------|
| `/` or `\`  | Path separators would create subdirectories | Replaced with `_` |
| `< > " ' &` | HTML/security risks                         | Removed           |
| `: * ? \|`  | Invalid on Windows                          | Removed           |
| `..`        | Path traversal security risk                | Removed           |

**Example for ambiguous taxa**: If you have `Snowella/Woronichinia` in
your class list, it will be saved as `Snowella_Woronichinia`. The app
will display a message when this happens.

------------------------------------------------------------------------

## Merging Class Lists

If you need to combine class lists from different projects:

1.  Export both as text files
2.  Manually merge, keeping original indices
3.  Add new classes at the end
4.  Import merged list

------------------------------------------------------------------------

## Special Classes

### “unclassified”

- Usually index 1 or at the end
- Used for images below confidence threshold
- Used for images you can’t identify

### Detritus/Artifacts

Consider having classes for:

- `detritus`
- `bubble`
- `bad_image`
- `multiple_organisms`
