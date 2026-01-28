# Package index

## Running the App

Functions for launching ClassiPyR

- [`run_app()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/run_app.md)
  : Run the ClassiPyR Shiny Application
- [`init_python_env()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/init_python_env.md)
  : Initialize Python environment for iRfcb
- [`get_config_dir()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_config_dir.md)
  : Get ClassiPyR configuration directory
- [`get_settings_path()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_settings_path.md)
  : Get path to settings file

## Sample Loading

Functions for loading classifications and samples

- [`load_class_list()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_class_list.md)
  : Load class list from MAT or TXT file
- [`load_from_classifier_mat()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_from_classifier_mat.md)
  : Load classifications from MATLAB classifier output file
- [`load_from_csv()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_from_csv.md)
  : Load classifications from CSV file (validation mode)
- [`load_from_mat()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_from_mat.md)
  : Load classifications from existing MAT annotation file
- [`create_new_classifications()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/create_new_classifications.md)
  : Create new classifications for annotation mode
- [`filter_to_extracted()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/filter_to_extracted.md)
  : Filter classifications to only include extracted images

## Sample Saving

Functions for saving annotations and exporting images

- [`save_sample_annotations()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/save_sample_annotations.md)
  : Save sample annotations to MAT and statistics files
- [`save_validation_statistics()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/save_validation_statistics.md)
  : Save validation statistics to CSV files
- [`copy_images_to_class_folders()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/copy_images_to_class_folders.md)
  : Copy images to class-organized folders

## Utilities

Helper functions for IFCB data processing

- [`get_sample_paths()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_sample_paths.md)
  : Get sample paths from sample name
- [`read_roi_dimensions()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/read_roi_dimensions.md)
  : Read ROI dimensions from ADC file
- [`is_valid_sample_name()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/is_valid_sample_name.md)
  : Validate IFCB sample name format
- [`sanitize_string()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/sanitize_string.md)
  : Sanitize string for safe use in HTML/file paths
- [`create_empty_changes_log()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/create_empty_changes_log.md)
  : Create empty changes log data frame
