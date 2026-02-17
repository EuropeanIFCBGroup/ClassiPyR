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
- [`load_from_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_from_db.md)
  : Load classifications from SQLite database
- [`create_new_classifications()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/create_new_classifications.md)
  : Create new classifications for annotation mode
- [`filter_to_extracted()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/filter_to_extracted.md)
  : Filter classifications to only include extracted images

## Sample Saving

Functions for saving annotations and exporting images

- [`save_sample_annotations()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/save_sample_annotations.md)
  : Save sample annotations
- [`save_validation_statistics()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/save_validation_statistics.md)
  : Save validation statistics to CSV files
- [`copy_images_to_class_folders()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/copy_images_to_class_folders.md)
  : Copy images to class-organized folders

## Database Backend

SQLite database functions for annotation storage

- [`get_default_db_dir()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_default_db_dir.md)
  : Get default database directory
- [`get_db_path()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_db_path.md)
  : Get path to the annotations SQLite database
- [`save_annotations_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/save_annotations_db.md)
  : Save annotations to the SQLite database
- [`load_annotations_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_annotations_db.md)
  : Load annotations from the SQLite database
- [`list_annotated_samples_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/list_annotated_samples_db.md)
  : List samples with annotations in the database
- [`update_annotator()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/update_annotator.md)
  : Update the annotator name for one or more samples
- [`import_mat_to_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/import_mat_to_db.md)
  : Import a .mat annotation file into the SQLite database
- [`import_all_mat_to_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/import_all_mat_to_db.md)
  : Bulk import .mat annotation files into the SQLite database
- [`export_db_to_mat()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/export_db_to_mat.md)
  : Export annotations from SQLite to a .mat file
- [`export_all_db_to_mat()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/export_all_db_to_mat.md)
  : Bulk export all annotated samples from SQLite to .mat files
- [`export_db_to_png()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/export_db_to_png.md)
  : Export annotated images from SQLite to class-organized PNG folders
- [`export_all_db_to_png()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/export_all_db_to_png.md)
  : Bulk export all annotated samples from SQLite to class-organized
  PNGs

## File Index Cache

Functions for managing the file index cache for faster startup

- [`get_file_index_path()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_file_index_path.md)
  : Get path to file index cache
- [`load_file_index()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_file_index.md)
  : Load file index from disk cache
- [`save_file_index()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/save_file_index.md)
  : Save file index to disk cache
- [`rescan_file_index()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/rescan_file_index.md)
  : Rescan folders and rebuild the file index cache

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
