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

Functions for loading classifications and samples from ROI/PNG sources

- [`load_class_list()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_class_list.md)
  : Load class list from MAT or TXT file
- [`load_from_classifier_mat()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_from_classifier_mat.md)
  : Load classifications from MATLAB classifier output file
- [`load_from_csv()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_from_csv.md)
  : Load classifications from CSV file (validation mode)
- [`load_from_h5()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_from_h5.md)
  : Load classifications from HDF5 classifier output file
- [`load_from_mat()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_from_mat.md)
  : Load classifications from existing MAT annotation file
- [`load_from_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_from_db.md)
  : Load classifications from SQLite database
- [`create_new_classifications()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/create_new_classifications.md)
  : Create new classifications for annotation mode
- [`filter_to_extracted()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/filter_to_extracted.md)
  : Filter classifications to only include extracted images
- [`scan_png_class_folder()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/scan_png_class_folder.md)
  : Scan a PNG folder with class subfolders

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
  : Save annotations to SQLite
- [`delete_annotations_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/delete_annotations_db.md)
  : Delete annotations for a sample from the SQLite database
- [`load_annotations_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_annotations_db.md)
  : Load annotations from the SQLite database
- [`list_annotated_samples_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/list_annotated_samples_db.md)
  : List samples with annotations in the database
- [`list_annotation_metadata_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/list_annotation_metadata_db.md)
  : List distinct years, months, and instruments from annotations
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
- [`export_all_db_to_zip()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/export_all_db_to_zip.md)
  : Bulk export all annotated samples from SQLite to EcoTaxa-ready ZIP
- [`import_png_folder_to_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/import_png_folder_to_db.md)
  : Import annotations from a PNG class folder into the SQLite database
- [`list_classes_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/list_classes_db.md)
  : List all classes with counts in the annotations database
- [`save_class_taxonomy_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/save_class_taxonomy_db.md)
  : Save class taxonomy mappings to SQLite
- [`load_class_taxonomy_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_class_taxonomy_db.md)
  : Load class taxonomy mappings from SQLite
- [`save_global_class_list_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/save_global_class_list_db.md)
  : Save annotations to the SQLite database
- [`load_global_class_list_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_global_class_list_db.md)
  : Load global class list from SQLite
- [`load_class_annotations_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_class_annotations_db.md)
  : Load all annotations for a specific class from the database
- [`save_class_review_changes_db()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/save_class_review_changes_db.md)
  : Save class review changes to the database

## WoRMS Taxonomy

Functions for matching class names to WoRMS AphiaID values

- [`sanitize_worms_query()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/sanitize_worms_query.md)
  : Sanitize taxon names for WoRMS matching
- [`build_worms_match_rows()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/build_worms_match_rows.md)
  : Build WoRMS match rows for class names

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

## Dashboard

Functions for working with remote IFCB Dashboard instances

- [`parse_dashboard_url()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/parse_dashboard_url.md)
  : Parse an IFCB Dashboard URL
- [`list_dashboard_bins()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/list_dashboard_bins.md)
  : List bins from an IFCB Dashboard
- [`download_dashboard_images()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/download_dashboard_images.md)
  : Download and extract PNG images from the Dashboard
- [`download_dashboard_images_bulk()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/download_dashboard_images_bulk.md)
  : Bulk download zip archives for multiple samples from the Dashboard
- [`download_dashboard_image_single()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/download_dashboard_image_single.md)
  : Download a single PNG image from the Dashboard
- [`download_dashboard_images_individual()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/download_dashboard_images_individual.md)
  : Download individual PNG images from the Dashboard
- [`download_dashboard_adc()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/download_dashboard_adc.md)
  : Download ADC file from the Dashboard
- [`download_dashboard_autoclass()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/download_dashboard_autoclass.md)
  : Download and parse autoclass scores from the Dashboard
- [`resolve_sample_dataset()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/resolve_sample_dataset.md)
  : Resolve the dataset name for a sample from the Dashboard API
- [`get_dashboard_cache_dir()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_dashboard_cache_dir.md)
  : Get persistent cache directory for dashboard downloads

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
