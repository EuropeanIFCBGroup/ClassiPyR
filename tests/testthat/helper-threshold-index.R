# Shared set-up for the index cache tests (test-threshold_index*.R)

# A fresh class-score folder and index location, removed by the caller.
thr_test_workspace <- function() {
  root <- tempfile("thr_index_")
  h5_folder <- file.path(root, "h5")
  dir.create(h5_folder, recursive = TRUE)
  list(root = root, h5_folder = h5_folder,
       index_path = file.path(root, "review", "threshold_index.sqlite"))
}

thr_test_samples <- sprintf("D202401%02dT000000_IFCB134", 1:9)

thr_test_files_table <- function(index_path) {
  thr_test_query(index_path, "SELECT * FROM files ORDER BY h5_path")
}
