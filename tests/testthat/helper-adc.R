# Shared test helper: write a mock ADC file with n_roi rows in the standard
# IFCB folder structure under roi_folder. Columns 16/17 are width/height; ROIs
# listed in zero_dims get width/height 0 (no image).
write_mock_adc <- function(roi_folder, sample_name, n_roi, zero_dims = integer(0)) {
  year <- substr(sample_name, 2, 5)
  date_part <- substr(sample_name, 1, 9)
  adc_dir <- file.path(roi_folder, year, date_part)
  dir.create(adc_dir, recursive = TRUE, showWarnings = FALSE)
  adc_path <- file.path(adc_dir, paste0(sample_name, ".adc"))

  width <- rep(100L, n_roi)
  height <- rep(80L, n_roi)
  width[zero_dims] <- 0L
  height[zero_dims] <- 0L

  mock <- data.frame(
    V1 = seq_len(n_roi), V2 = 0, V3 = 0, V4 = 0, V5 = 0,
    V6 = 0, V7 = 0, V8 = 0, V9 = 0, V10 = 0,
    V11 = 0, V12 = 0, V13 = 0, V14 = 0, V15 = 0,
    V16 = width, V17 = height
  )
  write.table(mock, adc_path, row.names = FALSE, col.names = FALSE, sep = ",")
  adc_path
}
