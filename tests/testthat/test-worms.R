library(testthat)

test_that("sanitize_worms_query handles separators, qualifiers, and long strings", {
  x <- c(
    "Prorocentrum_micans",
    "Alexandrium/cf._tamarense",
    "Spelaeopogon spp.",
    "Asppartanium sp",
    "This_is_a_very_long_name_that_should_be_skipped_because_it_exceeds_the_limit"
  )

  out <- sanitize_worms_query(x, max_chars = 40)

  expect_equal(out[1], "Prorocentrum micans")
  expect_equal(out[2], "Alexandrium tamarense")
  expect_equal(out[3], "Spelaeopogon")
  expect_equal(out[4], "Asppartanium")
  expect_equal(out[5], "")
})

test_that("get_worms_col returns column values or NA fallback", {
  df <- data.frame(AphiaID = c("1", "2"), stringsAsFactors = FALSE)
  expect_equal(ClassiPyR:::get_worms_col(df, "AphiaID"), c("1", "2"))
  expect_equal(ClassiPyR:::get_worms_col(df, "missing"), c(NA_character_, NA_character_))
})

test_that("pick_worms_match handles empty, accepted, and synonym records", {
  empty_res <- ClassiPyR:::pick_worms_match(NULL)
  expect_equal(empty_res$status, "unmatched")
  expect_true(is.na(empty_res$aphia_id))

  accepted_df <- data.frame(
    AphiaID = 123,
    valid_AphiaID = 123,
    scientificname = "Prorocentrum micans",
    valid_name = "Prorocentrum micans",
    status = "accepted",
    stringsAsFactors = FALSE
  )
  accepted_res <- ClassiPyR:::pick_worms_match(accepted_df)
  expect_equal(accepted_res$status, "accepted")
  expect_equal(accepted_res$aphia_id, "123")

  synonym_df <- data.frame(
    AphiaID = 50,
    valid_AphiaID = 123,
    scientificname = "Old name",
    valid_name = "Accepted name",
    status = "unaccepted",
    stringsAsFactors = FALSE
  )
  synonym_res <- ClassiPyR:::pick_worms_match(synonym_df)
  expect_equal(synonym_res$status, "synonym")
  expect_equal(synonym_res$aphia_id, "123")
  expect_equal(synonym_res$accepted_name, "Accepted name")
})

test_that("fetch_worms_records returns chunk results when API succeeds", {
  mock_api <- function(query, marine_only = FALSE) {
    if (length(query) > 1) {
      return(lapply(query, function(q) data.frame(
        AphiaID = 1,
        valid_AphiaID = 1,
        scientificname = q,
        valid_name = q,
        status = "accepted",
        stringsAsFactors = FALSE
      )))
    }
    data.frame(
      AphiaID = 1,
      valid_AphiaID = 1,
      scientificname = query,
      valid_name = query,
      status = "accepted",
      stringsAsFactors = FALSE
    )
  }

  local_mocked_bindings(
    worms_records_names_api = mock_api,
    .package = "ClassiPyR"
  )

  res <- ClassiPyR:::fetch_worms_records(c("A", "B"), chunk_size = 2)
  expect_length(res, 2)
  expect_true(is.data.frame(res[["A"]]))
  expect_equal(res[["A"]]$scientificname[1], "A")
})

test_that("fetch_worms_records falls back to single-query and short-query retry", {
  calls <- character()
  mock_api <- function(query, marine_only = FALSE) {
    qtxt <- paste(query, collapse = "|")
    calls <<- c(calls, qtxt)

    # Fail bulk query
    if (length(query) > 1) stop("bulk failed")

    # Fail long query once, then succeed on short fallback
    if (identical(query, "LongTaxonName that will fail")) stop("single failed")

    data.frame(
      AphiaID = 42,
      valid_AphiaID = 42,
      scientificname = query,
      valid_name = query,
      status = "accepted",
      stringsAsFactors = FALSE
    )
  }

  local_mocked_bindings(
    worms_records_names_api = mock_api,
    .package = "ClassiPyR"
  )

  res <- ClassiPyR:::fetch_worms_records(
    c("LongTaxonName that will fail", "GoodName"),
    chunk_size = 2
  )

  expect_true("LongTaxonName that will fail|GoodName" %in% calls)
  expect_true("LongTaxonName that will fail" %in% calls)
  expect_true(any(grepl("^LongTaxonName that$", calls)))
  expect_true(is.data.frame(res[["LongTaxonName that will fail"]]))
  expect_equal(res[["LongTaxonName that will fail"]]$AphiaID[1], 42)
})

test_that("build_worms_match_rows returns expected statuses", {
  mock_api <- function(query, marine_only = FALSE) {
    if (identical(query, "NoMatch")) {
      return(data.frame())
    }
    data.frame(
      AphiaID = 7,
      valid_AphiaID = 7,
      scientificname = query,
      valid_name = query,
      status = "accepted",
      stringsAsFactors = FALSE
    )
  }

  local_mocked_bindings(
    worms_records_names_api = mock_api,
    .package = "ClassiPyR"
  )

  very_long <- paste(rep("VeryLongTaxonLabel", 6), collapse = "_")
  class_names <- c("Prorocentrum_micans", "NoMatch", very_long)
  raw_queries <- c("Prorocentrum_micans", "NoMatch", very_long)

  rows <- build_worms_match_rows(class_names, raw_queries)

  expect_equal(nrow(rows), 3)
  expect_equal(rows$status[1], "accepted")
  expect_equal(rows$aphia_id[1], "7")
  expect_equal(rows$status[2], "unmatched")
  expect_equal(rows$status[3], "skipped")
})
