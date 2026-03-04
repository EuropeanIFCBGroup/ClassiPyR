# WoRMS helper functions for class-name matching

#' Sanitize taxon names for WoRMS matching
#'
#' Cleans IFCB-style class labels before querying WoRMS.
#' Names longer than \code{max_chars} are skipped (returned as empty strings).
#'
#' @param x Character vector of class/taxon labels.
#' @param max_chars Maximum allowed input length before skipping a query.
#'   Default \code{80}.
#' @return Character vector of sanitized query strings.
#' @export
#' @examples
#' sanitize_worms_query(c(
#'   "Prorocentrum_micans",
#'   "Alexandrium_cf._tamarense",
#'   "Very_very_long_label_that_should_be_skipped"
#' ), max_chars = 30)
sanitize_worms_query <- function(x, max_chars = 80L) {
  x <- as.character(x)
  x <- trimws(x)
  too_long <- nchar(x) > max_chars
  if (all(too_long)) {
    return(rep("", length(x)))
  }
  x[too_long] <- ""
  keep <- !too_long

  # IFCB class conventions often include trailing numeric suffixes.
  x[keep] <- sub("_\\d{3}$", "", x[keep])
  # Common IFCB separators that hurt direct WoRMS lookup.
  x[keep] <- gsub("[_/]+", " ", x[keep])
  # Keep only conservative characters for taxon querying.
  x[keep] <- gsub("[^[:alnum:] .,'()-]", " ", x[keep])
  x[keep] <- gsub("\\s+", " ", x[keep])
  x[keep] <- trimws(x[keep])

  # IFCB classes can contain concatenated candidate taxa; prefer first binomial.
  x[keep] <- vapply(strsplit(x[keep], "\\s+"), function(tokens) {
    tokens <- tokens[nzchar(tokens)]
    # Remove standalone qualifiers (sp, spp, cf), optional trailing dot.
    tokens <- tokens[!grepl("^(sp|spp|cf)\\.?$", tokens, ignore.case = TRUE)]
    if (!length(tokens)) return("")
    if (length(tokens) >= 4) {
      return(paste(tokens[1:2], collapse = " "))
    }
    paste(tokens, collapse = " ")
  }, character(1))
  x[keep] <- trimws(x[keep])
  x
}

fetch_worms_records <- function(queries, chunk_size = 20L) {
  records_by_query <- stats::setNames(vector("list", length(queries)), queries)
  if (!length(queries)) return(records_by_query)

  idx_groups <- split(seq_along(queries), ceiling(seq_along(queries) / chunk_size))

  for (g in idx_groups) {
    chunk_queries <- queries[g]
    chunk_res <- tryCatch(
      worms_records_names_api(chunk_queries, marine_only = FALSE),
      error = function(e) e
    )

    if (!inherits(chunk_res, "error")) {
      if (is.list(chunk_res) && length(chunk_res) == length(chunk_queries)) {
        for (i in seq_along(chunk_queries)) records_by_query[[chunk_queries[i]]] <- chunk_res[[i]]
        next
      }
      if (is.data.frame(chunk_res) && length(chunk_queries) == 1) {
        records_by_query[[chunk_queries[1]]] <- chunk_res
        next
      }
    }

    # Fallback for long-URI or malformed chunk responses: query one-by-one.
    for (q in chunk_queries) {
      one_res <- tryCatch(
        worms_records_names_api(q, marine_only = FALSE),
        error = function(e) e
      )
      if (!inherits(one_res, "error")) {
        records_by_query[[q]] <- one_res
        next
      }

      # Last fallback: shorten query more aggressively.
      short_q <- sub("^((\\S+\\s+\\S+)).*$", "\\1", q)
      short_q <- trimws(short_q)
      if (!nzchar(short_q)) short_q <- substr(q, 1L, 40L)
      one_short <- tryCatch(
        worms_records_names_api(short_q, marine_only = FALSE),
        error = function(e) NULL
      )
      records_by_query[[q]] <- one_short
    }
  }

  records_by_query
}

# Internal wrapper to make WoRMS calls mockable in tests.
worms_records_names_api <- function(query, marine_only = FALSE) {
  worrms::wm_records_names(query, marine_only = marine_only)
}

get_worms_col <- function(df, col) {
  if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, nrow(df))
}

pick_worms_match <- function(records_df) {
  if (is.null(records_df) || !nrow(records_df)) {
    return(list(
      scientific_name = NA_character_,
      matched_name = NA_character_,
      accepted_name = NA_character_,
      aphia_id = NA_character_,
      accepted_aphia_id = NA_character_,
      status = "unmatched",
      note = "No WoRMS match"
    ))
  }

  aphia <- suppressWarnings(as.numeric(get_worms_col(records_df, "AphiaID")))
  valid_aphia <- suppressWarnings(as.numeric(get_worms_col(records_df, "valid_AphiaID")))
  sci <- get_worms_col(records_df, "scientificname")
  valid_name <- get_worms_col(records_df, "valid_name")
  status_col <- tolower(get_worms_col(records_df, "status"))

  idx <- which(!is.na(valid_aphia) & valid_aphia > 0)[1]
  if (is.na(idx)) idx <- which(!is.na(aphia) & aphia > 0)[1]
  if (is.na(idx)) idx <- 1L

  selected_valid_aphia <- valid_aphia[idx]
  selected_aphia <- aphia[idx]
  selected_valid_name <- valid_name[idx]
  selected_sci <- sci[idx]
  selected_status <- status_col[idx]

  query_aphia <- if (!is.na(selected_aphia) && selected_aphia > 0) {
    as.character(as.integer(selected_aphia))
  } else {
    NA_character_
  }

  accepted_aphia <- if (!is.na(selected_valid_aphia) && selected_valid_aphia > 0) {
    as.character(as.integer(selected_valid_aphia))
  } else if (!is.na(query_aphia) && nzchar(query_aphia)) {
    query_aphia
  } else {
    NA_character_
  }

  accepted_name <- if (!is.na(selected_valid_name) && nzchar(selected_valid_name)) {
    selected_valid_name
  } else if (!is.na(selected_sci) && nzchar(selected_sci)) {
    selected_sci
  } else {
    NA_character_
  }

  if (is.na(accepted_aphia) || !nzchar(accepted_aphia)) {
    return(list(
      scientific_name = selected_sci,
      matched_name = selected_sci,
      accepted_name = accepted_name,
      aphia_id = NA_character_,
      accepted_aphia_id = NA_character_,
      status = "unmatched",
      note = "Matched record lacks AphiaID"
    ))
  }

  match_status <- if (!is.na(selected_status) && nzchar(selected_status) &&
    selected_status != "accepted") {
    "synonym"
  } else {
    "accepted"
  }

  note <- if (nrow(records_df) > 1) {
    paste("Multiple candidates (", nrow(records_df), "), first accepted candidate used", sep = "")
  } else {
    ""
  }

  list(
    scientific_name = selected_sci,
    matched_name = selected_sci,
    accepted_name = accepted_name,
    aphia_id = if (!is.na(query_aphia) && nzchar(query_aphia)) query_aphia else accepted_aphia,
    accepted_aphia_id = accepted_aphia,
    status = match_status,
    note = note
  )
}

#' Build WoRMS match rows for class names
#'
#' Runs WoRMS lookup for one or more class names and returns a standardized
#' results table suitable for UI display or downstream processing.
#'
#' Requires the optional \pkg{worrms} package.
#'
#' @param class_names Character vector of original class names.
#' @param raw_queries Character vector of query strings (same length as
#'   \code{class_names}), typically class names or manual overrides.
#' @return Data frame with columns:
#'   \code{class_name}, \code{query_name}, \code{scientific_name},
#'   \code{matched_name}, \code{accepted_name}, \code{aphia_id}
#'   (query AphiaID), \code{accepted_aphia_id}, \code{status}, and \code{note}.
#' @export
#' @examples
#' \dontrun{
#' build_worms_match_rows(
#'   class_names = c("Prorocentrum_micans", "Alexandrium_cf_tamarense"),
#'   raw_queries = c("Prorocentrum micans", "Alexandrium tamarense")
#' )
#' }
build_worms_match_rows <- function(class_names, raw_queries) {
  raw_queries <- as.character(raw_queries)
  raw_queries[is.na(raw_queries)] <- ""
  raw_queries <- trimws(raw_queries)

  sanitized_queries <- sanitize_worms_query(raw_queries, max_chars = 80L)
  valid_query <- nzchar(sanitized_queries)
  unique_queries <- unique(sanitized_queries[valid_query])
  records_by_query <- if (length(unique_queries)) {
    fetch_worms_records(unique_queries, chunk_size = 20L)
  } else {
    list()
  }

  dplyr::bind_rows(lapply(seq_along(class_names), function(i) {
    cls <- class_names[i]
    raw <- raw_queries[i]
    qry <- sanitized_queries[i]

    if (!nzchar(qry)) {
      too_long <- nzchar(raw) && nchar(raw) > 80L
      return(data.frame(
        class_name = cls,
        query_name = qry,
        scientific_name = NA_character_,
        matched_name = NA_character_,
        accepted_name = NA_character_,
        aphia_id = NA_character_,
        accepted_aphia_id = NA_character_,
        status = if (too_long) "skipped" else "unmatched",
        note = if (too_long) "Skipped: query longer than 80 characters" else "Empty query after sanitization",
        stringsAsFactors = FALSE
      ))
    }

    rec <- records_by_query[[qry]]
    rec_df <- tryCatch({
      if (is.list(rec) && !is.data.frame(rec)) {
        dplyr::bind_rows(rec)
      } else {
        as.data.frame(rec, stringsAsFactors = FALSE)
      }
    }, error = function(e) NULL)

      picked <- pick_worms_match(rec_df)
      data.frame(
        class_name = cls,
        query_name = qry,
        scientific_name = picked$scientific_name,
        matched_name = picked$matched_name,
        accepted_name = picked$accepted_name,
        aphia_id = picked$aphia_id,
        accepted_aphia_id = picked$accepted_aphia_id,
        status = picked$status,
        note = picked$note,
        stringsAsFactors = FALSE
      )
  }))
}
