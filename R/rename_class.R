# Renaming a class in the SQLite annotations database

#' Rename a class across the annotations database
#'
#' Annotations in the SQLite database are stored by class name, so renaming a
#' class in the class list alone leaves earlier annotations under the old
#' name. This function renames the class everywhere in the database: in the
#' annotations, in the class list stored with each sample, in the in-app
#' class list and in the taxonomy table. All renames are made in one
#' transaction: if one of them fails, none is applied.
#'
#' A rename is not a re-annotation: the annotator, the timestamp and the
#' manual/unreviewed flag of the annotations are left as they are.
#'
#' When annotations already use \code{new_name}, the rename would merge two
#' classes, which cannot be undone. It is refused unless \code{merge = TRUE}.
#' On a merge the taxonomy row of \code{new_name} is kept and the row of
#' \code{old_name} is removed; when \code{new_name} has no taxonomy row, the
#' row of \code{old_name} is moved to it. The class lists stored with each
#' sample keep their indices, so a sample can list the merged name at two
#' indices and a later \code{.mat} export stays stable.
#'
#' Files written earlier are not changed: PNG folders named after the class
#' and existing \code{.mat} files keep the old name until they are exported
#' again.
#'
#' @param db_path Path to the SQLite database file.
#' @param old_name Character vector of current class names.
#' @param new_name Character vector of new class names, one per
#'   \code{old_name}.
#' @param merge Allow a rename to a name that already has annotations
#'   (merging the two classes). A single value, or one per rename. Default
#'   \code{FALSE}.
#' @return A data frame with one row per rename: \code{old_name},
#'   \code{new_name}, the number of rows changed in \code{annotations},
#'   \code{class_lists}, \code{global_class_list} and \code{class_taxonomy},
#'   and \code{merged} (\code{TRUE} when annotations already used the new
#'   name).
#' @export
#' @seealso \code{\link{list_classes_db}} for the number of annotations per
#'   class.
#' @examples
#' \dontrun{
#' db_path <- get_db_path(get_default_db_dir())
#' rename_class_db(db_path, "Skeletonema", "Skeletonema_marinoi")
#'
#' # Merge one class into another
#' rename_class_db(db_path, "Diatom_chain", "Diatom", merge = TRUE)
#' }
rename_class_db <- function(db_path, old_name, new_name, merge = FALSE) {
  check_class_renames(old_name, new_name, merge)
  if (!file.exists(db_path)) {
    stop("Database not found: ", db_path, call. = FALSE)
  }
  merge <- rep_len(merge, length(old_name))

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  init_db_schema(con)

  dbExecute(con, "BEGIN IMMEDIATE TRANSACTION")
  tryCatch({
    rows <- lapply(seq_along(old_name), function(i) {
      rename_one_class(con, old_name[i], new_name[i], merge[i])
    })
    dbExecute(con, "COMMIT")
    do.call(rbind, rows)
  }, error = function(e) {
    tryCatch(dbExecute(con, "ROLLBACK"), error = function(re) NULL)
    stop(conditionMessage(e), call. = FALSE)
  })
}

#' Validate the arguments of rename_class_db()
#'
#' @inheritParams rename_class_db
#' @return \code{NULL}, invisibly. Stops with a message on invalid input.
#' @keywords internal
check_class_renames <- function(old_name, new_name, merge) {
  usable <- function(x) is.character(x) && length(x) > 0 &&
    !anyNA(x) && all(nzchar(trimws(x)))
  if (!usable(old_name) || !usable(new_name)) {
    stop("Class names must be non-empty character strings.", call. = FALSE)
  }
  if (length(old_name) != length(new_name)) {
    stop("`old_name` and `new_name` must have the same length.", call. = FALSE)
  }
  if (any(old_name == new_name)) {
    stop("The old and the new class name are the same: ",
         paste(old_name[old_name == new_name], collapse = ", "), call. = FALSE)
  }
  if (!is.logical(merge) || anyNA(merge) ||
      !length(merge) %in% c(1L, length(old_name))) {
    stop("`merge` must be TRUE or FALSE, once or for each rename.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Rename one class inside an open transaction
#'
#' @param con A DBI connection with a transaction in progress.
#' @param old_name,new_name Single class names.
#' @param merge Allow \code{new_name} to have annotations already.
#' @return A one-row data frame, as described in \code{\link{rename_class_db}}.
#' @keywords internal
rename_one_class <- function(con, old_name, new_name, merge) {
  count <- function(sql) dbGetQuery(con, sql, params = list(new_name))$n
  merged <- count("SELECT COUNT(*) AS n FROM annotations WHERE class_name = ?") > 0
  if (merged && !merge) {
    stop("Class '", new_name, "' already has annotations in the database. ",
         "Renaming '", old_name, "' to it would merge the two classes; ",
         "use merge = TRUE to allow that.", call. = FALSE)
  }

  rename_in <- function(table) {
    dbExecute(con,
              paste("UPDATE", table, "SET class_name = ? WHERE class_name = ?"),
              params = list(new_name, old_name))
  }
  # A table that holds each name once: when the new name is there already,
  # its row is kept and the row of the old name is removed
  rename_or_drop <- function(table) {
    taken <- count(paste("SELECT COUNT(*) AS n FROM", table,
                         "WHERE class_name = ?")) > 0
    if (!taken) return(rename_in(table))
    dbExecute(con, paste("DELETE FROM", table, "WHERE class_name = ?"),
              params = list(old_name))
  }

  data.frame(
    old_name = old_name,
    new_name = new_name,
    annotations = rename_in("annotations"),
    class_lists = rename_in("class_lists"),
    global_class_list = rename_or_drop("global_class_list"),
    class_taxonomy = rename_or_drop("class_taxonomy"),
    merged = merged,
    stringsAsFactors = FALSE
  )
}
