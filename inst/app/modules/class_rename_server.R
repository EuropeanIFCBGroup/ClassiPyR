# Renaming classes across the database
#
# A class renamed in the class list editor, or by the WoRMS "rename to
# accepted_name" option, is renamed in the SQLite database too
# (ClassiPyR::rename_class_db()) and in everything the session holds in
# memory, after the user has confirmed how many annotations are rewritten.

empty_class_renames <- function() {
  data.frame(old_name = character(), new_name = character(),
             stringsAsFactors = FALSE)
}

# Renames between two versions of the class list: lists of the same length
# with a different name at the same position, where the old name is gone and
# the new name was not in the list before. Swapped or reordered classes, and
# classes added or removed, are not renames.
detect_class_renames <- function(old_classes, new_classes) {
  if (length(old_classes) == 0 || length(old_classes) != length(new_classes)) {
    return(empty_class_renames())
  }
  renamed <- old_classes != new_classes &
    !old_classes %in% new_classes & !new_classes %in% old_classes
  data.frame(old_name = old_classes[renamed], new_name = new_classes[renamed],
             stringsAsFactors = FALSE)
}

# Class names with the renames applied
rename_classes <- function(class_names, renames) {
  if (is.null(class_names)) return(NULL)
  idx <- match(class_names, renames$old_name)
  ifelse(is.na(idx), class_names, renames$new_name[idx])
}

rename_in_classifications <- function(classifications, renames) {
  if (is.null(classifications)) return(NULL)
  transform(classifications,
            class_name = rename_classes(class_name, renames),
            stringsAsFactors = FALSE)
}

rename_in_changes_log <- function(changes_log, renames) {
  if (is.null(changes_log)) return(NULL)
  transform(changes_log,
            original_class = rename_classes(original_class, renames),
            new_class = rename_classes(new_class, renames),
            stringsAsFactors = FALSE)
}

# A session cache entry (see save_to_cache()) with the renames applied
rename_in_cache_entry <- function(entry, renames) {
  utils::modifyList(entry, list(
    classifications = rename_in_classifications(entry$classifications, renames),
    original_classifications =
      rename_in_classifications(entry$original_classifications, renames),
    changes_log = rename_in_changes_log(entry$changes_log, renames)
  ))
}

# What each rename does to the database. `class_counts` is the result of
# list_classes_db(). A rename onto a name that has annotations is a merge.
class_rename_plan <- function(renames, class_counts) {
  count_of <- function(names) {
    n <- class_counts$count[match(names, class_counts$class_name)]
    ifelse(is.na(n), 0, n)
  }
  target <- count_of(renames$new_name)
  data.frame(renames,
             annotations = count_of(renames$old_name),
             target_annotations = target,
             merge = target > 0,
             stringsAsFactors = FALSE)
}

# Renames that rewrite no annotation are applied without asking
rename_needs_confirmation <- function(plan) {
  any(plan$annotations > 0) || any(plan$merge)
}

# Renames asked for by "Rename classes to WoRMS accepted_name". A class is
# left alone (and counted as a conflict) when its accepted name is already in
# the class list, including a name an earlier rename in the same batch took.
worms_rename_candidates <- function(matched, class2use) {
  new_name <- matched$accepted_name
  wanted <- !is.na(new_name) & nzchar(new_name) &
    matched$class_name != new_name & matched$class_name %in% class2use
  free <- wanted & !new_name %in% class2use
  first <- free & !duplicated(ifelse(free, new_name, NA_character_))
  list(
    renames = data.frame(old_name = matched$class_name[first],
                         new_name = new_name[first],
                         stringsAsFactors = FALSE),
    conflicts = sum(wanted & !first)
  )
}

class_rename_lines <- function(plan) {
  lapply(seq_len(nrow(plan)), function(i) {
    detail <- if (plan$merge[i]) {
      sprintf("%d annotations, merged into the %d that '%s' already has",
              plan$annotations[i], plan$target_annotations[i], plan$new_name[i])
    } else {
      sprintf("%d annotations", plan$annotations[i])
    }
    tags$li(tags$strong(plan$old_name[i]), " → ",
            tags$strong(plan$new_name[i]), sprintf(" (%s)", detail))
  })
}

class_rename_modal <- function(plan) {
  merges <- plan[plan$merge, , drop = FALSE]
  plain <- plan[!plan$merge, , drop = FALSE]
  modalDialog(
    title = "Rename classes in the database",
    easyClose = FALSE,
    tags$p("Annotations are stored by class name. Renaming these classes ",
           "rewrites the annotations already in the database:"),
    if (nrow(plain) > 0) tags$ul(class_rename_lines(plain)),
    if (nrow(merges) > 0) tags$div(
      class = "alert alert-danger",
      tags$strong("These renames merge two classes and cannot be undone. "),
      "Afterwards the annotations of the two classes cannot be told apart.",
      tags$ul(class_rename_lines(merges)),
      checkboxInput("confirm_class_merge",
                    "Merge these classes", value = FALSE)
    ),
    tags$p(class = "text-muted", style = "font-size: 12px;",
           "The annotator, time and review status of the annotations are ",
           "kept. PNG folders and .mat files written earlier keep the old ",
           "name until they are exported again."),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_class_rename",
                   if (nrow(merges) > 0) "Rename and merge" else "Rename",
                   class = if (nrow(merges) > 0) "btn-danger" else "btn-primary")
    )
  )
}

setup_class_rename_server <- function(input, output, session, rv, config) {
  # The renames waiting for confirmation: list(plan, on_done)
  pending <- reactiveVal(NULL)

  rename_in_memory <- function(renames) {
    rv$classifications <- rename_in_classifications(rv$classifications, renames)
    rv$original_classifications <-
      rename_in_classifications(rv$original_classifications, renames)
    rv$cached_validation_classifications <-
      rename_in_classifications(rv$cached_validation_classifications, renames)
    rv$class_review_original <-
      rename_in_classifications(rv$class_review_original, renames)
    rv$class_review_class <- rename_classes(rv$class_review_class, renames)
    rv$changes_log <- rename_in_changes_log(rv$changes_log, renames)
    rv$session_cache <- lapply(rv$session_cache, rename_in_cache_entry, renames)
    if (!is.null(rv$saved_state)) {
      rv$saved_state <- utils::modifyList(rv$saved_state, list(
        classes = rename_classes(rv$saved_state$classes, renames)
      ))
    }
    rv$class_rename_count <- rv$class_rename_count + 1L
  }

  # Rename in the database (all or nothing), then in memory. TRUE on success.
  apply_renames <- function(plan) {
    db_path <- get_db_path(config$db_folder)
    renamed <- tryCatch({
      if (file.exists(db_path)) {
        ClassiPyR::rename_class_db(db_path, plan$old_name, plan$new_name,
                                   merge = plan$merge)
      }
      TRUE
    }, error = function(e) {
      showNotification(paste("Classes were not renamed:", e$message),
                       type = "error", duration = 10)
      FALSE
    })
    if (renamed) rename_in_memory(plan)
    renamed
  }

  # Apply `renames` (asking first when annotations are rewritten), then call
  # on_done(renames). Nothing is called when the user cancels or the database
  # rename fails. With allow_merge = FALSE, renames that would merge two
  # classes are dropped and on_done() receives the remaining ones.
  rename_then <- function(renames, on_done, allow_merge = TRUE) {
    if (nrow(renames) == 0) return(on_done(renames))

    plan <- class_rename_plan(
      renames, ClassiPyR::list_classes_db(get_db_path(config$db_folder))
    )
    if (!allow_merge) plan <- plan[!plan$merge, , drop = FALSE]
    if (nrow(plan) == 0) return(on_done(empty_class_renames()))

    if (rename_needs_confirmation(plan)) {
      pending(list(plan = plan, on_done = on_done))
      showModal(class_rename_modal(plan))
    } else if (apply_renames(plan)) {
      finish(plan, on_done)
    }
    invisible(NULL)
  }

  # The class filter of the gallery lists class names too; it is rebuilt
  # after on_done() so it is marked against the updated class list
  finish <- function(plan, on_done) {
    renames <- plan[c("old_name", "new_name")]
    on_done(renames)
    if (is.null(rv$classifications)) return(invisible(NULL))
    available <- sort(unique(rv$classifications$class_name))
    unmatched <- setdiff(available, c(rv$class2use, "unclassified"))
    updateSelectInput(session, "class_filter",
                      choices = build_class_filter_choices(available, unmatched),
                      selected = rename_classes(input$class_filter, renames))
  }

  observeEvent(input$confirm_class_rename, {
    request <- pending()
    req(request)
    if (any(request$plan$merge) && !isTRUE(input$confirm_class_merge)) {
      showNotification("Tick 'Merge these classes' to confirm the merge.",
                       type = "warning")
      return()
    }
    pending(NULL)
    removeModal()
    if (apply_renames(request$plan)) finish(request$plan, request$on_done)
  })

  list(rename_then = rename_then)
}
