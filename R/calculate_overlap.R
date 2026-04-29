
#' Calculate Inter-Rater Agreement Across Multiple Datasets
#'
#' This function calculates the percentage overlap (agreement) between multiple
#' datasets containing the same variables. It's designed to assess reliability
#' and consistency when the same data is processed multiple times or by different
#' systems/raters.
#'
#' @param datasets List. A list of data frames to compare. Each data frame should
#'   have the same column structure with variable columns (Var1, Var2, etc.).
#'
#' @param alpha Logical. If TRUE, also compute Krippendorff's Alpha (nominal)
#'   for each variable across the provided datasets. Defaults to FALSE.
#'
#' @param key_cols Character vector or NULL. Column name(s) used to match rows
#'   across datasets. Use this when datasets have different row counts due to a
#'   compound row identity (e.g. \code{key_cols = c("Var1", "Var3")}). Rows with
#'   NA in any key column are dropped before matching. When NULL (default), rows
#'   are matched on the first column only, which is the standard behaviour for
#'   single-response output from \code{parallel_execute()}.
#'
#' @details
#' The function performs comprehensive overlap analysis:
#' \itemize{
#'   \item Finds common rows across all datasets based on \code{key_cols}
#'   \item Aligns and sorts datasets by the key before comparing
#'   \item Calculates pairwise agreement for all possible dataset combinations
#'   \item Computes variable-wise agreement percentages
#'   \item Returns average agreement across all dataset pairs
#' }
#'
#' Agreement is calculated as the percentage of cases where values match exactly.
#' The number of rows used in the comparison is printed as a message (\code{N = ...}).
#'
#' @return Matrix. If alpha = FALSE: one-row matrix with average percentage
#'   agreement for each variable (columns), rounded to 2 decimals.
#'   If alpha = TRUE: two-row matrix with row "Overlap" (percent agreement)
#'   and row "Alpha" (Krippendorff's Alpha for nominal data), rounded
#'   to 3 decimals for Alpha.
#'
#' @examples
#' \dontrun{
#' # Standard use: compare two processing runs (single-response output)
#' run1 <- parallel_execute(my_data, slicing_n = 100, cores = 4)
#' run2 <- parallel_execute(my_data, slicing_n = 100, cores = 4)
#' calculate_overlap(list(run1, run2))
#'
#' # Multi-response output: datasets may differ in row count;
#' # match on compound key (e.g. speech ID + paragraph number)
#' calculate_overlap(list(run1, run2), key_cols = c("Var1", "Var3"))
#' }
#'
#' @seealso [parallel_execute()]
#' @export
calculate_overlap <- function(datasets, alpha = FALSE, key_cols = NULL) {

  # Input validation
  if (!is.list(datasets) || length(datasets) < 2) {
    stop("datasets must be a list with at least 2 data frames")
  }
  if (!all(sapply(datasets, is.data.frame))) {
    stop("All elements in datasets must be data frames")
  }

  num_datasets <- length(datasets)
  first_names <- names(datasets[[1]])

  if (!all(sapply(datasets, function(df) identical(names(df), first_names)))) {
    stop("All datasets must have the same column names.")
  }

  if (!is.null(key_cols)) {
    if (!is.character(key_cols) || !all(key_cols %in% first_names)) {
      stop("key_cols must be a character vector of column names present in all datasets.")
    }
  }

  # Align datasets on common rows -----------------------------------------------

  if (!is.null(key_cols)) {

    # Compound key: paste key columns together, drop rows with NA in any key col
    make_key <- function(df) {
      key_df <- df[, key_cols, drop = FALSE]
      complete <- complete.cases(key_df)
      df_clean <- df[complete, ]
      keys <- do.call(paste, c(df_clean[, key_cols, drop = FALSE], sep = "|||"))
      list(df = df_clean, keys = keys)
    }

    keyed <- lapply(datasets, make_key)
    common_keys <- Reduce(intersect, lapply(keyed, `[[`, "keys"))

    if (length(common_keys) == 0) {
      stop("No common rows found across datasets using the specified key_cols.")
    }

    datasets <- lapply(keyed, function(x) {
      matched <- x$df[x$keys %in% common_keys, ]
      sort_order <- order(do.call(paste, c(matched[, key_cols, drop = FALSE], sep = "|||")))
      matched <- matched[sort_order, ]
      matched[!duplicated(matched[, key_cols, drop = FALSE]), ]
    })

  } else {

    # Default: align on first column only
    common_ids <- Reduce(intersect, lapply(datasets, function(df) df[[1]]))

    datasets <- lapply(datasets, function(df) {
      filtered <- df[df[[1]] %in% common_ids, ]
      sort_order <- order(as.numeric(filtered[[1]]))
      filtered <- filtered[sort_order, ]
      filtered[!duplicated(filtered), ]
    })
  }

  # Verify alignment succeeded
  row_counts <- sapply(datasets, nrow)
  if (length(unique(row_counts)) > 1) {
    stop(
      "Datasets have different row counts after alignment (",
      paste(row_counts, collapse = ", "), " rows). ",
      "If your data uses a compound row key, specify key_cols."
    )
  }

  # Pairwise overlap ------------------------------------------------------------

  n_pairs <- num_datasets * (num_datasets - 1L) / 2L
  overlaps_list <- vector("list", n_pairs)
  all_vars <- first_names
  pair_index <- 1L

  for (i in 1:(num_datasets - 1)) {
    for (j in (i + 1):num_datasets) {
      overlaps_list[[pair_index]] <- sapply(all_vars, function(var) {
        mean(datasets[[i]][[var]] == datasets[[j]][[var]], na.rm = TRUE)
      })
      pair_index <- pair_index + 1L
    }
  }

  avg_overlap <- round(
    (Reduce("+", overlaps_list) / n_pairs) * 100,
    digits = 2
  )

  n_rows <- nrow(datasets[[1]])
  message("N = ", n_rows)

  result_mat <- matrix(
    avg_overlap, nrow = 1, byrow = TRUE,
    dimnames = list("Overlap", all_vars)
  )

  if (isTRUE(alpha)) {
    alpha_vals <- sapply(all_vars, function(var) {
      ratings <- do.call(rbind, lapply(datasets, function(df) as.character(df[[var]])))
      if (!requireNamespace("irr", quietly = TRUE)) return(NA_real_)
      kripp_alpha <- get("kripp.alpha", asNamespace("irr"))
      suppressWarnings(out <- try(kripp_alpha(ratings, method = "nominal"), silent = TRUE))
      if (inherits(out, "try-error")) NA_real_ else as.numeric(round(out$value, digits = 3))
    })
    result_mat <- rbind(result_mat, Alpha = alpha_vals)
  }

  return(result_mat)
}
