
#' Calculate Inter-Rater Agreement Across Multiple Datasets
#'
#' This function calculates the percentage overlap (agreement) between multiple
#' datasets containing the same variables. It's designed to assess reliability
#' and consistency when the same data is processed multiple times or by different
#' systems/raters.
#'
#' @importFrom dplyr arrange distinct
#' @importFrom dplyr "%>%"
#'
#' @param datasets List. A list of data frames to compare. Each data frame should
#'   have the same structure with variable columns (Var1, Var2, etc.).
#'
#' @details
#' The function performs comprehensive overlap analysis:
#' \itemize{
#'   \item Finds common IDs across all datasets to ensure fair comparison
#'   \item Aligns datasets by ID and uses all columns for comparison
#'   \item Calculates pairwise agreement for all possible dataset combinations
#'   \item Computes variable-wise agreement percentages
#'   \item Returns average agreement across all dataset pairs
#' }
#'
#' The function expects exactly the same data structure as produced by the
#' main processing functions: N coded variables (Var1-VarN).
#' Agreement is calculated as the percentage of cases where values match exactly.
#'
#' @param alpha Logical. If TRUE, also compute Krippendorff's Alpha (nominal)
#'   for each variable across the provided datasets. Defaults to FALSE.
#'
#' @return Matrix. If alpha = FALSE: one-row matrix with average percentage
#'   agreement for each variable (columns), rounded to 2 decimals.
#'   If alpha = TRUE: two-row matrix with row "Overlap" (percent agreement)
#'   and row "Alpha" (Krippendorff's Alpha for nominal data; 0-1), rounded
#'   to 3 decimals for Alpha.
#'
#' @examples
#' \dontrun{
#' # Compare three processing runs
#' run1 <- parallel_execute(my_data, slicing_n = 100, cores = 4)
#' run2 <- parallel_execute(my_data, slicing_n = 100, cores = 4)
#' # Switch to GROQ for comparison
#' set_api_specs(provider = "Groq", model = "llama-3.3-70b-versatile",
#'   temp = 0.3, api_key = "your_groq_key")
#' run3 <- parallel_execute(my_data, slicing_n = 100, cores = 4)
#' 
#' overlap_scores <- calculate_overlap(list(run1, run2, run3))
#' # Returns vector like: c(Var1 = 85.6, Var2 = 92.1, ...)
#' }
#'
#' @seealso [parallel_execute()]
#' @export
calculate_overlap <- function(datasets, alpha = FALSE){

  # Input validation
  if (!is.list(datasets) || length(datasets) < 2) {
    stop("datasets must be a list with at least 2 data frames")
  }
  
  if (!all(sapply(datasets, is.data.frame))) {
    stop("All elements in datasets must be data frames")
  }

  num_datasets <- length(datasets)

  # Step 1: Find common IDs across all datasets (using first column Var1)
  common_ids <- Reduce(intersect, lapply(datasets, function(df) df[[1]]))

  # Step 2: Filter and align datasets
  # Filter and align datasets by common IDs
  datasets <- lapply(datasets, function(df) {
    filtered_df <- df[df[[1]] %in% common_ids, ]
    filtered_df %>%
      arrange(as.numeric(filtered_df[[1]])) %>%
      distinct()
  })

  if (num_datasets < 2) {
    stop("Provide at least two datasets for comparison.")
  }

  # Check dimensions match
  if (!all(sapply(datasets, dim) == dim(datasets[[1]]))) {
    stop("All datasets must have the same dimensions.")
  }

  all_vars <- names(datasets[[1]])
  overlaps_list <- list()

  pair_index <- 1

  for (i in 1:(num_datasets - 1)) {
    for (j in (i + 1):num_datasets) {
      df1 <- datasets[[i]]
      df2 <- datasets[[j]]

      # Ensure they have the same structure
      stopifnot(identical(names(df1), names(df2)))

      # Truncate to the same number of rows
      min_rows <- min(nrow(df1), nrow(df2))
      df1 <- df1[1:min_rows, ]
      df2 <- df2[1:min_rows, ]

      # Compute overlap per variable
      overlap <- sapply(all_vars, function(var) {
        mean(df1[[var]] == df2[[var]], na.rm = TRUE)
      })

      overlaps_list[[pair_index]] <- overlap
      pair_index <- pair_index + 1
    }
  }

  # Compute average overlap per variable across all pairs
  avg_overlap <- round((Reduce("+", overlaps_list) / length(overlaps_list)) * 100, digits = 2)

  # Shape result as a one-row matrix by default
  result_mat <- matrix(avg_overlap, nrow = 1, byrow = TRUE)
  rownames(result_mat) <- c("Overlap")
  colnames(result_mat) <- all_vars

  # Optionally compute Krippendorff's Alpha (nominal) per variable
  if (isTRUE(alpha)) {
    # Build aligned ratings per variable across datasets
    # irr::kripp.alpha expects matrix: raters in rows, items in columns
    alpha_vals <- sapply(all_vars, function(var) {
      ratings <- do.call(rbind, lapply(datasets, function(df) as.character(df[[var]])))
      # Try to use irr if available; otherwise return NA (dynamic call avoids Suggests check warning)
      if (!requireNamespace("irr", quietly = TRUE)) {
        return(NA_real_)
      }
      kripp_alpha <- get("kripp.alpha", asNamespace("irr"))
      suppressWarnings({
        out <- try(kripp_alpha(ratings, method = "nominal"), silent = TRUE)
      })
      if (inherits(out, "try-error")) {
        return(NA_real_)
      } else {
        return(as.numeric(round(out$value, digits = 3)))
      }
    })

    # Bind as a second row under Overlap
    result_mat <- rbind(result_mat, Alpha = alpha_vals)
  }

  return(result_mat)
}
