#' Process Missing Data with Parallel Execution
#'
#' This function handles data that failed to process in the initial parallel run
#' by reprocessing missing IDs with optimized batch sizes and parallel execution.
#'
#' @import future
#' @import future.apply
#' @importFrom dplyr filter slice_sample arrange distinct bind_rows
#' @importFrom dplyr "%>%" .data
#' @importFrom rlang sym
#'
#' @param subset_trial Data frame. The original data subset containing all posts.
#' @param missing_ids Numeric vector. IDs that failed processing and need retry.
#' @param n_post Integer. Number of posts to process in each API call.
#' @param cores Integer. Number of CPU cores available for parallel processing.
#' @param provider Character. AI provider to use, either "OpenAI" or "Groq".
#' @param multi_core Logical. If TRUE, uses multicore backend for parallel processing. If FALSE, uses multisession backend.
#' @param verbose Logical. If TRUE (default), progress and status messages are shown via \code{message()}. If FALSE, suppressed.
#'
#' @details
#' It continues until the number of missing items is reduced to a manageable level.
#' The function implements an adaptive retry strategy:
#' \itemize{
#'   \item Continues processing while more than 15 rows remain to be processed
#'   \item Filters out posts with 4 or fewer characters to avoid processing very short content
#'   \item Dynamically adjusts the number of parallel workers based on data size
#'   \item Ensures each worker has at least 15 posts to maintain efficiency
#'   \item Uses the same processing pipeline as the main parallel execution
#'   \item Accumulates successfully processed results across iterations
#'   \item Provides progress feedback on remaining items and filtered posts
#' }
#'
#' The function uses future-based parallel processing with automatic cleanup
#' and includes timing information for performance monitoring.
#'
#' @return Data frame. Successfully processed results from the missing data,
#'   or NULL if no missing data was recovered. The function also outputs
#'   debugging information about filtered posts with 4 or fewer characters.
#'
#' @seealso [parallel_execute()], [track_progress()]
#' @keywords internal
missingness_func <- function(subset_trial, missing_ids, n_post, cores, provider, multi_core = TRUE, verbose = TRUE) {

  # Start timing
  test_start <- Sys.time()

  # Input validation
  if (!is.data.frame(subset_trial)) {
    stop("subset_trial must be a data frame")
  }

  # Get column parameters from package environment
  text_column <- get("text_column", envir = .package_env)

  # Validate that the specified column exists
  if (text_column > ncol(subset_trial)) {
    stop("text_column (", text_column, ") is greater than the number of columns in subset_trial (", ncol(subset_trial), ")")
  }

  # Get column names for easier reference
  # ID column is always column 1 (internal_id)
  id_col_name <- names(subset_trial)[1]
  # Text column is adjusted by +1 because ID column was added
  internal_text_column <- text_column + 1
  text_col_name <- names(subset_trial)[internal_text_column]

  if (!is.numeric(missing_ids) || length(missing_ids) == 0) {
    stop("missing_ids must be a non-empty numeric vector")
  }

  if (!is.numeric(cores) || cores <= 0) {
    stop("cores must be a positive number")
  }

  if (!provider %in% c("OpenAI", "Groq")) {
    stop("provider must be either 'OpenAI' or 'Groq'")
  }

  # Define the %!in% operator for filtering
  `%!in%` <- Negate(`%in%`)

  # Get the values from the main session's package environment
  coding_instruction <- get("coding_instruction", envir = .package_env)
  n_variables <- get("n_variables", envir = .package_env)
  id_column <- get("id_column", envir = .package_env)
  text_column <- get("text_column", envir = .package_env)
  sep <- get("sep", envir = .package_env)

  # Get API specifications to pass to workers
  api_provider <- get("api_provider", envir = .package_env)
  api_model <- get("api_model", envir = .package_env)
  api_temp <- get("api_temp", envir = .package_env)
  multi_response <- get("multi_response", envir = .package_env)

  if (verbose) message("Starting recovery for ", length(missing_ids), " missing IDs")

  missing_data <- subset_trial %>%
    filter(!!sym(id_col_name) %in% missing_ids) %>%
    distinct(!!sym(id_col_name), .keep_all = TRUE)

  # Filter out posts with 4 or fewer characters using dynamic column name
  original_count <- nrow(missing_data)
  missing_data <- missing_data %>%
    filter(nchar(!!sym(text_col_name)) > 4)
  filtered_count <- original_count - nrow(missing_data)
  if (verbose) message("Filtered out ", filtered_count, " posts with 4 or fewer characters in recovery")

  if (verbose) message("Found ", nrow(missing_data), " rows to recover")

  # If no data found, return NULL
  if (nrow(missing_data) == 0) {
    if (verbose) message("No data found for missing IDs after filtering")
    return(NULL)
  }

  # Track total filtered posts across iterations
  total_filtered <- filtered_count

  # Run recovery iterations until fewer than 15 rows remain
  while(nrow(missing_data) > 15) {

    if (verbose) message("Processing recovery batch of ", nrow(missing_data), " rows")

    rep_end <- cores

    while(nrow(missing_data) / rep_end <= 15 && rep_end > 1) {
      rep_end <- rep_end - 1
    }

    if(multi_core == TRUE) {
      plan(multicore, workers = rep_end)
    } else {
      plan(multisession, workers = rep_end)
    }

    # Ensure future.apply is available
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop("Package 'future.apply' is required but not installed.")
    }

    missing_list <- split(missing_data, rep(1:rep_end, length.out = nrow(missing_data)))

    result_list <- future_lapply(seq_along(missing_list), function(i, track_progress, main_func, gpt_func,
                                                                  groq_func, ask_openai, ask_groq) {

      # Use this worker's package environment (not the serialized one from the main session)
      worker_env <- get(".package_env", envir = asNamespace("rapidcodeR"))
      assign("coding_instruction", coding_instruction, envir = worker_env)
      assign("n_variables", n_variables, envir = worker_env)
      assign("id_column", id_column, envir = worker_env)
      assign("text_column", text_column, envir = worker_env)
      assign("sep", sep, envir = worker_env)
      assign("api_provider", api_provider, envir = worker_env)
      assign("api_model", api_model, envir = worker_env)
      assign("api_temp", api_temp, envir = worker_env)
      assign("multi_response", multi_response, envir = worker_env)
      if (provider == "Groq" && exists("GROQ_API_KEY", envir = .package_env)) {
        assign("GROQ_API_KEY", get("GROQ_API_KEY", envir = .package_env), envir = worker_env)
      }

      # Pass worker env and verbose so track_progress can gate messages
      track_progress(missing_list[[i]], i, length(missing_list), n_post, nrow(missing_list[[i]]), provider, worker_env = worker_env, verbose = verbose)
    }, future.seed = TRUE, future.globals = list(
      `%>%` = `%>%`,
      filter = dplyr::filter,
      slice_sample = dplyr::slice_sample,
      arrange = dplyr::arrange,
      distinct = dplyr::distinct,
      bind_rows = dplyr::bind_rows,
      sym = rlang::sym,
      str_remove = stringr::str_remove,
      track_progress = track_progress,
      main_func = main_func,
      gpt_func = gpt_func,
      groq_func = groq_func,
      ask_openai = ask_openai,
      ask_groq = ask_groq,
      verbose = verbose
    ),
      track_progress = track_progress,
      main_func = main_func,
      gpt_func = gpt_func,
      groq_func = groq_func,
      ask_openai = ask_openai,
      ask_groq = ask_groq
    )

    if (verbose) message("The recovery took ", round(as.numeric(difftime(Sys.time(),  test_start, units = "secs")), 2), " seconds!")

    plan(sequential)

    # Memory management: force garbage collection after parallel processing
    gc()

    # Process results and filter out NULL/empty results
    processed_results <- lapply(result_list, function(x) {
      # Check if x is NULL or empty
      if (is.null(x) || length(x) == 0) {
        return(NULL)  # Return NULL for empty/failed results
      }

      # Handle nested list structure (x is a list of dataframes from main_func)
      if (is.list(x) && !is.data.frame(x)) {
        # Flatten the nested list and process each dataframe
        flattened_dataframes <- list()

        for (i in seq_along(x)) {
          if (!is.null(x[[i]]) && is.data.frame(x[[i]]) && nrow(x[[i]]) > 0) {
            # Process each dataframe in the nested list
            processed_df <- x[[i]]

            # Convert the first column to character (dynamically find it)
            if (ncol(processed_df) > 0) {
              first_col <- names(processed_df)[1]
              processed_df[[first_col]] <- as.character(processed_df[[first_col]])
            }

            # Convert the last column to character (dynamically find it)
            if (ncol(processed_df) > 0) {
              last_col_name <- names(processed_df)[ncol(processed_df)]
              processed_df[[last_col_name]] <- as.character(processed_df[[last_col_name]])
            }

            flattened_dataframes[[length(flattened_dataframes) + 1]] <- processed_df
          }
        }

        # Return the list of processed dataframes
        return(flattened_dataframes)
      }

      # Handle single dataframe (legacy case)
      if (is.data.frame(x) && nrow(x) > 0) {
        # Convert the first column to character (dynamically find it)
        if (ncol(x) > 0) {
          first_col <- names(x)[1]
          x[[first_col]] <- as.character(x[[first_col]])
        }

        # Convert the last column to character (dynamically find it)
        if (ncol(x) > 0) {
          last_col_name <- names(x)[ncol(x)]
          x[[last_col_name]] <- as.character(x[[last_col_name]])
        }

        return(list(x))  # Return as a list for consistency
      }

      return(NULL)  # Return NULL for any other case
    })

    # Filter out NULL results and bind
    valid_results <- processed_results[!sapply(processed_results, is.null)]

    failed_tasks <- length(result_list) - length(valid_results)
    if (length(valid_results) == 0) {
      temp_df <- data.frame()  # Create empty data frame
      if (verbose) message("No valid results from recovery iteration")
    } else {
      if (failed_tasks > 0 && verbose) {
        message(failed_tasks, " out of ", length(result_list), " missingness recovery tasks failed.")
      }

      # Handle list of dataframes from each task
      all_dataframes <- list()
      for (i in seq_along(valid_results)) {
        # Each element in valid_results is now a list of dataframes
        if (is.list(valid_results[[i]]) && length(valid_results[[i]]) > 0) {
          for (j in seq_along(valid_results[[i]])) {
            if (is.data.frame(valid_results[[i]][[j]]) && nrow(valid_results[[i]][[j]]) > 0) {
              all_dataframes[[length(all_dataframes) + 1]] <- valid_results[[i]][[j]]
            }
          }
        }
      }

      # Bind all dataframes together
      if (length(all_dataframes) > 0) {
        temp_df <- do.call(bind_rows, all_dataframes)
      } else {
        temp_df <- data.frame()
      }

      if (verbose) message("Recovered ", nrow(temp_df), " rows in this iteration")
    }

    if(exists("restored_dfs")) {
      if (nrow(temp_df) > 0) {
        restored_dfs <- bind_rows(restored_dfs, temp_df)
      }
      # If temp_df is empty, just keep restored_dfs as is
    } else {
      restored_dfs <- temp_df
    }

    if (verbose) message("Recovery iteration completed")

    # Update missing_data for next iteration by removing successfully processed IDs
    if (nrow(temp_df) > 0) {
      # Get the first column (internal_id) which contains the IDs
      first_col <- names(temp_df)[1]
      processed_ids <- as.numeric(temp_df[[first_col]])

      missing_data <- missing_data %>%
        filter(!!sym(id_col_name) %!in% processed_ids)

      # Filter out posts with 4 or fewer characters for next iteration using dynamic column name
      original_count <- nrow(missing_data)
      missing_data <- missing_data %>%
        filter(nchar(!!sym(text_col_name)) > 4)
      iteration_filtered <- original_count - nrow(missing_data)
      total_filtered <- total_filtered + iteration_filtered

      if (iteration_filtered > 0 && verbose) {
        message("Filtered out ", iteration_filtered, " additional posts with 4 or fewer characters")
      }

      if (verbose) message("Remaining rows for next iteration: ", nrow(missing_data))
    } else {
      if (verbose) message("No results from this iteration, stopping recovery")
      break
    }
  }

  # Check if we stopped because of the threshold
  if (nrow(missing_data) <= 15 && nrow(missing_data) > 0 && verbose) {
    message("Recovery stopped because only ", nrow(missing_data), " rows remain (below threshold of 15)")
  }



  if(exists("restored_dfs") && nrow(restored_dfs) > 0){
    if (verbose) {
      message("Recovery completed. Returning ", nrow(restored_dfs), " recovered rows")
      message("Total posts with 4 or fewer characters filtered out in recovery: ", total_filtered)
    }
    return(restored_dfs)
  } else {
    if (verbose) {
      message("Recovery completed. No rows recovered.")
      message("Total posts with 4 or fewer characters filtered out in recovery: ", total_filtered)
    }
    return(NULL)
  }

}
