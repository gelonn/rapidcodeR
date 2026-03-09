
#' Execute Parallel Text Processing with AI Models
#'
#' This is the main function that orchestrates parallel processing of text data
#' using AI APIs for variable extraction. It optimizes for speed and cost by
#' distributing work across multiple cores and handling missing data efficiently.
#'
#' @import future
#' @import future.apply
#' @importFrom dplyr filter slice_sample arrange distinct bind_rows
#' @importFrom dplyr "%>%" .data
#' @importFrom rlang sym
#'
#' @param test_data Data frame. Input data containing text to be processed.
#' @param slicing_n Integer. Number of rows to sample from the input data for processing.
#'   The batch size will be automatically calculated by dividing this by the number of cores.
#' @param n_post Integer. Number of posts to process in each API call. Default is 15.
#'   Must be between 1 and 1000. Larger values may be more efficient but use more API credits.
#' @param cores Integer. Number of CPU cores to use for parallel processing. Default is 8.
#'   The function will use the provider specified in set_api_specs() to determine
#'   whether to use OpenAI or GROQ services.
#' @param seed Integer or NULL. Random seed for reproducible sampling. If NULL (default),
#'   sampling is random. If set to a number, ensures identical datasets across runs.
#' @param multi_core Logical. If TRUE, uses multicore backend for parallel processing
#'   (faster but Unix/Mac only). If FALSE (default), uses multisession backend
#'   (works on all platforms including Windows).
#' @param benchmarking Logical. If TRUE, returns processing time in seconds instead of
#'   the result data frame. Default is FALSE.
#' @param verbose Logical. If TRUE (default), progress and status messages are
#'   printed via \code{message()}. If FALSE, such messages are suppressed.
#'
#' @details
#' The function implements a multi-stage parallel processing workflow:
#' \enumerate{
#'   \item Validates parameters and tests API connectivity
#'   \item Randomly samples data and splits into batches for parallel processing
#'   \item Executes parallel processing using futures and tracks progress
#'   \item Handles missing/failed extractions with a secondary processing stage
#'   \item Combines and deduplicates results
#' }
#'
#' The function requires a coding instruction to be set via `set_coding_instruction()`
#' before execution. This instruction tells the AI how to format its responses.
#'
#' @return Data frame. Processed results containing extracted variables,
#'   with duplicates removed (when \code{multi_response=FALSE}) and sorted by ID.
#'
#' @examples
#' \dontrun{
#' # Set up API specifications and coding instruction
#' set_api_specs(provider = "OpenAI", model = "gpt-4", temp = 0.7, api_key = "your_key")
#' set_coding_instruction("Extract sentiment: id,sentiment,confidence")
#'
#' # Process data with random sampling (default)
#' results <- parallel_execute(
#'   test_data = my_data,
#'   slicing_n = 240,
#'   cores = 4
#' )
#'
#' # Process data with reproducible sampling (for calculate_overlap)
#' results1 <- parallel_execute(
#'   test_data = my_data,
#'   slicing_n = 240,
#'   cores = 4,
#'   seed = 123
#' )
#'
#' # Switch to GROQ for comparison
#' set_api_specs(provider = "Groq", model = "llama-3.3-70b-versatile",
#'   temp = 0.3, api_key = "your_groq_key")
#' results2 <- parallel_execute(
#'   test_data = my_data,
#'   slicing_n = 240,
#'   cores = 4,
#'   seed = 123
#' )
#'
#' # Calculate overlap between identical datasets
#' overlap_scores <- calculate_overlap(list(results1, results2))
#'
#' }
#'
#' @seealso [set_api_specs()], [set_coding_instruction()], [set_parameters()]
#' @export
parallel_execute <- function(test_data,
                             slicing_n,
                             n_post = 15,
                             cores = 8,
                             seed = NULL,
                             multi_core = FALSE,
                             benchmarking = FALSE,
                             verbose = TRUE) {

  # Input validation
  if (!is.data.frame(test_data)) {
    stop("test_data must be a data frame")
  }


  # Get column parameters from package environment
  id_column <- get("id_column", envir = .package_env)
  text_column <- get("text_column", envir = .package_env)

  # Validate that the specified columns exist
  if (id_column > ncol(test_data)) {
    stop("id_column (", id_column, ") is greater than the number of columns in test_data (", ncol(test_data), ")")
  }

  if (text_column > ncol(test_data)) {
    stop("text_column (", text_column, ") is greater than the number of columns in test_data (", ncol(test_data), ")")
  }

  # Get column names for easier reference
  id_col_name <- names(test_data)[id_column]
  text_col_name <- names(test_data)[text_column]

  # Validate that the text column contains character data
  if (!is.character(test_data[[text_column]]) && !is.factor(test_data[[text_column]])) {
    stop("Text column (column ", text_column, ") must contain character or factor data, but contains: ", class(test_data[[text_column]])[1])
  }

  # Check if text column has any non-NA values
  if (all(is.na(test_data[[text_column]]))) {
    stop("Text column (column ", text_column, ") contains only NA values. Please check your data.")
  }

  if (!is.numeric(slicing_n) || slicing_n <= 0) {
    stop("slicing_n must be a positive number")
  }

  if (!is.numeric(n_post) || n_post <= 0 || n_post > 1000) {
    stop("n_post must be a positive number between 1 and 1000")
  }

  if (!is.numeric(cores) || cores <= 0) {
    stop("cores must be a positive number")
  }

  # Check if cores is reasonable (not more than available cores + 1)
  available_cores <- parallel::detectCores()
  if (cores > available_cores + 1) {
    warning("Requested cores (", cores, ") exceeds available cores (", available_cores, "). This may cause performance issues.")
  }

  # Get provider from set_api_specs
  if (!exists("api_provider", envir = .package_env)) {
    stop("API provider not set. Please use set_api_specs() to configure your API settings.")
  }

  provider <- get("api_provider", envir = .package_env)
  if (!provider %in% c("OpenAI", "Groq")) {
    stop("Invalid provider. Provider must be 'OpenAI' or 'Groq' as set in set_api_specs()")
  }

  # Get API specifications to pass to workers
  api_model <- get("api_model", envir = .package_env)
  api_temp <- get("api_temp", envir = .package_env)
  multi_response <- get("multi_response", envir = .package_env)

  # Validate seed parameter
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || seed != as.integer(seed))) {
    stop("seed must be NULL or a single integer value")
  }

  # Calculate how many batches will have the larger size
  # For example: 967 posts, 8 cores -> 7 batches of 121, 1 batch of 120
  total_posts <- slicing_n
  base_batch_size <- floor(total_posts / cores)
  extra_posts <- total_posts %% cores

  if (extra_posts > 0) {
    if (verbose) {
      message("- First ", extra_posts, " batches will have ", base_batch_size + 1, " posts each")
      message("- Remaining ", cores - extra_posts, " batches will have ", base_batch_size, " posts each")
    }
  } else {
    if (verbose) message("All ", cores, " batches will have ", base_batch_size, " posts each")
  }

  #Making sure slicing_n is not greater than the number of rows in test_data
  if (slicing_n > nrow(test_data)) {
    stop("Make sure that slicing_n is not greater than the number of rows in test_data")
  }

  if (provider == "OpenAI") {
    test_answer <- ask_openai("What is the capital of Norway?")
    if (!is.na(test_answer)) {
      if (verbose) message("There are credits in the account!")
    } else {
      stop("OpenAI API test failed. Please check your API key and internet connection.")
    }
  } else if (provider == "Groq") {
    test_answer <- ask_groq("What is the capital of Norway?", temp = api_temp,
                            model = api_model, api_key = get("GROQ_API_KEY", envir = .package_env))
    if (!is.na(test_answer)) {
      if (verbose) message("There are credits in the account!")
    } else {
      stop("GROQ API test failed. Please check your API key and internet connection.")
    }
  }

  # Set seed for reproducible sampling if specified
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # First, add unique internal ID column to the full dataset
  test_data$internal_id <- 1:nrow(test_data)
  test_data <- test_data[, c("internal_id", setdiff(names(test_data), "internal_id"))]

  # Adjust column positions (add 1 because we added internal_id column)
  internal_id_column <- id_column + 1
  internal_text_column <- text_column + 1
  internal_id_col_name <- names(test_data)[internal_id_column]
  internal_text_col_name <- names(test_data)[internal_text_column]

  # Sample rows for processing
  subset_trial <- test_data %>% slice_sample(n = slicing_n, replace = FALSE)

  # Filter out posts with 4 or fewer characters (before counting missing IDs)
  original_count <- nrow(subset_trial)
  filtered_data <- subset_trial %>%
    filter(nchar(!!sym(internal_text_col_name)) > 4)
  filtered_count <- original_count - nrow(filtered_data)

  if (verbose) message("Filtered out ", filtered_count, " posts with 4 or fewer characters")

  # Check if we have any data left after filtering
  if (nrow(filtered_data) == 0) {
    stop("No data remaining after filtering posts with 4 or fewer characters. Please check your text column data.")
  }

  # Store all IDs being processed for later comparison using internal ID column
  all_processed_ids <- subset_trial$internal_id

  # Create batches with calculated sizes
  # First create a vector of batch sizes
  batch_sizes <- rep(base_batch_size, cores)
  if (extra_posts > 0) {
    batch_sizes[1:extra_posts] <- base_batch_size + 1
  }

  # Create cumulative indices for splitting
  batch_indices <- cumsum(c(0, batch_sizes))

  # Split the data into batches
  df_list <- list()
  for (i in 1:cores) {
    start_idx <- batch_indices[i] + 1
    end_idx <- batch_indices[i + 1]
    if (start_idx <= nrow(subset_trial)) {
      df_list[[i]] <- subset_trial[start_idx:min(end_idx, nrow(subset_trial)), , drop = FALSE]
    }
  }

  # Remove any empty batches
  df_list <- df_list[sapply(df_list, nrow) > 0]

  if(multi_core == TRUE) {
    plan(multicore, workers = cores)
  } else {
    plan(multisession, workers = cores) #Starts a multisession, using cores amount of cores
  }
  # Check if coding instruction and parameters are set in the main session
  if(!exists("coding_instruction", envir = .package_env) ||
            is.na(get("n_variables", envir = .package_env))) {
    stop("Coding instruction has not been set. Use set_coding_instruction() before running parallel_execute()")
  }

  if(!exists("n_variables", envir = .package_env) ||
      is.na(get("n_variables", envir = .package_env))) {
    stop("Variable number has not been set. Use set_parameters() before running parallel_execute()")
  }

  if(!exists("text_column", envir = .package_env) ||
      is.na(get("text_column", envir = .package_env))) {
    stop("Text column has not been set. Use set_parameters() before running parallel_execute()")
  }

  # Get the values from the main session's package environment
  coding_instruction <- get("coding_instruction", envir = .package_env)
  n_variables <- get("n_variables", envir = .package_env)
  id_column <- get("id_column", envir = .package_env)
  text_column <- get("text_column", envir = .package_env)
  sep <- get("sep", envir = .package_env)

  test_start <- Sys.time()

  # Ensure needed package is available for parallel helpers
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    stop("Package 'future.apply' is required but not installed.")
  }

  # Check for other required packages
  required_packages <- c("dplyr", "stringr", "rlang")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("Required packages are missing: ", paste(missing_packages, collapse = ", "), ". Please install them.")
  }

  # future.apply::future_lapply executes the loop body on the workers created
  # by the active future plan (multisession here). All referenced objects and
  # functions are automatically exported; no need to re-source files.

  result_list <- future_lapply(seq_along(df_list), function(i, track_progress, main_func, gpt_func,
                                                            groq_func, ask_openai, ask_groq) {

    # Use this worker's package environment (not the serialized one from the main session)
    worker_env <- get(".package_env", envir = asNamespace("rapidcodeR"))
    assign("coding_instruction", coding_instruction, envir = worker_env)
    assign("n_variables", n_variables, envir = worker_env)
    assign("id_column", id_column, envir = worker_env)
    assign("text_column", text_column, envir = worker_env)
    assign("sep", sep, envir = worker_env)
    assign("api_model", api_model, envir = worker_env)
    assign("api_temp", api_temp, envir = worker_env)
    assign("multi_response", multi_response, envir = worker_env)
    if (provider == "Groq" && exists("GROQ_API_KEY", envir = .package_env)) {
      assign("GROQ_API_KEY", get("GROQ_API_KEY", envir = .package_env), envir = worker_env)
    }

    # Pass the data and worker env explicitly to track_progress
    # Note: nrow(df_list[[i]]) is the batch size, n_post is the API call size
    track_progress(df_list[[i]], i, length(df_list), n_post, nrow(df_list[[i]]), provider, worker_env = worker_env, verbose = verbose)

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

  plan(sequential) #Closes the multisession

  # Memory management: force garbage collection after parallel processing
  gc()

  # Process results from parallel execution
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

  # Check if we have any valid results
  failed_tasks <- length(result_list) - length(valid_results)
  if (length(valid_results) == 0) {
    warning("No valid results from parallel processing. All ", length(result_list), " tasks failed.")
    test_dfs <- data.frame()  # Create empty data frame
  } else {
    if (failed_tasks > 0) {
      warning(failed_tasks, " out of ", length(result_list), " tasks failed and were excluded from results.")
    }

    # Flatten all valid results into a single list of dataframes
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
      test_dfs <- do.call(bind_rows, all_dataframes)

      # Deduplicate results by internal_id only when multi_response=FALSE.
      # When multi_response=TRUE, multiple rows per ID are intentional (one post, multiple extracted responses).
      if (nrow(test_dfs) > 0 && !multi_response) {
        first_col_temp <- names(test_dfs)[1]
        duplicates_before <- nrow(test_dfs)
        test_dfs <- test_dfs %>% distinct(.data[[first_col_temp]], .keep_all = TRUE)
        duplicates_removed <- duplicates_before - nrow(test_dfs)
        if (duplicates_removed > 0 && verbose) {
          message("Removed ", duplicates_removed, " duplicate rows from initial results")
        }
      }
    } else {
      test_dfs <- data.frame()
    }
  }

  if (verbose) message("Initial results have ", nrow(test_dfs), " rows")

  # Compare processed results with original IDs to find missing ones
  if (nrow(test_dfs) > 0) {
    # Get the first column (internal_id) which contains the IDs
    first_col <- names(test_dfs)[1]
    # Deduplicate processed_ids to ensure accurate missing ID calculation
    # This prevents duplicate IDs from affecting the count of processed vs missing IDs
    processed_ids <- unique(as.numeric(test_dfs[[first_col]]))
    missing_ids <- all_processed_ids[!all_processed_ids %in% processed_ids]
    if (verbose) {
      message("Unique processed IDs: ", length(processed_ids))
      message("Missing IDs after parallel processing: ", length(missing_ids))
    }
    # Only call missingness_func if there are missing IDs
    if (length(missing_ids) > 0) {
      if (verbose) message("Attempting to recover ", length(missing_ids), " missing IDs")
      restored_dfs <- missingness_func(subset_trial, missing_ids, n_post, cores,
                                       provider, multi_core, verbose = verbose)
    } else {
      restored_dfs <- NULL
      if (verbose) message("No missing IDs to recover")
    }
  } else {
    # If no results at all, all IDs are missing
    missing_ids <- all_processed_ids
    if (verbose) message("No results from parallel processing. Attempting to recover all ", length(missing_ids), " IDs")
    restored_dfs <- missingness_func(subset_trial, missing_ids, n_post, cores, provider, multi_core, verbose = verbose)
  }

  process_time <- round(as.numeric(difftime(Sys.time(),  test_start, units = "secs")), 2)

  if (verbose) message("The processing took ", process_time, " seconds!")

  # Memory management: force garbage collection after recovery processing
  gc()

  # Combine results if recovery was successful
  if (!is.null(restored_dfs) && nrow(restored_dfs) > 0) {
    # Convert first column (internal_id) to character for binding
    if (nrow(test_dfs) > 0) {
      first_col <- names(test_dfs)[1]
      test_dfs[[first_col]] <- as.character(test_dfs[[first_col]])
    }
    first_col_restored <- names(restored_dfs)[1]
    restored_dfs[[first_col_restored]] <- as.character(restored_dfs[[first_col_restored]])

    all_data <- bind_rows(test_dfs, restored_dfs)
    if (verbose) message("Recovered ", nrow(restored_dfs), " additional rows")
    # Deduplicate after combining restored results only when multi_response=FALSE
    # (when multi_response=TRUE, multiple rows per ID are intentional)
    if (nrow(all_data) > 0 && !multi_response) {
      first_col_all <- names(all_data)[1]
      duplicates_before_combine <- nrow(all_data)
      all_data <- all_data %>% distinct(.data[[first_col_all]], .keep_all = TRUE)
      duplicates_removed_combine <- duplicates_before_combine - nrow(all_data)
      if (duplicates_removed_combine > 0 && verbose) {
        message("Removed ", duplicates_removed_combine, " duplicate rows after combining restored results")
      }
    }
  } else {
    all_data <- test_dfs
    if (verbose) message("No additional rows recovered")
  }

  if (verbose) message("Before final processing: ", nrow(all_data), " rows")

  # Sort by internal_id (first column) before removing it
  if (ncol(all_data) > 0) {
    internal_id_col <- names(all_data)[1]
    if (!is.na(internal_id_col)) {
      all_data <- all_data %>%
        arrange(as.numeric(.data[[internal_id_col]]))
    }
  }

  # Remove the internal_id column (first column) from the final output
  if (ncol(all_data) > 1) {
    output <- all_data[, -1, drop = FALSE]
  } else {
    output <- all_data
  }

  # Remove internal columns (multiple_response_flag, user_id) if present (no longer needed)
  internal_cols <- c("multiple_response_flag", "user_id")
  cols_to_drop <- internal_cols[internal_cols %in% names(output)]
  if (length(cols_to_drop) > 0) {
    output <- output[, setdiff(names(output), cols_to_drop)]
  }

  if (verbose) {
    message("After processing: ", nrow(output), " rows")
    message("Total posts with 4 or fewer characters filtered out: ", filtered_count)
  }

  if(benchmarking == FALSE) {
    return(output)
  } else {
    return(process_time)
  }

}




