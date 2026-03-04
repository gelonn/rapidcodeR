
#' Process Text Data Using GROQ AI API
#'
#' This function sends text data to the GROQ AI API for variable extraction,
#' optimized for speed and cost efficiency. It formats the data for API consumption
#' and parses the CSV-formatted response.
#'
#' @importFrom stats setNames
#' @importFrom stringr str_remove
#'
#' @param data_subset Data frame. Subset of text data to process. Must contain
#'   columns specified by `id_column` and `text_column` parameters (set via `set_parameters()`).
#' @param n_post Integer. Number of posts to process in this batch.
#' @param coding_instruction Character. Instructions for the AI model specifying
#'   how to extract and format variables from the text.
#' @param worker_env Environment or NULL. When running in a parallel worker, the worker's
#'   package environment. If NULL (default), \code{.package_env} is used.
#'
#' @details
#' The function performs several optimizations:
#' \itemize{
#'   \item Filters out very short posts (< 4 characters) to reduce noise
#'   \item Sanitizes text by replacing problematic characters (semicolons, quotes)
#'   \item Constructs a single prompt with multiple posts for batch processing
#'   \item Calls GROQ API with parameters from set_api_specs() (e.g. temperature, top_p)
#'   \item Parses the response assuming CSV format with newline separators
#'   \item Always returns a dataframe (single rows become 1-row dataframes)
#' }
#'
#' The function expects the AI to return one line per input post in CSV format.
#' Trailing semicolons are automatically removed from the response.
#'
#' @return Data frame. Each row contains the AI's response for one post,
#'   with columns named Var1, Var2, etc. Returns NULL if no posts remain after filtering.
#'
#' @seealso [ask_groq()], [gpt_func()]
#' @keywords internal
groq_func <- function(data_subset, n_post, coding_instruction, worker_env = NULL) {

  env <- if (is.null(worker_env)) .package_env else worker_env

  # Use all rows in the pre-sampled data subset
  posts <- data_subset

  initial_count <- nrow(posts)

  # Get column parameters from package environment
  id_column <- get("id_column", envir = env)
  text_column <- get("text_column", envir = env)

  # Get column names for easier reference (data_subset has internal_id already removed by main_func)
  id_col_name <- names(posts)[id_column]
  text_col_name <- names(posts)[text_column]

  #Remove ultra-short blurbs:
  posts <- subset(posts, nchar(posts[[text_col_name]]) >= 4)

  filtered_count <- nrow(posts)
  if (initial_count != filtered_count) {
    cat("Filtered out", initial_count - filtered_count, "posts with < 4 characters\n")
  }

  if (nrow(posts) == 0) {
    print("No posts left to process, breaking loop.")
    return(NULL)
  }

  # Get expected number of variables and separator
  n_variables <- if (exists("n_variables", envir = env)) {
    get("n_variables", envir = env)
  } else {
    9  # Default fallback
  }

  sep <- if (exists("sep", envir = env)) {
    get("sep", envir = env)
  } else {
    ";"  # Default fallback
  }

  # Sanitize text content - replace the separator with a colon to avoid conflicts
  posts[[text_col_name]] <- gsub(sep, ":", posts[[text_col_name]])
  posts[[text_col_name]] <- gsub("'", "`", posts[[text_col_name]])
  posts[[text_col_name]] <- gsub("\"", "`", posts[[text_col_name]])

  #Build GPT query out of posts using dynamic column names:
  gpt_posts <- ""

  for (i in 1:nrow(posts)){
    # Always include the ID column in the prompt
    gpt_posts <- paste(gpt_posts, posts[[id_col_name]][i], sep, posts[[text_col_name]][i])
  }

  gpt_prompt <- paste(coding_instruction, gpt_posts)

  gpt_response <- ""

  # Get API parameters from package environment
  api_temp <- if (exists("api_temp", envir = env)) {
    get("api_temp", envir = env)
  } else {
    0.7  # Default fallback
  }

  api_model <- if (exists("api_model", envir = env)) {
    get("api_model", envir = env)
  } else {
    "llama-3.3-70b-versatile"  # Default fallback
  }

  api_key <- if (exists("GROQ_API_KEY", envir = env)) {
    get("GROQ_API_KEY", envir = env)
  } else {
    Sys.getenv("GROQ_API_KEY")
  }

  gpt_response <- ask_groq(gpt_prompt, temp = api_temp, topp = 1, model = api_model, api_key = api_key)

  # Parse multiple lines from API response
  codings <- unlist(strsplit(gpt_response,"\n"))

  # Remove trailing separators and empty lines
  codings <- str_remove(codings, paste0("\\", sep, "\\s*$"))
  codings <- codings[codings != ""]

  # If no valid codings, return NULL
  if (length(codings) == 0 || all(is.na(codings))) {
    return(NULL)
  }

  # Process each coding line into a dataframe row
  result_rows <- list()

  for (i in seq_along(codings)) {
    if (!is.na(codings[i]) && codings[i] != "") {
      # Parse the coding line using the dynamic separator
      values <- unlist(strsplit(codings[i], sep))
      values <- trimws(values)

      # Ensure we have the expected number of columns
      while(length(values) < n_variables) {
        values <- c(values, NA)
      }

      if (length(values) >= n_variables) {
        # Create column names
        col_names <- paste0("Var", 1:n_variables)

        # Create a single-row dataframe
        row_df <- as.data.frame(
          as.list(setNames(values[1:n_variables], col_names)),
          stringsAsFactors = FALSE
        )

        result_rows[[length(result_rows) + 1]] <- row_df
      }
    }
  }

  # Combine all rows into a single dataframe
  if (length(result_rows) > 0) {
    result_df <- do.call(bind_rows, result_rows)
    return(result_df)
  } else {
    return(NULL)
  }

}

