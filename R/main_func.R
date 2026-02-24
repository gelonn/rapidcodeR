#' Main Text Processing Function for AI-Based Variable Extraction
#'
#' This function processes batches of text data using AI models to extract
#' machine-readable variables. It implements robust error handling and retry
#' logic to ensure reliable processing even with API failures.
#'
#' @importFrom dplyr filter slice_sample bind_rows
#' @importFrom dplyr "%>%"
#' @importFrom rlang sym
#'
#' @param df Data frame. Input data subset containing text to be processed.
#' @param to_code_max_id Integer. Maximum number of posts to process in this batch.
#' @param n_post Integer. Number of posts to process per API call (typically 15).
#' @param provider Character. AI provider to use, either "OpenAI" or "Groq".
#'
#' @details
#' The function implements a robust processing loop that:
#' \itemize{
#'   \item Samples posts randomly to avoid processing order bias
#'   \item Makes API calls in manageable batches
#'   \item Validates and cleans AI responses
#'   \item Handles API failures with counter and early stopping
#'   \item Tracks missing IDs for potential reprocessing
#'   \item Collects dataframes into a list for later binding
#' }
#'
#' The AI models are instructed to return data in CSV format for efficient parsing.
#' The function expects responses with exactly N columns as set by set_parameters().
#'
#' @return List of data frames. Each element contains successfully processed results
#'   with extracted variables. Returns empty list if no successful processing.
#'
#' @examples
#' \dontrun{
#' instruction <- "Extract variables: var1,var2,var3,var4,var5,var6"  # for set_parameters(n_variables = 6)
#' result <- main_func(
#'   df = my_data_subset,
#'   to_code_max_id = 30,
#'   n_post = 15,
#'   provider = "OpenAI"
#' )
#' processed_data <- bind_rows(result)  # Combine all dataframes
#' }
#'
#' @seealso [gpt_func()], [groq_func()], [make_value_row()]
#' @export
main_func <- function(df, to_code_max_id, n_post, provider) {

  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  # Get column parameters from package environment
  id_column <- get("id_column", envir = globalenv())
  text_column <- get("text_column", envir = globalenv())

  # Validate that the specified columns exist
  if (id_column > ncol(df)) {
    stop("id_column (", id_column, ") is greater than the number of columns in df (", ncol(df), ")")
  }

  if (text_column > ncol(df)) {
    stop("text_column (", text_column, ") is greater than the number of columns in df (", ncol(df), ")")
  }

  # Get column names for easier reference
  # ID column is always column 1 (internal_id)
  id_col_name <- names(df)[1]
  # User's ID column is adjusted by +1 because internal_id column was added
  internal_id_column <- id_column + 1
  user_id_col_name <- names(df)[internal_id_column]
  # Text column is adjusted by +1 because internal_id column was added
  internal_text_column <- text_column + 1
  text_col_name <- names(df)[internal_text_column]

  if (!is.numeric(to_code_max_id) || to_code_max_id <= 0) {
    stop("to_code_max_id must be a positive number")
  }

  if (!is.numeric(n_post) || n_post <= 0) {
    stop("n_post must be a positive number")
  }

  if (!provider %in% c("OpenAI", "Groq")) {
    stop("provider must be either 'OpenAI' or 'Groq'")
  }

  # Get multi_response from global environment
  multi_response <- if (exists("multi_response", envir = globalenv())) {
    get("multi_response", envir = globalenv())
  } else {
    FALSE  # Default fallback
  }

  if (!is.logical(multi_response) || length(multi_response) != 1) {
    stop("multi_response must be a single logical value (TRUE or FALSE)")
  }

  # Validate multi_response constraint
  if (multi_response && n_post != 1) {
    stop("multi_response = TRUE can only be used with n_post = 1")
  }

  coding_instruction <- get("coding_instruction", envir = globalenv())

  n_variables <- as.numeric(get("n_variables", envir = globalenv()))

  `%!in%` <- Negate(`%in%`)

  coded_ids <- as.vector(NULL)

  # Initialize list to collect dataframes
  result_list <- list()


  # Single while loop):
  while(to_code_max_id > 0) {

    # Get the current batch of data to process using dynamic column names
    data_sample <- df %>%
      filter(!!sym(id_col_name) %!in% coded_ids) %>%
      slice_sample(n = min(n_post, to_code_max_id), replace = FALSE)

      if (nrow(data_sample) == 0) {
        break
      }

      # Store the internal IDs for this batch
      batch_internal_ids <- data_sample[[id_col_name]]

      # Store user IDs for this batch
      batch_user_ids <- data_sample[[user_id_col_name]]

      # Remove internal_id column for API processing (keep user_id and other columns)
      data_for_api <- data_sample[, -1, drop = FALSE]

      # Process with API
      if (provider == "OpenAI") {
        result_df <- gpt_func(data_for_api, n_post, coding_instruction)
      } else if (provider == "Groq") {
        result_df <- groq_func(data_for_api, n_post, coding_instruction)
      }


      # Process API response and add internal_id back
      if (!is.null(result_df) && nrow(result_df) > 0) {

        # Handle row count validation based on multi_response setting
        if (!multi_response && nrow(result_df) != length(batch_internal_ids)) {
          # Skip this iteration if multi_response=FALSE but row counts don't match
          # Don't mark IDs as processed since this batch failed
          to_code_max_id <- to_code_max_id - nrow(data_sample)
          next
        }

        # Add IDs back to the results
        if (multi_response) {
          # For multi_response=TRUE, all rows get the same user_id and internal_id
          result_df$user_id <- batch_user_ids[1]
          result_df$internal_id <- batch_internal_ids[1]
        } else {
          # For multi_response=FALSE, each row gets its corresponding user_id and internal_id
          # Only assign IDs for the rows that actually exist in the result
          n_result_rows <- nrow(result_df)
          #result_df$user_id <- batch_user_ids[1:n_result_rows]
          result_df$internal_id <- batch_internal_ids[1:n_result_rows]
        }

        # Move internal_id to first column
        result_df <- result_df[, c("internal_id", setdiff(names(result_df), c("internal_id")))]

        # Add flag for multiple responses (1 if multiple rows, 0 otherwise)
        result_df$multiple_response_flag <- ifelse(nrow(result_df) > 1, 1, 0)

        # Add to result list
        result_list[[length(result_list) + 1]] <- result_df

        # Update tracking variables - only mark IDs as processed if we successfully got results
        coded_ids <- c(coded_ids, data_sample[[id_col_name]])
        to_code_max_id <- to_code_max_id - nrow(data_sample)
      } else {
        # No results from API, skip to next batch
        # Don't mark IDs as processed since this batch failed
        to_code_max_id <- to_code_max_id - nrow(data_sample)
      }
  }

  return(result_list)
}
