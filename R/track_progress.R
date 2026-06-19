
#' Track Progress of Parallel Processing Tasks
#'
#' This function wraps the main processing function with progress tracking,
#' error handling, and rate limiting. It's designed to be called from parallel
#' workers to process data batches while providing feedback on progress.
#'
#' @param df Data frame. A subset of data to process in this task.
#' @param index Integer. The current task number (for progress tracking).
#' @param total_tasks Integer. Total number of tasks in the parallel job.
#' @param n_post Integer. Number of posts to process in each API call.
#' @param provider Character. AI provider to use, either "OpenAI" or "Groq".
#' @param worker_env Environment or NULL. When running in a parallel worker, the worker's
#'   package environment so config (e.g. \code{coding_instruction}, \code{id_column})
#'   is available. If NULL (default), the package's \code{.package_env} is used.
#' @param verbose Logical. If TRUE (default), progress messages are shown. If FALSE, suppressed.
#'
#' @details
#' The function provides several important features:
#' \itemize{
#'   \item Progress messages showing current task vs total tasks
#'   \item Rate limiting with 5-second delays to avoid overwhelming APIs
#'   \item Comprehensive error handling with informative error messages
#'   \item Automatic validation of provider parameter
#'   \item Graceful degradation returning NULL on errors
#' }
#'
#' This function is primarily used internally by the parallel processing
#' framework and calls `main_func()` with the `n_post` parameter passed by the caller.
#'
#' @return Data frame with successfully processed results from `main_func()`,
#'   or NULL if an error occurs during processing.
#'
#' @seealso [main_func()], [parallel_execute()]
#' @keywords internal
track_progress <- function(df, index, total_tasks, n_post, batch_size, provider, worker_env = NULL, verbose = TRUE) {

  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  if (!is.numeric(index) || index <= 0) {
    stop("index must be a positive number")
  }

  if (!is.numeric(total_tasks) || total_tasks <= 0) {
    stop("total_tasks must be a positive number")
  }

  if (!is.numeric(batch_size) || batch_size <= 0) {
    stop("batch_size must be a positive number")
  }

  if (!provider %in% c("OpenAI", "Groq")) {
    stop("provider must be either 'OpenAI' or 'Groq'")
  }

  tryCatch({
    if (verbose) message("Processing task ", index, " out of ", total_tasks)
    Sys.sleep(5)  # 5-second delay to avoid hitting the API too hard
    if(provider %in% c("OpenAI", "Groq")) {
      to_code_max_id <- nrow(df)
      result <- main_func(df, to_code_max_id, n_post, provider, worker_env = worker_env)
    }
    else {
      stop("Please specify whether provider is 'OpenAI' or 'Groq'")
    }
    if (verbose) message("Finished task ", index)
    return(result)
  }, error = function(e) {
    message("Error processing task ", index, ": ", e$message)
    return(NULL)  # Return NULL or handle the error appropriately
  })
}
