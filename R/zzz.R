#' rapidcodeR: Optimized Data Analysis System for AI-Based Text Processing
#'
#' A high-performance R package for extracting machine-readable variables from
#' natural language text using AI APIs. The package is optimized for speed and
#' cost efficiency through parallel processing and direct CSV-formatted responses
#' from language models.
#'
#' @section Key Features:
#' \itemize{
#'   \item \strong{Parallel Processing}: Distributes work across multiple CPU cores for maximum speed
#'   \item \strong{Cost Optimization}: Uses batch processing to minimize API calls
#'   \item \strong{Multi-Provider Support}: Works with both OpenAI GPT and GROQ AI services
#'   \item \strong{Robust Error Handling}: Automatic retry mechanisms for failed extractions
#'   \item \strong{CSV-Optimized Output}: AI models return structured data for efficient parsing
#'   \item \strong{Missing Data Recovery}: Secondary processing stage for failed extractions
#' }
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{parallel_execute}}}{Main function for parallel text processing}
#'   \item{\code{\link{set_api_specs}}}{Configure API specifications for AI services}
#'   \item{\code{\link{set_coding_instruction}}}{Define how AI should extract variables}
#'   \item{\code{\link{set_parameters}}}{Configure processing parameters}
#'   \item{\code{\link{calculate_overlap}}}{Assess reliability across multiple runs}
#' }
#'
#' @section Workflow:
#' \enumerate{
#'   \item Set up API specifications using \code{set_api_specs()}
#'   \item Configure processing parameters with \code{set_parameters()} (optional)
#'   \item Define extraction instructions with \code{set_coding_instruction()}
#'   \item Process data with \code{parallel_execute()}
#'   \item Optionally assess reliability with \code{calculate_overlap()}
#' }
#'
#' @section Data Format:
#' Input data should contain columns specified by \code{set_parameters()}:
#' \itemize{
#'   \item ID column: Unique identifier for each text (default column 1)
#'   \item Text column: The text content to be processed (default column 2)
#' }
#'
#' Output contains configurable columns: id, language, text, and N extracted variables (Var1-VarN).
#'
#' @examples
#' \dontrun{
#' # Basic workflow
#' library(rapidcodeR)
#'
#' # 1. Set up API specifications
#' set_api_specs(provider = "OpenAI", model = "gpt-4", temp = 0.7, api_key = "your_openai_key")
#'
#' # 2. Configure processing parameters (optional, default is 9 variables, ID in column 1, text in column 2)
#' set_parameters(n_variables = 4, id_column = 1, text_column = 2, sep = ",")  # Extract 4 variables, ID in column 1, text in column 2, comma separator
#'
#' # 3. Define what to extract
#' instruction <- paste(
#'   "Extract sentiment and topic from each post.",
#'   "Return as CSV: id,language,text,sentiment,confidence,topic,relevance"
#' )
#' set_coding_instruction(instruction)
#'
#' # 4. Process your data
#' results <- parallel_execute(
#'   test_data = my_text_data,
#'   slicing_n = 240,     # Process 240 random posts
#'   cores = 4,           # Use 4 CPU cores
#'   # Provider determined by set_api_specs()
#' )
#'
#' # 5. Check reliability (optional)
#' run1 <- parallel_execute(my_data, 100, cores = 4, seed = 123)
#' run2 <- parallel_execute(my_data, 100, cores = 4, seed = 123)
#' agreement <- calculate_overlap(list(run1, run2))
#' }
#'
#' @keywords internal
"_PACKAGE"

# Package imports are handled by DESCRIPTION Imports field
# These library calls are only needed during development

.package_env <- new.env(parent = emptyenv())

# Package imports are handled by DESCRIPTION Imports field


#' Set API Specifications for AI Models
#'
#' This function sets the API specifications including provider, model, temperature,
#' and API key for AI model interactions. It replaces the need for separate
#' set_api_keys calls and allows for more flexible configuration.
#'
#' @param provider Character. The AI provider to use, either "OpenAI" or "Groq".
#' @param model Character. The specific model to use (e.g., "gpt-4", "gpt-3.5-turbo", "llama-3.3-70b-versatile").
#' @param temp Numeric. Temperature parameter controlling randomness (0-1). Lower values make output more deterministic.
#' @param api_key Character. The API key for the specified provider.
#' @param multi_response Logical. If TRUE, allows AI to return multiple rows per input (only works with n_post=1).
#'   If FALSE (default), AI must return exactly one row per input.
#'
#' @details
#' This function stores the API specifications in the package's internal environment
#' and sets the appropriate environment variables for API authentication. The
#' specifications are automatically retrieved by processing functions.
#'
#' @return Character. A confirmation message.
#'
#' @examples
#' \dontrun{
#' # Set OpenAI specifications
#' set_api_specs(
#'   provider = "OpenAI",
#'   model = "gpt-4",
#'   temp = 0.7,
#'   api_key = "your_openai_key"
#' )
#'
#' # Set GROQ specifications
#' set_api_specs(
#'   provider = "Groq",
#'   model = "llama-3.3-70b-versatile",
#'   temp = 0.3,
#'   api_key = "your_groq_key"
#' )
#' }
#'
#' @seealso [set_coding_instruction()], [set_parameters()]
#' @export
set_api_specs <- function(provider, model, temp, api_key, multi_response = FALSE) {

  # Input validation
  if (!is.character(provider) || length(provider) != 1) {
    stop("provider must be a single character string")
  }

  if (!provider %in% c("OpenAI", "Groq")) {
    stop("provider must be either 'OpenAI' or 'Groq'")
  }

  if (!is.character(model) || length(model) != 1) {
    stop("model must be a single character string")
  }

  if (!is.numeric(temp) || length(temp) != 1 || temp < 0 || temp > 1) {
    stop("temp must be a single numeric value between 0 and 1")
  }

  if (!is.character(api_key) || length(api_key) != 1) {
    stop("api_key must be a single character string")
  }

  if (!is.logical(multi_response) || length(multi_response) != 1) {
    stop("multi_response must be a single logical value (TRUE or FALSE)")
  }

  # Set environment variables based on provider
  if (provider == "OpenAI") {
    Sys.setenv(OPENAI_API_KEY = api_key)
  } else if (provider == "Groq") {
    assign("GROQ_API_KEY", api_key, envir = .package_env)
  }

  # Store specifications in package environment
  assign("api_provider", provider, envir = .package_env)
  assign("api_model", model, envir = .package_env)
  assign("api_temp", temp, envir = .package_env)
  assign("multi_response", multi_response, envir = .package_env)

  out <- paste("API specs set: provider =", provider,
               ", model =", model,
               ", temperature =", temp,
               ", multi_response =", multi_response,
               ", API key set")

  return(out)
}

#' Set Coding Instructions for AI Models
#'
#' This function sets the coding instructions that will be used by AI models
#' to extract machine-readable variables from natural language text. The instruction
#' defines how the AI should format and structure its responses.
#'
#' @param instruction Character. A string containing the coding instruction that
#'   tells the AI model how to process and format the text data. Should specify
#'   the expected output format (typically CSV-structured responses).
#'
#' @details
#' The coding instruction is stored in the package's internal environment and
#' is automatically retrieved by processing functions. This instruction typically
#' defines:
#' \itemize{
#'   \item Expected output format (e.g., CSV structure)
#'   \item Variable definitions and coding schemes
#'   \item Response formatting requirements
#' }
#'
#' @return Character. A confirmation message indicating the instruction has been set.
#'
#' @examples
#' \dontrun{
#' instruction <- "Extract sentiment and topics from posts. Return as CSV: id,sentiment,topic"
#' set_coding_instruction(instruction)
#' }
#'
#' @export
set_coding_instruction <- function(instruction) {
  assign("coding_instruction", instruction, envir = .package_env)

  out <- "Coding instruction has been set!"

  return(out)
}

#' Set Processing Parameters for AI Models
#'
#' This function configures the parameters that will be used by AI models
#' to extract machine-readable variables from natural language text.
#'
#' @param n_variables Integer. Number of columns to extract (default is 9).
#'   This determines the total number of columns in the final dataset.
#'   Must be between 1 and 20.
#' @param id_column Integer. The column number containing the unique ID for each
#'   observation (default is 1). Must be a positive integer.
#' @param text_column Integer. The column number containing the text content
#'   to be processed (default is 2). Must be a positive integer.
#' @param sep Character. Separator used in API response parsing (default is ";").
#'   Must be one of: ";" (semicolon), "," (comma), or "|" (vertical bar).
#'
#' @details
#' The function sets the expected parameters in the package's internal
#' environment. This affects:
#' \itemize{
#'   \item Response parsing and validation
#'   \item Column naming (Var1, Var2, ..., VarN)
#'   \item Data structure expectations across all processing functions
#'   \item Column identification for text content and unique IDs
#'   \item Separator used for parsing API responses
#' }
#'
#' The package automatically adds a unique internal ID column to each row during processing.
#' This internal ID is used for tracking and is removed from the final output.
#'
#' The total expected columns will be n_variables:
#' \itemize{
#'   \item All columns: Var1, Var2, ..., VarN
#' }
#'
#' @return Character. A confirmation message indicating the parameters have been set.
#'
#' @examples
#' \dontrun{
#' # Extract 4 columns, ID in column 1, text in column 2, semicolon separator
#' set_parameters(n_variables = 4, id_column = 1, text_column = 2, sep = ";")
#'
#' # Extract 10 columns, ID in column 3, text in column 5, comma separator
#' set_parameters(n_variables = 10, id_column = 3, text_column = 5, sep = ",")
#'
#' # Extract 6 columns, ID in column 2, text in column 3, vertical bar separator
#' set_parameters(n_variables = 6, id_column = 2, text_column = 3, sep = "|")
#' }
#'
#' @export
set_parameters <- function(n_variables = 9, id_column = 1, text_column = 2, sep = ";") {
  # Input validation
  if (!is.numeric(n_variables) || n_variables < 1 || n_variables > 20) {
    stop("n_variables must be a positive number between 1 and 20")
  }

  if (!is.numeric(id_column) || id_column <= 0) {
    stop("id_column must be a positive number")
  }

  if (!is.numeric(text_column) || text_column <= 0) {
    stop("text_column must be a positive number")
  }

  # Validate separator
  valid_separators <- c(";", ",", "|")
  if (!sep %in% valid_separators) {
    stop("sep must be one of: ';' (semicolon), ',' (comma), or '|' (vertical bar)")
  }

  # Store parameters in package environment
  assign("n_variables", as.integer(n_variables), envir = .package_env)
  assign("id_column", as.integer(id_column), envir = .package_env)
  assign("text_column", as.integer(text_column), envir = .package_env)
  assign("sep", sep, envir = .package_env)

  out <- paste("Parameters set: n_variables =", n_variables,
               ", id_column =", id_column,
               ", text_column =", text_column,
               ", sep = '", sep, "'",
               " (internal ID column will be added automatically)")

  return(out)
}






