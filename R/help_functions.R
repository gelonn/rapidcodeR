# Package imports are handled by DESCRIPTION Imports field

#' Send Prompt to GROQ AI API
#'
#' This function sends a text prompt to the GROQ AI API and returns the model's
#' response. It includes error handling and uses the groqR package for API communication.
#'
#' @param prompt Character. The text prompt to send to the AI model.
#' @param temp Numeric. Temperature parameter controlling randomness (0-1).
#'   Lower values make output more deterministic. Required (typically from set_api_specs()).
#' @param topp Numeric. Top-p parameter controlling diversity (0-1). Default is 1.
#' @param model Character. GROQ model to use. Required (typically from set_api_specs()).
#' @param api_key Character. GROQ API key. Must be provided.

#'
#' @details
#' The function uses the groqR package to communicate with GROQ's API. It includes
#' error handling that returns NA if the API call fails. The function is optimized
#' for text processing tasks with a maximum token limit of 5000.
#'
#' @return Character. The AI model's response to the prompt, or NA if an error occurs.
#'
#' @examples
#' \dontrun{
#' # Set API key first
#' Sys.setenv(GROQ_API_KEY = "your_api_key")
#'
#' # Send a simple prompt
#' response <- ask_groq("What is the capital of France?")
#'
#' # Use different parameters
#' response <- ask_groq("Analyze this text", temp = 0.3, topp = 0.9)
#' }
#'
#' @seealso [ask_openai()]
#' @export
ask_groq <- function(prompt, temp, topp = 1, model, api_key) {

  # Input validation
  if (!is.character(prompt) || length(prompt) != 1) {
    stop("prompt must be a single character string")
  }

  # Get model and temp from set_api_specs if not provided
  if (is.null(model)) {
    if (exists("api_model", envir = globalenv())) {
      model <- get("api_model", envir = globalenv())
    } else if (exists("api_model", envir = .package_env)) {
      model <- get("api_model", envir = .package_env)
    } else {
      stop("Model not set. Please use set_api_specs() to configure your API settings.")
    }
  }

  if (is.null(temp)) {
    if (exists("api_temp", envir = globalenv())) {
      temp <- get("api_temp", envir = globalenv())
    } else if (exists("api_temp", envir = .package_env)) {
      temp <- get("api_temp", envir = .package_env)
    } else {
      stop("Temperature not set. Please use set_api_specs() to configure your API settings.")
    }
  }

  if (!is.numeric(temp) || temp < 0 || temp > 1) {
    stop("temp must be a number between 0 and 1")
  }

  if (!is.numeric(topp) || topp < 0 || topp > 1) {
    stop("topp must be a number between 0 and 1")
  }

  if (!is.character(model) || length(model) != 1) {
    stop("model must be a single character string")
  }

  if (identical(api_key, "")) {
    stop("Environment variable 'GROQ_API_KEY' is not set. Please set it with Sys.setenv() or via your .Renviron file.")
  }

  result <- tryCatch({
    # Ensure groqR package is loaded
    if (!requireNamespace("groqR", quietly = TRUE)) {
      stop("groqR package is required but not installed")
    }

    # Set the API key as an environment variable for groqR
    Sys.setenv(GROQ_API_KEY = api_key)

    groqR::ask(
      prompt,
      systemRole   = "Your assistant",
      model        = model,
      maxTokens    = getOption("rapidcodeR.max_tokens", 5000),
      temperature  = temp,
      top_p        = topp,
      proxy        = NULL
    )
  }, error = function(e) {
    warning("Got API error: ", e$message)
    NA
  })

  return(result)
}

#' Send Prompt to OpenAI API
#'
#' This function sends a text prompt to the OpenAI API and returns the model's
#' response. It includes robust error handling and uses the openai package for API communication.
#'
#' @param prompt Character. The text prompt to send to the AI model.
#' @param temp Numeric. Temperature parameter controlling randomness (0-1).
#'   Lower values make output more deterministic. If NULL, retrieved from set_api_specs().
#' @param topp Numeric. Top-p parameter controlling diversity (0-1). Default is 1.
#' @param model Character. OpenAI model to use. If NULL, retrieved from set_api_specs().
#'
#' @details
#' The function uses the openai package to communicate with OpenAI's API. It includes
#' comprehensive error handling that returns NA if the API call fails or returns
#' an unexpected format. The function expects the OPENAI_API_KEY environment variable
#' to be set via `set_api_specs()`.
#'
#' @return Character. The AI model's response to the prompt, or NA if an error occurs.
#'
#' @examples
#' \dontrun{
#' # Set API specifications first
#' set_api_specs(provider = "OpenAI", model = "gpt-4", temp = 0.7, api_key = "your_api_key")
#'
#' # Send a simple prompt
#' response <- ask_openai("What is the capital of France?")
#'
#' # Use different parameters
#' response <- ask_openai("Analyze this text", temp = 0.3, topp = 0.9)
#' }
#'
#' @seealso [ask_groq()], [set_api_specs()]
#' @export
ask_openai <- function(prompt, temp = NULL, topp = 1, model = NULL) {

  # Input validation
  if (!is.character(prompt) || length(prompt) != 1) {
    stop("prompt must be a single character string")
  }

  # Get model and temp from set_api_specs if not provided
  if (is.null(model)) {
    if (exists("api_model", envir = globalenv())) {
      model <- get("api_model", envir = globalenv())
    } else if (exists("api_model", envir = .package_env)) {
      model <- get("api_model", envir = .package_env)
    } else {
      stop("Model not set. Please use set_api_specs() to configure your API settings.")
    }
  }

  if (is.null(temp)) {
    if (exists("api_temp", envir = globalenv())) {
      temp <- get("api_temp", envir = globalenv())
    } else if (exists("api_temp", envir = .package_env)) {
      temp <- get("api_temp", envir = .package_env)
    } else {
      stop("Temperature not set. Please use set_api_specs() to configure your API settings.")
    }
  }

  if (!is.numeric(temp) || temp < 0 || temp > 1) {
    stop("temp must be a number between 0 and 1")
  }

  if (!is.numeric(topp) || topp < 0 || topp > 1) {
    stop("topp must be a number between 0 and 1")
  }

  if (!is.character(model) || length(model) != 1) {
    stop("model must be a single character string")
  }

  result <- tryCatch({
    # Ensure openai package is loaded
    if (!requireNamespace("openai", quietly = TRUE)) {
      stop("openai package is required but not installed")
    }

    response <- openai::create_chat_completion(
      model = model,
      messages = list(
        list(role = "system", content = "You are a helpful assistant."),
        list(role = "user", content = prompt)
      ),
      temperature = temp,
      top_p = topp
    )


    # Handle response$choices as either a data.frame or list
    if (!is.null(response$choices)) {
      if (is.data.frame(response$choices)) {
        # If it's a data.frame, get the first row's message content
        if (nrow(response$choices) > 0) {
          # Check for message.content column (new format)
          if ("message.content" %in% names(response$choices)) {
            content <- response$choices[1, "message.content"]
            if (is.character(content) && !is.na(content)) {
              return(content)
            }
          }
          # Check for nested message structure (old format)
          else if ("message" %in% names(response$choices)) {
            first_choice <- response$choices[1, "message"]
            if (is.list(first_choice) && "content" %in% names(first_choice)) {
              return(first_choice$content)
            } else if (is.character(first_choice)) {
              return(first_choice)
            }
          }
        }
      } else if (is.list(response$choices) && length(response$choices) > 0) {
        # If it's a list, get the first element's message content
        first_choice <- response$choices[[1]]
        if (is.list(first_choice) && "message" %in% names(first_choice)) {
          message_obj <- first_choice$message
          if (is.list(message_obj) && "content" %in% names(message_obj)) {
            return(message_obj$content)
          }
        }
      }
    }

    message("Unexpected API response format.")
    return(NA)

  }, error = function(e) {
    message("API error: ", e$message)
    return(NA)
  })

  return(result)
}

#' Parse AI Response into Formatted Row String
#'
#' This function takes a single AI response string and formats it into a standardized
#' SQL-style row format for consistent data processing. It handles text sanitization
#' and ensures the response has exactly N elements (N set by set_parameters()).
#'
#' @param ai_response Character. A single response string from an AI model, typically
#'   containing semicolon-separated values.
#'
#' @details
#' The function performs several formatting operations:
#' \itemize{
#'   \item Converts input to character format
#'   \item Replaces quotes with backticks to avoid SQL conflicts
#'   \item Splits on semicolons to extract individual values
#'   \item Trims whitespace from each value
#'   \item Pads with NA values if fewer than expected elements
#'   \item Validates that we have the expected number of elements
#'   \item Converts to lowercase and formats as SQL-style row
#' }
#'
#' The function expects exactly N values as set by set_parameters().
#'
#' @return Character. A formatted row string in SQL format like "('val1','val2',...)",
#'   or NA if validation fails.
#'
#' @examples
#' \dontrun{
#' # AI response with semicolon-separated values (assuming set_parameters(n_variables = 6))
#' ai_response <- "123;en;Hello world;positive;high;topic1"
#' formatted_row <- make_value_row(ai_response)
#' # Returns: "('123','en','hello world','positive','high','topic1')"
#' }
#'
#' @seealso [main_func()]
#' @export
make_value_row <- function(ai_response) {
  # Input validation
  if (!is.character(ai_response) || length(ai_response) != 1) {
    stop("ai_response must be a single character string")
  }

  if (is.na(ai_response) || ai_response == "") {
    return(NA)
  }

  # No longer need to enforce a specific number of columns
  # The function will handle any number of columns dynamically

  separator <- if (exists("sep", envir = globalenv())) {
    get("sep", envir = globalenv())
  } else {
    ";" # Default fallback
  }

  # Ensure input is a single string
  ai_response <- as.character(ai_response)

  # Replace single/double quotes with backticks
  ai_response <- gsub("['\"]", "`", ai_response)

  # Split into values based on semicolons
  insert_values <- unlist(strsplit(ai_response, split = separator, fixed = TRUE))

  # Trim whitespace from each entry
  insert_values <- trimws(insert_values)

  # Get expected number of variables from package environment
  n_variables <- if (exists("n_variables", envir = globalenv())) {
    get("n_variables", envir = globalenv())
  } else {
    9  # Default fallback (matches set_parameters default)
  }

  # Ensure we have the expected number of columns
  while(length(insert_values) < n_variables) {
    insert_values = c(insert_values, NA)
  }

  # Ensure we have the expected number of elements
  if (length(insert_values) == n_variables) {
    # Format each element safely (escape single quotes for SQL-style output)
    insert_values <- gsub("'", "''", insert_values)
    insert_values <- tolower(insert_values)  # Convert to lowercase (optional)

    # Build SQL-style row string
    insert_row <- paste0("('", paste(insert_values, collapse = "','"), "')")
  } else {
    warning(paste("make_value_row: Expected", n_variables, "values, got", length(insert_values),
                  "values:", paste(insert_values, collapse = ", ")))
    insert_row <- NA
  }

  return(insert_row)
}
