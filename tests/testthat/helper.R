# Test helper functions
# This file is automatically sourced by testthat before running tests

# Function to get API key from various sources
get_test_api_key <- function() {
  # Try different sources in order of preference
  sources <- c(
    Sys.getenv("OPENAI_API_KEY"),
    Sys.getenv("RAPIDCODER_TEST_API_KEY"),
    # Add other sources as needed
    ""
  )
  
  # Return first non-empty key
  for (key in sources) {
    if (nchar(key) > 0) {
      return(key)
    }
  }
  
  return("")
}

# Test if API key is available
is_api_key_available <- function() {
  nchar(get_test_api_key()) > 0
}

# Get API key for tests
get_api_key <- function() {
  key <- get_test_api_key()
  if (nchar(key) == 0) {
    stop("No API key found. Set OPENAI_API_KEY or RAPIDCODER_TEST_API_KEY environment variable.")
  }
  return(key)
}
