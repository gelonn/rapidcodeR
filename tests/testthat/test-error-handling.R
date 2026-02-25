test_that("Error handling tests for rapidcodeR", {

  # Test 1: Invalid input data
  cat("\nTesting invalid input data...\n")

  # Test with non-dataframe input
  expect_error(
    parallel_execute(test_data = "not_a_dataframe", slicing_n = 10, cores = 2),
    "test_data must be a data frame"
  )

  # Set up basic parameters for error testing
  set_parameters(n_variables = 4, id_column = 1, text_column = 2, sep = ";")
  set_coding_instruction("Extract: sentiment,confidence,topic,relevance")

  # Test with empty dataframe
  empty_df <- data.frame()
  expect_error(
    parallel_execute(test_data = empty_df, slicing_n = 10, cores = 2),
    "id_column.*is greater than the number of columns"
  )

  # Test with missing required columns
  invalid_df <- data.frame(wrong_col = 1:10)
  expect_error(
    parallel_execute(test_data = invalid_df, slicing_n = 10, cores = 2),
    "text_column.*is greater than the number of columns"
  )

  # Test with NA values in text column
  df_with_na <- data.frame(
    post_id = 1:7,
    full_post = c("Valid text", NA, "Another valid text", "", "More text", "This is a longer valid text", "Another valid entry")
  )

  # This should work (NA values are filtered out)
  set_api_specs(provider = "OpenAI", model = "gpt-3.5-turbo", temp = 0.3,
                api_key = Sys.getenv("OPENAI_API_KEY"))

  # Skip this test if no API key
  if (nchar(Sys.getenv("OPENAI_API_KEY")) > 0) {
    result_with_na <- parallel_execute(
      test_data = df_with_na,
      slicing_n = 5,
      cores = 1
    )
    expect_true(is.data.frame(result_with_na), "Should handle NA values gracefully")
  }

  # Test 2: Invalid parameters
  cat("\nTesting invalid parameters...\n")

  # Set up API specs for parameter testing with real key
  set_api_specs(provider = "OpenAI", model = "gpt-3.5-turbo", temp = 0.3,
                api_key = Sys.getenv("OPENAI_API_KEY"))

  valid_df <- data.frame(
    post_id = 1:10,
    full_post = paste("Test post", 1:10)
  )

  # Test with negative slicing_n
  expect_error(
    parallel_execute(test_data = valid_df, slicing_n = -5, cores = 2),
    "slicing_n must be a positive number"
  )

  # Test with slicing_n greater than data rows
  expect_error(
    parallel_execute(test_data = valid_df, slicing_n = 100, cores = 2),
    "Make sure that slicing_n is not greater than the number of rows"
  )

  # Test with zero cores
  expect_error(
    parallel_execute(test_data = valid_df, slicing_n = 10, cores = 0),
    "cores must be a positive number"
  )

  # Test with negative cores
  expect_error(
    parallel_execute(test_data = valid_df, slicing_n = 10, cores = -2),
    "cores must be a positive number"
  )

  # Test 3: Missing API configuration
  cat("\nTesting missing API configuration...\n")

  # Clear any existing API configuration
  if (exists("api_provider", envir = .package_env)) {
    rm("api_provider", envir = .package_env)
  }

  expect_error(
    parallel_execute(test_data = valid_df, slicing_n = 10, cores = 2),
    "API provider not set"
  )

  # Test 4: Invalid API configuration
  cat("\nTesting invalid API configuration...\n")

  # Test invalid provider in set_api_specs
  expect_error(
    set_api_specs(provider = "InvalidProvider", model = "gpt-3.5-turbo", temp = 0.3,
                  api_key = Sys.getenv("OPENAI_API_KEY")),
    "provider must be either 'OpenAI' or 'Groq'"
  )

  # Test 5: Missing coding instruction (skip on CI when no API key: API test runs first and fails)
  cat("\nTesting missing coding instruction...\n")
  testthat::skip_if(
    nchar(Sys.getenv("OPENAI_API_KEY")) == 0,
    "OpenAI API key not set; cannot test coding instruction error path"
  )

  # Set valid API config but clear coding instruction
  set_api_specs(provider = "OpenAI", model = "gpt-3.5-turbo", temp = 0.3,
                api_key = Sys.getenv("OPENAI_API_KEY"))

  if (exists("coding_instruction", envir = .package_env)) {
    rm("coding_instruction", envir = .package_env)
  }

  expect_error(
    parallel_execute(test_data = valid_df, slicing_n = 10, cores = 2),
    "Coding instruction has not been set"
  )

  # Test 6: Invalid set_parameters calls
  cat("\nTesting invalid set_parameters calls...\n")

  # Test with invalid n_variables
  expect_error(
    set_parameters(n_variables = -1),
    "n_variables must be a positive number between 1 and 20"
  )

  expect_error(
    set_parameters(n_variables = 25),
    "n_variables must be a positive number between 1 and 20"
  )

  # Test with invalid id_column
  expect_error(
    set_parameters(id_column = 0),
    "id_column must be a positive number"
  )

  expect_error(
    set_parameters(id_column = -2),
    "id_column must be a positive number"
  )

  # Test with invalid text_column
  expect_error(
    set_parameters(text_column = 0),
    "text_column must be a positive number"
  )

  # Test with invalid separator
  expect_error(
    set_parameters(sep = "invalid"),
    "sep must be one of:"
  )

  # Test 7: Invalid set_api_specs calls
  cat("\nTesting invalid set_api_specs calls...\n")

  # Test with invalid provider
  expect_error(
    set_api_specs(provider = "Invalid", model = "gpt-3.5-turbo", temp = 0.3, api_key = "key"),
    "provider must be either 'OpenAI' or 'Groq'"
  )

  # Test with invalid temperature
  expect_error(
    set_api_specs(provider = "OpenAI", model = "gpt-3.5-turbo", temp = 1.5, api_key = "key"),
    "temp must be a single numeric value between 0 and 1"
  )

  expect_error(
    set_api_specs(provider = "OpenAI", model = "gpt-3.5-turbo", temp = -0.1, api_key = "key"),
    "temp must be a single numeric value between 0 and 1"
  )

  # Test 8: Invalid seed parameter
  cat("\nTesting invalid seed parameter...\n")

  set_api_specs(provider = "OpenAI", model = "gpt-3.5-turbo", temp = 0.3, api_key = Sys.getenv("OPENAI_API_KEY"))
  set_parameters(n_variables = 4, id_column = 1, text_column = 2, sep = ";")
  set_coding_instruction("Extract: sentiment,confidence,topic,relevance")

  # Test with non-integer seed
  expect_error(
    parallel_execute(test_data = valid_df, slicing_n = 10, cores = 2, seed = 1.5),
    "seed must be NULL or a single integer value"
  )

  # Test with vector seed
  expect_error(
    parallel_execute(test_data = valid_df, slicing_n = 10, cores = 2, seed = c(1, 2)),
    "seed must be NULL or a single integer value"
  )

  # Test 9: Text column validation
  cat("\nTesting text column validation...\n")

  # Test with non-character text column
  df_numeric_text <- data.frame(
    post_id = 1:5,
    full_post = 1:5  # Numeric instead of character
  )

  expect_error(
    parallel_execute(test_data = df_numeric_text, slicing_n = 5, cores = 2),
    "Text column.*must contain character or factor data"
  )

  # Test with all NA text column
  df_all_na_text <- data.frame(
    post_id = 1:5,
    full_post = rep(NA_character_, 5)
  )

  expect_error(
    parallel_execute(test_data = df_all_na_text, slicing_n = 5, cores = 2),
    "Text column.*contains only NA values"
  )

  # Test 10: Core count warnings
  cat("\nTesting core count warnings...\n")

  # Ensure parameters are set before parallel_execute
  set_parameters(n_variables = 4, id_column = 1, text_column = 2, sep = ";")
  set_coding_instruction("Extract: sentiment,confidence,topic,relevance")
  set_api_specs(provider = "OpenAI", model = "gpt-3.5-turbo", temp = 0.3,
                api_key = Sys.getenv("OPENAI_API_KEY"))

  # This should produce a warning but not an error
  expect_warning(
    parallel_execute(test_data = valid_df, slicing_n = 5, cores = 20),
    "Requested cores.*exceeds available cores"
  )

  # API key is already set to real key throughout the test


  cat("\nError handling tests completed successfully!\n")
})
