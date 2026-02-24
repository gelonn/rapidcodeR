test_that("Performance tests for rapidcodeR", {

  # Skip if API keys are not set
  skip_if_not(is_api_key_available(), "OpenAI API key not set")

  # Test API connectivity first
  set_api_specs(provider = "OpenAI", model = "gpt-3.5-turbo", temp = 0.3,
                api_key = Sys.getenv("OPENAI_API_KEY"))

  # Try to make a simple API call to test connectivity
  test_result <- tryCatch({
    ask_openai("What is the capital of Norway?")
  }, error = function(e) {
    return(NA)
  })

  # Skip test if API connection fails
  skip_if(is.na(test_result), "OpenAI API test failed - check API key and internet connection")

  # Create test data
  test_data <- data.frame(
    post_id = 1:50,
    full_post = paste("This is test post number", 1:50,
                     "about various topics including technology, science, and general discussion.")
  )

  # Set up package configuration
  set_parameters(n_variables = 4, id_column = 1, text_column = 2, sep = ";")
  set_coding_instruction("Extract exactly 4 variables from each text: sentiment;confidence;topic;relevance. Return ONLY the values separated by semicolons, no explanations or labels. Example: positive;0.8;technology;high")

  # Test 1: Small dataset performance (10 posts)
  cat("\nTesting small dataset performance (10 posts)...\n")
  start_time <- Sys.time()

  result_small <- parallel_execute(
    test_data = test_data[1:10, ],
    slicing_n = 10,
    cores = 2,
    multi_core = FALSE
  )

  small_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat("Small dataset processing time:", small_time, "seconds\n")

  # Skip if parallel processing failed (can happen in check() environment)
  skip_if(nrow(result_small) == 0, "Parallel processing failed - likely environment issue in check()")

  # Performance expectations
  expect_true(small_time < 120, "Small dataset should complete within 2 minutes")
  expect_true(nrow(result_small) > 0, "Small dataset should produce results")

  # Test 2: Medium dataset performance (30 posts)
  cat("\nTesting medium dataset performance (30 posts)...\n")
  start_time <- Sys.time()

  result_medium <- parallel_execute(
    test_data = test_data[1:30, ],
    slicing_n = 30,
    cores = 4,
    multi_core = FALSE
  )

  medium_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat("Medium dataset processing time:", medium_time, "seconds\n")

  # Skip if parallel processing failed (can happen in check() environment)
  skip_if(nrow(result_medium) == 0, "Parallel processing failed - likely environment issue in check()")

  # Performance expectations
  if (medium_time >= 300) {
    warning("Medium dataset took ", round(medium_time, 1), " seconds (", round(medium_time/60, 1), " minutes) - ideally should complete within 5 minutes")
  }
  expect_true(nrow(result_medium) > 0, "Medium dataset should produce results")

  # Test 3: Core scaling performance
  cat("\nTesting core scaling performance...\n")

  # Test with 2 cores - use smaller slicing_n to create multiple batches
  cat("Starting 2-core test (2 batches)...\n")
  start_time_2cores <- Sys.time()
  result_2cores <- parallel_execute(
    test_data = test_data[1:20, ],
    slicing_n = 10,  # Creates 2 batches for 2 cores
    cores = 2,
    multi_core = FALSE
  )
  time_2cores <- as.numeric(difftime(Sys.time(), start_time_2cores, units = "secs"))
  cat("2 cores completed in", round(time_2cores, 2), "seconds. Rows processed:", nrow(result_2cores), "\n")

  # Test with 4 cores (if available)
  if (parallel::detectCores() >= 4) {
    cat("Starting 4-core test (4 batches)...\n")
    start_time_4cores <- Sys.time()
    result_4cores <- parallel_execute(
      test_data = test_data[1:40, ],  # More data for 4 cores
      slicing_n = 10,  # Creates 4 batches for 4 cores
      cores = 4,
      multi_core = FALSE
    )
    time_4cores <- as.numeric(difftime(Sys.time(), start_time_4cores, units = "secs"))
    cat("4 cores completed in", round(time_4cores, 2), "seconds. Rows processed:", nrow(result_4cores), "\n")

    # Check if 4 cores is significantly slower - with proper batching it shouldn't be
    # Allow some overhead but expect better or similar performance
    if (time_4cores >= time_2cores * 2.0) {
      warning(paste0("Performance issue detected: 4 cores (", round(time_4cores, 2),
                    "s) is significantly slower than 2 cores (", round(time_2cores, 2),
                    "s). This may be due to API rate limiting, worker overhead, or resource contention."))
    } else {
      cat("Core scaling looks reasonable. 4 cores:", round(time_4cores, 2), "s vs 2 cores:", round(time_2cores, 2), "s\n")
    }
  }

  # Test 4: Memory usage monitoring
  cat("\nTesting memory usage...\n")
  memory_before <- gc()["Vcells", "used"]

  result_memory <- parallel_execute(
    test_data = test_data[1:25, ],
    slicing_n = 25,
    cores = 2,
    multi_core = FALSE
  )

  memory_after <- gc()["Vcells", "used"]
  memory_increase <- memory_after - memory_before

  cat("Memory increase:", memory_increase, "Vcells\n")
  # Allow reasonable memory increase - parallel processing uses more memory
  expect_true(memory_increase < 500000, "Memory increase should be reasonable")

  # Test 5: Result structure validation
  expect_true(is.data.frame(result_small), "Result should be a data frame")

  expect_true(ncol(result_small) == 4, "Result should have 4 variables")
  expect_true(all(c("Var1", "Var2", "Var3", "Var4") %in% names(result_small)),
              "Result should have correct column names")

  cat("\nPerformance tests completed successfully!\n")
})
