test_that("Flexibility tests for rapidcodeR", {

  # Skip if API keys are not set
  skip_if_not(is_api_key_available(), "OpenAI API key not set")

  # Test API connectivity first
  set_api_specs(provider = "OpenAI", model = "gpt-3.5-turbo", temp = 0.3,
                api_key = Sys.getenv("OPENAI_API_KEY"))

  test_result <- tryCatch({
    ask_openai("What is the capital of Norway?")
  }, error = function(e) {
    return(NA)
  })

  # Skip test if API connection fails
  skip_if(is.na(test_result), "OpenAI API test failed - check API key and internet connection")

  # Create diverse test data
  test_data <- data.frame(
    post_id = 1:35,
    full_post = c(
      # Short posts
      "Great day!", "Not good.", "Excellent service.",

      # Medium posts
      "This product is amazing and I highly recommend it to everyone.",
      "I had a terrible experience with customer service yesterday.",
      "The weather is beautiful today, perfect for a walk.",

      # Long posts
      "I've been using this software for months now and I must say it has completely transformed my workflow. The interface is intuitive, the features are comprehensive, and the performance is outstanding. I would definitely recommend it to anyone looking for a reliable solution.",
      "Yesterday I went to the new restaurant downtown and was disappointed with both the food quality and service. The wait time was excessive, the staff seemed overwhelmed, and the food was cold when it arrived. I don't think I'll be returning.",
      "The conference was incredibly informative with speakers from various industries sharing their insights on technology trends, market analysis, and future predictions. I learned so much about emerging technologies and their potential applications.",

      # Multilingual content (simplified for testing)
      "Bonjour, comment allez-vous? Je suis très content.",
      "Hola, ¿cómo estás? Todo está muy bien.",
      "Guten Tag, wie geht es Ihnen? Alles ist gut.",

      # Posts with special characters
      "Check out this amazing deal: 50% off! 🎉 #sale #discount",
      "Meeting @ 3:00 PM tomorrow. Please bring your laptops & notebooks.",
      "The results are in: 95% success rate! (That's amazing!)",

      # Technical content
      "The API endpoint /api/v1/users returns a JSON response with user data including id, name, email, and timestamp fields.",
      "Error 404: Resource not found. Please check the URL and try again.",
      "Database query executed successfully. 1,247 records updated in 0.3 seconds.",

      # Emotional content
      "I'm so excited about this opportunity! Can't wait to get started.",
      "Feeling really frustrated with this situation. Nothing seems to be working.",
      "This makes me so happy! Thank you for making my day better.",

      # Question/answer format
      "What is the best way to optimize database performance?",
      "How do I configure the security settings for maximum protection?",
      "Where can I find documentation for the new API features?",

      # Mixed content
      "Product review: ⭐⭐⭐⭐⭐ Excellent quality, fast shipping, great customer service. Highly recommend!",
      "Technical issue resolved: Updated firmware to v2.1.3, rebooted system, cleared cache. All systems operational.",
      "Event update: Conference moved to Room 204B. Lunch will be served at 12:30 PM. See you there!",

      # Edge cases
      "a",  # Very short
      "12345",  # Numbers only
      "!@#$%^&*()",  # Special characters only
      "",  # Empty (will be filtered out)
      "   ",  # Whitespace only (will be filtered out)
      "This is a very long post that contains multiple sentences and should test the system's ability to handle longer text content. It includes various punctuation marks, numbers like 123 and 456, and special characters like @, #, and $. The post continues with more content to ensure it's long enough to be processed properly by the AI model.",
      "Mixed content: English text with 中文 characters and العربية script.",
      "Email: user@example.com, Phone: +1-555-123-4567, Website: https://example.com"
    )
  )

  # Test 1: Different variable counts
  cat("\nTesting different variable counts...\n")

  set_api_specs(provider = "OpenAI", model = "gpt-5", temp = 1,
                api_key = Sys.getenv("OPENAI_API_KEY"))

  # Test with 3 variables
  set_parameters(n_variables = 3, id_column = 1, text_column = 2, sep = ";")
  set_coding_instruction("Extract exactly 3 variables from each text: sentiment;confidence;topic. Return ONLY the values separated by semicolons, no explanations or labels. Example: positive;0.8;technology")

  result_3vars <- parallel_execute(
    test_data = test_data[1:10, ],
    slicing_n = 10,
    cores = 2,
    multi_core = FALSE
  )

  expect_true(ncol(result_3vars) == 3, "Should extract 3 variables")
  expect_true(all(c("Var1", "Var2", "Var3") %in% names(result_3vars)),
              "Should have correct column names for 3 variables")

  # Test with 6 variables
  set_parameters(n_variables = 6, id_column = 1, text_column = 2, sep = ";")
  set_coding_instruction("Extract exactly 6 variables from each text: sentiment;confidence;topic;emotion;intensity;relevance. Return ONLY the values separated by semicolons, no explanations or labels. Example: positive;0.8;technology;joy;high;high")

  result_6vars <- parallel_execute(
    test_data = test_data[1:10, ],
    slicing_n = 10,
    cores = 2,
    multi_core = FALSE
  )

  expect_true(ncol(result_6vars) == 6, "Should extract 6 variables")
  expect_true(all(c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6") %in% names(result_6vars)),
              "Should have correct column names for 6 variables")


  # Test 2: Different separators
  cat("\nTesting different separators...\n")

  # Test with comma separator
  set_parameters(n_variables = 4, id_column = 1, text_column = 2, sep = ",")
  set_coding_instruction("Extract: sentiment,confidence,topic,relevance")

  result_comma <- parallel_execute(
    test_data = test_data[1:8, ],
    slicing_n = 8,
    cores = 2,
    multi_core = FALSE
  )

  expect_true(is.data.frame(result_comma), "Comma separator should work")

  # Test with pipe separator
  set_parameters(n_variables = 4, id_column = 1, text_column = 2, sep = "|")
  set_coding_instruction("Extract: sentiment,confidence,topic,relevance")

  result_pipe <- parallel_execute(
    test_data = test_data[1:8, ],
    slicing_n = 8,
    cores = 2,
    multi_core = FALSE
  )

  expect_true(is.data.frame(result_pipe), "Pipe separator should work")

  # Test 3: Different core counts
  cat("\nTesting different core counts...\n")

  set_parameters(n_variables = 4, id_column = 1, text_column = 2, sep = ";")
  set_coding_instruction("Extract: sentiment,confidence,topic,relevance")

  # Test with 1 core
  result_1core <- parallel_execute(
    test_data = test_data[1:8, ],
    slicing_n = 8,
    cores = 1,
    multi_core = FALSE
  )

  expect_true(is.data.frame(result_1core), "Single core should work")

  # Test with 2 cores (if available)
  if (parallel::detectCores() >= 2) {
    result_2cores <- parallel_execute(
      test_data = test_data[1:8, ],
      slicing_n = 8,
      cores = 2,
      multi_core = FALSE
    )

    expect_true(is.data.frame(result_2cores), "Two cores should work")
  }

  # Test 4: Different text content types
  cat("\nTesting different text content types...\n")

  # Test with short posts
  short_posts <- test_data[1:5, ]
  result_short <- parallel_execute(
    test_data = short_posts,
    slicing_n = 5,
    cores = 2,
    multi_core = FALSE
  )

  expect_true(is.data.frame(result_short), "Short posts should be processed")

  # Test with long posts
  long_posts <- test_data[7:9, ]  # These are the long posts from our test data
  result_long <- parallel_execute(
    test_data = long_posts,
    slicing_n = 3,
    cores = 2,
    multi_core = FALSE
  )

  expect_true(is.data.frame(result_long), "Long posts should be processed")

  # Test 5: Different column configurations
  cat("\nTesting different column configurations...\n")

  # Create data with different column order
  test_data_reordered <- data.frame(
    some_other_col = "other",
    post_id = 1:8,
    full_post = test_data$full_post[1:8]
  )

  set_parameters(n_variables = 4, id_column = 2, text_column = 3, sep = ";")
  set_coding_instruction("Extract: sentiment,confidence,topic,relevance")

  result_reordered <- parallel_execute(
    test_data = test_data_reordered,
    slicing_n = 8,
    cores = 2,
    multi_core = FALSE
  )

  expect_true(is.data.frame(result_reordered), "Reordered columns should work")

  # Test 6: Edge cases
  cat("\nTesting edge cases...\n")

  # Reset parameters for edge case test
  set_parameters(n_variables = 2, id_column = 1, text_column = 2, sep = ";")
  set_coding_instruction("Extract exactly 2 variables from each text: sentiment;confidence. Return ONLY the values separated by semicolons, no explanations or labels. Example: positive;0.8")

  # Test with very short slicing_n
  result_minimal <- parallel_execute(
    test_data = test_data[1:5, ],
    slicing_n = 3,
    cores = 1,
    multi_core = FALSE
  )

  expect_true(is.data.frame(result_minimal), "Minimal slicing should work")

  cat("\nFlexibility tests completed successfully!\n")
})
