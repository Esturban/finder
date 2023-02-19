context("social_user")

# Define a test case
test_that("social_user correctly identifies social media source", {
  attrs <- list(href = "https://www.instagram.com/example/",
                src = "https://www.example.com/assets/images/instagram.png")
  expected_output <- "https://www.instagram.com/example/"
  social <- "instagram"
  output <- social_user(x = attrs, social = social)
  expect_equal(output, expected_output)
})

# Define another test case
test_that("social_user correctly handles empty input", {
  attrs <- list(href = "", src = "")
  expected_output <- NA_character_
  social <- "instagram"
  output <- social_user(x = attrs, social = social)
  expect_equal(output, expected_output)
})
