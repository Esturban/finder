library(testthat)

# Test the page_dl function
context("page_dl function")

test_that("Page download should work when given a valid URL", {
  # Use a test URL that we know should work
  url <- "https://www.example.com"
  result <- page_dl(url)
  expect_is(result, "list")
  expect_is(result$page, "xml_document")
  expect_equal(result$check, 0)
  expect_null(result$err)
})

test_that("Page download should fail when given an invalid URL", {
  # Use a test URL that we know should not work
  url <- "https://www.thisdoesnotexist.com"
  result <- page_dl(url)
  expect_is(result, "list")
  expect_null(result$page)
  expect_equal(result$check, -1)
  expect_match(result$err, "Could not resolve host")
})

test_that("Page download should timeout when given a slow URL", {
  # Use a test URL that we know will time out
  url <- "https://httpstat.us/200?sleep=20000"
  result <- page_dl(url, seconds_elapsed = 1)
  expect_is(result, "list")
  expect_null(result$page)
  expect_equal(result$check, -1)
  expect_match(result$err, "Timeout was reached")
})
