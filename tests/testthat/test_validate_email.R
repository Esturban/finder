context("validate_email")

test_that("valid email is correctly validated", {
  email <- "example@example.com"
  expect_true(validate_email(email))
})

test_that("invalid email is correctly invalidated", {
  email <- "example@.com"
  expect_false(validate_email(email))
})

test_that("email with multiple @ symbols is correctly invalidated", {
  email <- "example@@example.com"
  expect_false(validate_email(email))
})

test_that("email with spaces is correctly invalidated", {
  email <- "example @example.com"
  expect_false(validate_email(email))
})

