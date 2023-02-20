library(testthat)

test_that("nullif() function works as expected", {
  expect_equal(nullif("hello"), "hello")
  expect_equal(nullif(NA_character_), NA_character_)
  expect_equal(nullif(NULL), NA_character_)
})

test_that("nullifN() function works as expected", {
  expect_equal(nullifN(5), 5)
  expect_equal(nullifN(NA_real_), NA_real_)
  expect_equal(nullifN(NULL), NA_real_)
})

test_that("nullifI() function works as expected", {
  expect_equal(nullifI(10L), 10L)
  expect_equal(nullifI(NA_integer_), NA_integer_)
  expect_equal(nullifI(NULL), NA_integer_)
})
