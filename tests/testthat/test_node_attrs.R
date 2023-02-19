context("node_attrs function")

test_that("node_attrs returns the expected list of attributes for a given CSS selector", {
  html_file <- read_html("<html><head><link rel='stylesheet' href='style.css'></head><body><p class='paragraph'>Hello, world!</p></body></html>")
  expected_list <- list(href = "style.css")
  output_list <- node_attrs(html_file, selected = "head > link", condition = TRUE)
  expect_identical(expected_list, output_list)
})

test_that("node_attrs handles non-existent CSS selectors and returns NULL", {
  html_file <- read_html("<html><head><link rel='stylesheet' href='style.css'></head><body><p class='paragraph'>Hello, world!</p></body></html>")
  expected_output <- 
  output <- node_attrs(html_file, selected = ".non-existent-class", condition = TRUE)
  expect_identical(expected_output, output)
})

test_that("node_attrs handles invalid HTML input and returns NULL", {
  html_file <- "<html><head><link rel='stylesheet' href='style.css'></head><body><p class='paragraph'>Hello, world!</p></body></html>"
  expected_output <- NULL
  output <- node_attrs(html_file, selected = "head > link", condition = TRUE)
  expect_identical(expected_output, output)
})
