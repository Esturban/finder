context("get_deps")

test_that("it should return NA_character_ for empty input", {
  expect_equal(get_deps(list(), c("id"), c("d3")), NA_character_)
})

test_that("it should return NA_character_ if no dependency key exists in input", {
  attrs <- list(id = "node1", class = "node-class", data = "node-data")
  expect_equal(get_deps(attrs, c("id", "data"), c("non-existent")), NA_character_)
})

test_that("it should return a character vector of dependency names found in the input attributes", {
  attrs <- list(id = "node1", class = "node-class", data = "node-data", script = "d3.js")
  expect_equal(get_deps(attrs, c("id", "data"), c("d3", "D3", "d3.js", "D3.js")), "d3.js")
})

test_that("it should return only unique dependencies", {
  attrs <- list(id = "node1", class = "node-class", data = "node-data", script = "d3.js", link = "d3.js")
  expect_equal(get_deps(attrs, c("id", "data", "script", "link"), c("d3", "D3", "d3.js", "D3.js")), "d3.js")
})

test_that("it should handle cases where dependency is a substring of a larger string", {
  attrs <- list(id = "node1", class = "node-class", data = "node-data", script = "myd3.js")
  expect_equal(get_deps(attrs, c("id", "data", "script"), c("d3", "D3", "d3.js", "D3.js")), "myd3.js")
})
