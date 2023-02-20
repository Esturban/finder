context("find_deps")

# Test with no dependencies
test_that("find_deps returns FALSE for no dependencies", {
  attrs <- list(id = "node1", class = "node-class", data = "node-data")
  sources <- c("id", "data")
  dep_keys <- c("d3", "D3", "d3.js", "D3.js")
  expect_false(find_deps(attrs, sources, dep_keys))
})

# Test with one dependency
# test_that("find_deps returns TRUE for one dependency", {
#   attrs <- list(id = "node1", class = "node-class", data = "https://d3js.org/d3.v6.min.js")
#   sources <- c("id", "data")
#   dep_keys <- c("d3", "D3", "d3.js", "D3.js")
#   expect_true(find_deps(attrs, sources, dep_keys))
# })

# Test with multiple dependencies
# test_that("find_deps returns TRUE for multiple dependencies", {
#   attrs <- list(id = "node1", class = "node-class", data = "https://d3js.org/d3.v6.min.js",
#                 src = "https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js")
#   sources <- c("id", "data", "src")
#   dep_keys <- c("d3", "D3", "d3.js", "D3.js", "jquery")
#   expect_true(find_deps(attrs, sources, dep_keys))
# })

# Test with empty attributes
test_that("find_deps returns FALSE for empty attributes", {
  attrs <- list()
  sources <- c("id", "data", "src")
  dep_keys <- c("d3", "D3", "d3.js", "D3.js", "jquery")
  expect_false(find_deps(attrs, sources, dep_keys))
})

# Test with empty dependency keys
test_that("find_deps returns FALSE for empty dependency keys", {
  attrs <- list(id = "node1", class = "node-class", data = "https://d3js.org/d3.v6.min.js",
                src = "https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js")
  sources <- c("id", "data", "src")
  dep_keys <- character(0)
  expect_false(find_deps(attrs, sources, dep_keys))
})
