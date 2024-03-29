context("domain")

test_that("domain extracts the correct domain name from a URL", {
  expect_equal(domain("https://www.example.com"), "example.com")
  expect_equal(domain("https://www.example.com/path/to/resource"), "example.com")
  expect_equal(domain("http://subdomain.example.com/path/to/resource"), "subdomain.example.com")
  # expect_equal(domain("https://www.example.com?param=value"), "example.com")
  # expect_equal(domain("https://127.0.0.1:8000"), "127.0.0.1")
  expect_equal(domain("https://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]"), "[2001:0db8:85a3:0000:0000:8a2e:0370:7334]")
  expect_equal(domain("http://example.com"), "example.com")
  expect_equal(domain("http://example.com/"), "example.com")
  expect_equal(domain("example.com"), "example.com")
  expect_equal(domain("http://localhost"), "localhost")
  expect_equal(domain("http://localhost/path/to/resource"), "localhost")
})
