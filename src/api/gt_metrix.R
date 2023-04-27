# Load required package for making HTTP requests
require(httr)

# Retrieve GTMetrix API key from environment variable
gt_api <- Sys.getenv("GT_METRIX_API")

# Request the API status
res <- httr::GET(url = 'https://gtmetrix.com/api/2.0/status', httr::authenticate(gt_api, ''))

# Parse the response content as JSON
rjson::fromJSON(content(res, 'text'))

# Set headers for POST requests
headers = c(`Content-Type` = 'application/vnd.api+json')

# Prepare data payload for the test request
data = '\n{\n  "data": {\n    "type": "test",\n    "attributes": {\n      "url":      "https://estebanvalencia.com",\n    "adblock":  1\n   }\n  }\n}'

# Perform a POST request to create a test
res_ev <-
  httr::POST(
    url = 'https://gtmetrix.com/api/2.0/tests',
    httr::add_headers(.headers = headers),
    body = data,
    httr::authenticate(gt_api, '')
  )

# Parse the response content as JSON and extract the test ID
rjson::fromJSON(content(res_ev, 'text'))$data$id

# Load required package for making HTTP requests
require(httr)

# Loop through the first 10 elements of smbs_test
for (i in smbs_test[1:10]) {
  headers = c(`Content-Type` = 'application/vnd.api+json')
  
  # Prepare data payload for the test request
  data = paste0(
    '\n{\n  "data": {\n    "type": "test",\n    "attributes": {\n      "url":      "',
    domain(i),
    '",\n    "adblock":  1\n   }\n  }\n}'
  )
  
  # Perform a GET request to retrieve test results
  res_tests <-
    httr::GET(
      url = 'https://gtmetrix.com/api/2.0/tests',
      httr::add_headers(.headers = headers),
      body = data,
      httr::authenticate(gt_api, '')
    )
  
  # Parse the response content as JSON
  rjson::fromJSON(content(res_tests, 'text'))
}

# Perform a GET request to retrieve test results
httr::GET(url = 'https://gtmetrix.com/api/2.0/tests',
          httr::authenticate(gt_api, '')) -> res_out

# Parse the response content as JSON
rjson::fromJSON(content(res_out, 'text'))
