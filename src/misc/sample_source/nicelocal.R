# Read the HTML content of the specified URL and extract the href attribute of the desired nodes
'https://nicelocal.ca/ontario-ca/beauty/type/massage/' %>%
  read_html %>%
  html_nodes(' div.minicard-item__info > h2 > a') %>%
  html_attr('href') -> p1

# Process the HTML files in the specified directory and extract the desired information
lapply(list.files(here::here('data','nicelocal','pages','categories'), pattern = "*.html", full.names = T), function(x) {
  # Extract the href attribute of the desired nodes
  x %>%
    read_html %>%
    html_nodes('.orglist-item a') %>%
    html_attr('href') -> links
  
  # Extract the text content of the desired nodes
  company_name <- x %>%
    read_html %>%
    html_nodes('.orglist-item a') %>%
    html_text
  
  # Create a tibble with the extracted information
  res <- tibble(company_name = company_name,
                path = links)
  res
}) %>% dplyr::bind_rows(.) -> nice_local_massage_ontario

# Measure the time taken to process the data
start <- Sys.time()

# Process the data and extract the website information for each company
nice_local_massage_ontario %>%
  dplyr::mutate(website = purrr::map2(.x = path, .y = company_name, ~{
    # Define the file path for the company's HTML page
    page_loc <- paste0(here::here('data','nicelocal','pages','businesses', paste0(which(.x == nice_local_massage_ontario$path), "_", dbSafeNames(.y))), '.html')
    cat("Company #", which(.x == nice_local_massage_ontario$path), ":", .y, "\n")
    
    # Check if the file exists; if not, read and save the HTML content
    if (!file.exists(page_loc)) {
      .x %>% read_html -> page
      xml2::write_html(page, file = page_loc)
      page_loc %>% read_html %>% html_nodes('.service-website-value a') %>% html_text -> website
      return(website)
    } else {
      # If the file exists, read the HTML content and extract the website information
      page_loc %>% read_html %>% html_nodes('.service-website-value a') %>% html_text -> website
      return(website)
    }
  })) -> test_nl

# Measure the time taken to process the data
end <- Sys.time()
end - start
