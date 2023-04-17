# Load required library
require(tidyverse)

# Canada 247 info
# From scouring the explore pages, it has been determined that there are
# close to 50,000 pages of available businesses and their corresponding contacts
# with 20 on each page

# Generate the list of URLs to scrape
c(
  "https://canada247.info/explore",
  paste0("https://canada247.info/explore/page/", 1:48492)
) -> canada247_links

# Source helper functions from the "src/fns" folder
sapply(list.files(here::here("src", "fns"), full.names = T), source, local = F)

# Record the start time
start <- Sys.time()

# From 1 to 25000
# Time: 11.7 hours
# From 25001 to end
# Time: 16.9 hours

# Check if the "explore_companies_all.RDS" file does not exist and proceed with the scraping
if (!file.exists(here::here("data", "canada247", "explore", "explore_companies_all.RDS"))) {
  # Loop through each link in canada247_links
  df_list <- lapply(canada247_links, function(x) {
    # Print the current link for progress tracking
    print(x)
    
    # Download the page content
    x %>% page_dl(.) -> html_page
    unlink(x)
    
    # Check if the page content is not NULL
    if (!is.null(html_page$page)) {
      # Extract the required information from the page
      html_page$page %>%
        html_nodes("h3 a") %>%
        html_attr("href") -> c247_path
      
      html_page$page %>%
        html_nodes("h3 a") %>%
        html_text(.) -> company_name
      
      html_page$page %>%
        html_nodes("#left-content h3+ div") %>%
        html_text(.) %>%
        gsub("(?<=[a-z])[[:blank:]](?=[A-Z])", ";", ., perl = T) -> categories
      
      html_page$page %>%
        html_nodes(".explore-addr") %>%
        html_text(.) %>% gsub("Phone: ", "", .) -> phone
      
      html_page$page %>%
        html_nodes(".place-address") %>%
        html_text(.) %>%
        gsub("Address:  ", "", .) -> address
      
      # Create a tibble with the extracted data
      tibble(
        link = x,
        path = c247_path,
        company_name = company_name,
        categories = categories,
        phone = phone,
        address = address
      )
    }
  }) %>% purrr::compact(.)
  
  # Combine the extracted data into a single data frame
  c247_df <- dplyr::bind_rows(df_list)
  
  # Save the data frame as an RDS file
  saveRDS(c247_df, here::here("data", "canada247", "explore", "explore_companies_all.RDS"))
}

# Record the end time
end <- Sys.time()

# Calculate and display the time taken to process the pages
end - start
