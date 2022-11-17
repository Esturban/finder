# homestar_category_validation
start <- Sys.time()
#Only review 350 random cities at a time
# lapply(cities[sample.int(length(cities),350)], function(x) {
lapply(cities, function(x) {
  #City being analyzed
  if (!file.exists(here::here(
    "data",
    "homestars",
    "categories",
    paste0("cat_", gsub("[[:punct:]]|[[:blank:]]", "", x), ".csv")
  ))) {
  
    print(x)
    intermediate_city_categories <-
      city_cats %>% dplyr::filter(City == x)
    
    # intermediate_city_categories[1:10,]%>%
    intermediate_city_categories %>%
      dplyr::mutate(
        #Download the full web page
        # page = purrr::map(.x = Link,~page_dl(.x,skip = T)),
        #RCurl Test - Determine if the page exists or not
        # page_test = purrr::map(.x = Link,~{
        #   RCurl::url.exists(.x)->res
        #   print(res)
        #   return(res)
        # }),
        #HTTR Test - Determine if the page exists or not
        page_status = purrr::map(.x = Link,  ~ {
          httr::http_status(httr::GET(.x)) -> res
          # print(res)
          return(res)
        }),
        status_code = purrr::map(.x = page_status,  ~ gsub(
          "[\\(\\)]", "", regmatches(.x$message, gregexpr("\\(.*?\\)", .x$message))[[1]]
        ))
      ) -> city_dataset
    
    city_dataset %>% dplyr::mutate(
      page_status = purrr::map_chr(.x = page_status,  ~ rjson::toJSON(.x)),
      update_time = Sys.time()
    ) -> city_dataset
    if (!dir.exists(here::here("data", "homestars", "categories")))
      dir.create(here::here("data", "homestars", "categories"))
    readr::write_csv(city_dataset,
                     file = here::here(
                       "data",
                       "homestars",
                       "categories",
                       paste0("cat_", gsub("[[:punct:]]|[[:blank:]]", "", x), ".csv")
                     ))
  }
  
  
}) %>% dplyr::bind_rows(.) -> cities_categories_validated

end <- Sys.time()
end - start

readr::write_csv(
  cities_categories_validated,
  file = here::here(
    "data",
    "homestars",
    "categories",
    "all_cities_categories.csv"
  )
)
# head(city_cats)
# httr::httr::http_status(
#   httr::GET(
#     "https://homestars.com/on/1,%20solomons%20island/basement-renovation"
#   )
# ) -> resp
# resp$category
# resp$reason
# resp$message
# gsub("[\\(\\)]", "", regmatches(resp$message, gregexpr("\\(.*?\\)", resp$message))[[1]])
# test_list %>% dplyr::tibble(.)