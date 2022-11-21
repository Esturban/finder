hsv_csvs <-
  list.files(
    here::here("data", "homestars", "categories"),
    pattern = '.csv',
    full.names = T
  )
start <- Sys.time()
lapply(hsv_csvs, function(x)
{
  readr::read_csv(x, show_col_types = FALSE)
}) -> csv_list


categories_full <- csv_list %>%
  dplyr::bind_rows(.)

glimpse(categories_full)

categories_full %<>%
  dplyr::mutate(status_code = purrr::map_chr(.x = page_status,  ~ {
    rjson::fromJSON(.x)$message -> msg
    gsub("[\\(\\)]", "", regmatches(msg, gregexpr("\\(.*?\\)", msg))[[1]])
  }))
end <- Sys.time()
end - start
# length(unique(categories_full$Link))
categories_full%>%unique(.)->categories_unq
# categories_full%>%
#   group_by(status_code)%>%
#   summarise(n=n())
readr::write_csv(
  categories_full,
  file = here::here(
    "data",
    "homestars",
    "categories",
    "all_cities_categories.csv"
  )
)

readr::write_csv(
  categories_unq,
  file = here::here(
    "data",
    "homestars",
    "categories",
    "all_cities_categories_unique.csv"
  )
)
