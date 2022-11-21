# create_company_category and URLs

all_cc_csvs_paths <-
  list.files(
    here::here("data", "homestars", "categories", "details"),
    pattern = "*.csv",
    full.names = T
  )
all_cc_csvs_paths[1:10]

start <- Sys.time()
test_companies <-
  lapply(all_cc_csvs_paths, function(x) {
    readr::read_csv(x, show_col_types = F) %>%
      dplyr::mutate(
        n_hits = as.numeric(n_hits),
        project_minimum = ifelse(exists("project_minimum"), as.character(project_minimum), NA_character_),
        project_rate = ifelse(exists("project_rate"), as.character(project_rate), NA_character_),
        diplomas = ifelse(exists("diplomas"), as.character(diplomas), NA_character_),
        licenses = ifelse(exists("licenses"), as.character(licenses), NA_character_),
        subdomain = ifelse(exists("subdomain"), as.character(subdomain), NA_character_),
        permalink = ifelse(exists("permalink"), as.character(permalink), NA_character_),
        name = ifelse(exists("name"), as.character(name), NA_character_),
        hours = ifelse(exists("hours"), as.character(hours), NA_character_)
      )
  }) %>%
  dplyr::bind_rows(.)
end <- Sys.time()
end - start

length(unique(test_companies$hs_path))
length(unique(test_companies$parent_company_id))
length(unique(test_companies$url))
