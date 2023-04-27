# c247_details
require(tidyverse)
require(rvest)
require(rjson)
require(R.utils)
#'
#' Extracting the domain name from a string
#' @description The domain name is an element of a URL necessary for categorizing clients. This function will strip all of the unnecessary components of the URL so that the domain name remains
domain <-
  function(x)
    strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
sapply(list.files(here::here("src", "fns"), full.names = T), source, local = F)
# Details for a set of websites
# First pass will be for Ontario
c247_df <-
  readRDS(here::here("data", "canada247", "explore", "explore_companies_all.RDS"))

length(unique(c247_df$address))
length(unique(c247_df$phone))
length(unique(c247_df$address))
#Only for not Ontario
length(unique(c247_df$address[!grepl(", ON", c247_df$address)]))
which(grepl("linfo-graf-inc", c247_df$path))
c247_not_on <- c247_df %>%
  dplyr::filter(
    #in ontario
    !grepl(", ON", .$address),
    #Not missing a phone number
    !phone == "",
    #remove common unlikely businesses
    !grepl(
    "/canada_post.html|/mcdonalds-[[:digit:]]-*.html|/circle-k-[[:digit:]]-*.html",
    path
  ),
  !path == "https//canada247.info/",
  !is.na(path)
#Remove complete 247 info paths!path == "https//canada247.info/"
) %>%
  .[!duplicated(.$path),]

#390k business names
length(unique(c247_not_on$company_name))
rows_to_save <- 490000:nrow(c247_not_on)
#440k unique business paths from the explore pages
length(unique(c247_not_on$path))

start <- Sys.time()
services_canada_dev <- c247_not_on %>%
  .[rows_to_save,] %>%
  # .[sample.int(nrow(.),200),]%>%
  # smbs_dev<-smbs%>%
  dplyr::mutate(
    #Determine if the link is an independent website or if it's a down stream page of the site
    domain = path,
    #Collect the entire page
    main_page = purrr::map(domain,  ~ {
      # print(.x)
      page_dl(.x, verbose = T) -> page_res
      page_res
    }),
    #Check if the main page is operational and if it's been collected
    main_operating = purrr::map_lgl(.x = main_page,  ~ !is.null(.x$page)),
    page_attrs = purrr::map2(.x = main_operating, .y = main_page,  ~ {
      if (.x) {
        .y$page %>%
          html_nodes(".width60p a") %>%
          html_attr("href") %>%
          gsub("https://canada247.info/redirect.php\\?to=", "", .) %>%
          URLdecode(.) -> attrs
        return(attrs)
      } else
        return(NA_character_)
    }),
    description = purrr::map2(.x = main_operating, .y = main_page,  ~ {
      if (.x) {
        .y$page %>%
          html_nodes("#page-description") %>%
          html_text() -> text_description
        # print(text_description)
        text_description %>%
          paste0(collapse = "; ") %>%
          gsub('[^[:print:]]+', "", .)  -> attrs
        return(attrs)
      } else
        return(NA_character_)
    }),
    email = purrr::map2(.x = main_operating, .y = page_attrs,  ~ {
      if (.x) {
        .y %>% .[!grepl("facebook|twitter|instagram|linkedin|youtube|pinterest|tiktok",
                        .)] %>% .[grepl("mailto", .)] ->
          url
        if (identical(url, character(0)))
          return(NA_character_)
        else
          return(url)
      }
      else
        return(NA_character_)
    }),
    website = purrr::map2(.x = main_operating, .y = page_attrs,  ~ {
      if (.x) {
        .y %>% .[!grepl("facebook|twitter|instagram|linkedin|youtube|pinterest|tiktok",
                        .)] %>% .[grepl("http", .)] ->
          url
        if (identical(url, character(0)))
          return(NA_character_)
        else
          return(url)
      }
      else
        return(NA_character_)
    }),
    facebook = purrr::map2(.x = main_operating, .y = page_attrs,  ~ {
      if (.x) {
        .y %>% .[grepl("facebook", .)] -> url
        if (identical(url, character(0)))
          return(NA_character_)
        else
          return(url)
      }
      else
        return(NA_character_)
    }),
    linkedin = purrr::map2(.x = main_operating, .y = page_attrs,  ~ {
      if (.x) {
        .y %>% .[grepl("linkedin", .)] -> url
        if (identical(url, character(0)))
          return(NA_character_)
        else
          return(url)
      }
      else
        return(NA_character_)
    }),
    pinterest = purrr::map2(.x = main_operating, .y = page_attrs,  ~ {
      if (.x) {
        .y %>% .[grepl("pinterest", .)] -> url
        if (identical(url, character(0)))
          return(NA_character_)
        else
          return(url)
      }
      else
        return(NA_character_)
    }),
    instagram = purrr::map2(.x = main_operating, .y = page_attrs,  ~ {
      if (.x) {
        .y %>% .[grepl("instagram", .)] -> url
        if (identical(url, character(0)))
          return(NA_character_)
        else
          return(url)
      }
      else
        return(NA_character_)
    }),
    youtube = purrr::map2(.x = main_operating, .y = page_attrs,  ~ {
      if (.x) {
        .y %>% .[grepl("youtube", .)] -> url
        if (identical(url, character(0)))
          return(NA_character_)
        else
          return(url)
      }
      else
        return(NA_character_)
    }),
    twitter = purrr::map2(.x = main_operating, .y = page_attrs,  ~ {
      if (.x) {
        .y %>% .[grepl("twitter", .)] -> url
        if (identical(url, character(0)))
          return(NA_character_)
        else
          return(url)
      }
      else
        return(NA_character_)
    }),
    tiktok = purrr::map2(.x = main_operating, .y = page_attrs,  ~ {
      if (.x) {
        .y %>% .[grepl("tiktok", .)] -> url
        if (identical(url, character(0)))
          return(NA_character_)
        else
          return(url)
      }
      else
        return(NA_character_)
    })
  )

saveRDS(services_canada_dev,
        file = here::here(
          "data",
          "canada247",
          paste0(
            "explore_details_rest_of_",
            min(rows_to_save),
            "_to_",
            max(rows_to_save),
            ".RDS"
          )
        ))
end <- Sys.time()
end - start

