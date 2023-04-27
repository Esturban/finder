# c247_details
require(tidyverse)
require(rvest)
require(rjson)
require(R.utils)

sapply(list.files(here::here("src", "fns"), full.names = T), source, local = F)
# Details for a set of websites
# First pass will be for Ontario
c247_df <-
  readRDS(here::here("data", "canada247","explore", "explore_companies_all.RDS"))

#Only for Ontario
length(unique(c247_df$address[grepl(", ON", c247_df$address)]))

c247_on <- c247_df %>%
  dplyr::filter(
    #in ontario
    grepl(", ON", .$address),
    #Not missing a phone number
    !phone == "",
    #remove common unlikely businesses
    !grepl(
      "/canada_post.html|/mcdonalds-[[:digit:]]-*.html|/circle-k-[[:digit:]]-*.html",
      path
    ),
    #Remove complete 247 info paths
    !path == "https//canada247.info"
  ) %>%
  .[!duplicated(.$path), ]
#390k business names
length(unique(c247_on$company_name))
rows_to_save<-340001:380000
#440k unique business paths from the explore pages
length(unique(c247_on$path))

start <- Sys.time()
services_canada_dev <- c247_on %>%
  dplyr::filter(
    !phone == "",
    !grepl(
      "/canada_post.html|/mcdonalds-[[:digit:]]-*.html|/circle-k-[[:digit:]]-*.html",
      path
    ),
    !path == "https//canada247.info"
  ) %>%
  .[rows_to_save, ] %>%
  # .[sample.int(nrow(.),200),]%>%
  # smbs_dev<-smbs%>%
  dplyr::mutate(
    #Determine if the link is an independent website or if it's a down stream page of the site
    domain = path,
    #Collect the entire page
    main_page = purrr::map(domain,  ~ {
      page_dl(.x, verbose = T)->page_res
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
    email = purrr::map2(.x = main_operating, .y = page_attrs,  ~ {
      if (.x) {
        .y %>% .[!grepl("facebook|twitter|instagram|linkedin|youtube|pinterest|tiktok", .)] %>% .[grepl("mailto", .)] ->
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
        .y %>% .[!grepl("facebook|twitter|instagram|linkedin|youtube|pinterest|tiktok", .)] %>% .[grepl("http", .)] ->
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

saveRDS(
  services_canada_dev,
  file = here::here("data", "canada247", paste0("explore_details_",min(rows_to_save),"_to_",max(rows_to_save),".RDS"))
)
end <- Sys.time()
end - start
