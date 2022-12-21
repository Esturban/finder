require(tidyverse)
require(rvest)
require(rjson)
require(R.utils)

#From the categories that were scraped, collect all of the unique services
# companies that had been gathered in the process of crawling all of the
# categories by region/city
services_companies <-
  readRDS(file = here::here("data", "homestars", "categories", "companies_links_unq.RDS"))
# %>% dplyr::select(category, url, name, hs_path) %>% .[!duplicated(.$url), ]
# saveRDS(services_companies,here::here("data","homestars","categories","companies_links_unq.RDS"))
#Load all of the source files and functions needed to collect the website information
sapply(list.files(here::here("src", "fns"), full.names = T), source, local = F)

start <- Sys.time()
services_dev <- services_companies %>%
  #Remove etsy, IG and FB
  #Approximately 14% removal
  # .[c(20001:20822,20824:nrow(.)),] %>%
  # smbs_dev<-smbs%>%
  dplyr::mutate(
    #Determine if the link is an independent website or if it's a down stream page of the site
    domain = purrr::map_chr(url,  ~ {
      if (grepl(".", basename(.x)))
        return(paste0("https://", gsub("[[:blank:]]|https:", "", domain(.x)), "/"))
      else
        return (paste0("https://", gsub("[[:blank:]]|https:", "", domain(.x)), "/"))
    }),
    #Collect the entire page
    main_page = purrr::map(domain,  ~ page_dl(.x)),
    #Check if the main page is operational and if it's been collected
    main_operating = purrr::map_lgl(.x = main_page,  ~ !is.null(.x$page)),
    #For all operating websites,
    # find the header links
    header_links = purrr::map2(
      .x = main_page,
      .y = main_operating,
      ~ node_attrs(
        x = .x$page,
        selected = "head > link, head > script",
        condition = .y
      )
    ),
    # find all of the page links,
    page_links = purrr::map2(
      .x = main_page,
      .y = main_operating,
      ~ node_attrs(
        x = .x$page,
        selected = "a",
        condition = .y
      )
    ),
    instagram_user = purrr::map2(
      .x = page_links,
      .y = domain,
      ~ social_user(
        x = .x,
        verbose = F,
        stopwords = ig_stopwords
      )
    ),
    twitter_user = purrr::map2(
      .x = page_links,
      .y = domain,
      ~ social_user(
        x = .x,
        social = "twitter",
        verbose = F,
        stopwords = ig_stopwords
      )
    ),
    #Determine if the site relies on shopify assets
    is_shopify = purrr::map_lgl(.x = header_links,  ~ find_deps(
      attrs = .x,
      sources = c("href", "src"),
      dep_keys = c("shopify")
    )),
    #Determine if the site relies on squarespace assets
    is_squarespace = purrr::map_lgl(.x = header_links,  ~ find_deps(
      attrs = .x,
      sources = c("href", "src"),
      dep_keys = c("squarespace")
    )),
    #Determine if the site relies on wordpress assets
    is_wordpress = purrr::map_lgl(.x = header_links,  ~ find_deps(
      attrs = .x,
      sources = c("href", "src"),
      dep_keys = c("wordpress")
    )),
    #Determine if the site relies on wordpress assets
    is_wix = purrr::map_lgl(.x = header_links,  ~ find_deps(
      attrs = .x,
      sources = c("href", "src"),
      dep_keys = c("wix")
    )),
    #Determine if the site relies on book manager assets
    is_bookmanager = purrr::map_lgl(.x = header_links,  ~ find_deps(
      attrs = .x,
      sources = c("href", "src"),
      dep_keys = c("bookmanager")
    )),
    #Determine if the site relies on cloudflare assets
    is_cloudflare = purrr::map_lgl(.x = header_links,  ~ find_deps(
      attrs = .x,
      sources = c("href", "src"),
      dep_keys = c("cloudflare")
    )),
    #Determine if the site relies on netlify assets
    is_netlify = purrr::map_lgl(.x = header_links,  ~ find_deps(
      attrs = .x,
      sources = c("href", "src"),
      dep_keys = c("netlify")
    )),
    #Determine if the site relies on gtm assets
    on_gtm = purrr::map_lgl(.x = header_links,   ~ find_deps(
      attrs = .x,
      sources = c("href", "src"),
      dep_keys = c("googletagmanager")
    )),
    #Determine if the site relies on gtm assets
    on_hotjar = purrr::map_lgl(.x = header_links,   ~ find_deps(
      attrs = .x,
      sources = c("href", "src"),
      dep_keys = c("hotjar")
    )),
    #Determine if the site relies on google analytics assets
    on_ga = purrr::map_lgl(.x = header_links,  ~ find_deps(
      attrs = .x,
      sources = c("href", "src"),
      dep_keys = c("googleanalytics", "google-analytics")
    )),
    #Determine if the site relies on universal analytics google analytics assets
    on_ua = purrr::map2_lgl(.x = header_links, .y = on_gtm,  ~ {
      if (.y)
        find_deps(
          attrs = .x,
          sources = c("href", "src"),
          dep_keys = c("UA-(.*)")
        )
      else
        return(F)
    }),
    #Determine if the site includes GTM or GA assets
    on_gtm_ga = on_gtm | on_ga,
    #Determine if the site includes GA4 assets
    on_ga4 = purrr::map2_lgl(.x = header_links, .y = on_gtm_ga,  ~ {
      if (.y)
        find_deps(
          attrs = .x,
          sources = c("href", "src"),
          dep_keys = c("G-(.*)")
        )
      else
        return(F)
    }),
    #Determine if the site relies on bold assets
    on_bold = purrr::map2_lgl(.x = header_links, .y = on_gtm_ga,  ~ {
      if (.y)
        find_deps(
          attrs = .x,
          sources = c("href", "src"),
          dep_keys = c("boldapps")
        )
      else
        return(F)
    }),
    email = purrr::map2_chr(.x = main_page, .y = main_operating,  ~ {
      if (.y)
        .x$page %>% node_attrs(., selected = "a") %>% get_deps(
          attrs = .,
          sources = c("href", "src"),
          dep_keys = c("mailto:")
        )
      else
        return(NA_character_)
    }),
    telephone = purrr::map2_chr(.x = main_page, .y = main_operating,  ~ {
      if (.y)
        .x$page %>% node_attrs(., selected = "a") %>% get_deps(
          attrs = .,
          sources = c("href", "src"),
          dep_keys = c("tel:")
        )
      else
        return(NA_character_)
    }),
  )
if (interactive())
  services_dev %>%
  dplyr::select(category,
                name,
                url,
                email,
                telephone,
                instagram_user,
                twitter_user) %>%
  readr::write_csv(., file = "sample_emails_3.csv")

#This is how the original file was made
#Had to be done in 3 parts due to the lack of RAM 
# on my local machine and issues in the runtime with hanging links when scraping
# services_data <-
#   lapply(
#     list.files(
#       here::here("data", "homestars", "categories"),
#       recursive = F,
#       pattern = "_[[:digit:]]*.RDS",
#       full.names = T
#     ),
#     readRDS
#   )
# 
# services_df <- services_data %>% dplyr::bind_rows(.)
# saveRDS(
#   services_df,
#   file = here::here(
#     "data",
#     "homestars",
#     "categories",
#     "companies_details_original_source.RDS"
#   )
# )

#Save the full file with all of the original data from the scrapes
saveRDS(
  services_dev,
  file = here::here(
    "data",
    "homestars",
    "categories",
    "companies_details_original_source.RDS"
  )
)
