services_companies <-
  all_companies %>% dplyr::select(category, url, name, hs_path) %>% .[!duplicated(.$url), ]
sapply(list.files(here::here("src", "fns"), full.names = T), source, local = F)

start<-Sys.time()
services_dev <- services_companies %>%
  #Remove etsy, IG and FB
  #Approximately 14% removal
  .[sample.int(nrow(.), 100),] %>%
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


services_dev %>%
  dplyr::select(category, name, url, email, telephone) %>% print(n = 100)
end<-Sys.time()
end-start