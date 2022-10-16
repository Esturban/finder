# nat_algo
# Load all of the transformed data into an R object, data frame
# source(here::here("setup.R"), F)
# source(here::here("src","fns"), F)
smbs <-
  readRDS(file = paste0(
    here::here('data'),
    '/smbs_tformed_',
    format(Sys.time(), "%Y%m%d"),
    '.RDS'
  ))

# if(interactive())smbs <-
#   readRDS(file = paste0(
#     here::here('data'),
#     '/smbs_tformed_',
#     format(Sys.time()-86400, "%Y%m%d"),
#     '.RDS'
#   ))

# names(smbs)

#Begin runtime
# for(i in 1:10)
# {
  start <- Sys.time()
smbs_dev <- smbs %>%
  #Remove etsy, IG and FB
  #Approximately 14% removal
  dplyr::filter(!on_etsy,!on_instagram,!on_fb) %>%
  # .[sample.int(nrow(.), 20), ] %>%
  # smbs_dev<-smbs%>%
  dplyr::mutate(
    #Determine if the link is an independent website or if it's a down stream page of the site
    domain = purrr::map_chr(`Link to Website`,  ~ {
      if (grepl(".", basename(.x)))
        return(paste0("https://", gsub("[[:blank:]]|https:", "", domain(.x)), "/"))
      else
        return (paste0("https://", gsub("[[:blank:]]|https:", "", domain(.x)), "/"))
    }),
    #Collect the entire page
    main_page = purrr::map(domain,  ~page_dl(.x)),
    #Check if the main page is operational and if it's been collected
    main_operating = purrr::map_lgl(.x = main_page,  ~ !is.null(.x)),
    #For all operating websites,
    # find the header links
    header_links = purrr::map2(.x = main_page, .y = main_operating,  ~node_attrs(x = .x, selected = "head > link, head > script",condition = .y)),
    # find all of the page links,
    page_links = purrr::map2(.x = main_page, .y = main_operating,  ~node_attrs(x = .x, selected = "a",condition = .y)),
    instagram_user = purrr::map2(.x = page_links,.y = domain,~social_user(x = .x,verbose = F, stopwords = ig_stopwords)),
    twitter_user = purrr::map2(.x = page_links,.y = domain,~social_user(x = .x,social = "twitter",verbose = F, stopwords = ig_stopwords)),
    #Determine if the site relies on shopify assets
    is_shopify = purrr::map_lgl(.x = header_links,  ~ find_deps(attrs = .x,sources = c("href","src"),dep_keys = c("shopify"))),
    #Determine if the site relies on squarespace assets
    is_squarespace = purrr::map_lgl(.x = header_links,  ~ find_deps(attrs = .x,sources = c("href","src"),dep_keys = c("squarespace"))),
    #Determine if the site relies on wordpress assets
    is_wordpress = purrr::map_lgl(.x = header_links,  ~ find_deps(attrs = .x,sources = c("href","src"),dep_keys = c("wordpress"))),
    #Determine if the site relies on wordpress assets
    is_wix = purrr::map_lgl(.x = header_links,  ~ find_deps(attrs = .x,sources = c("href","src"),dep_keys = c("wix"))),
    #Determine if the site relies on book manager assets
    is_bookmanager = purrr::map_lgl(.x = header_links,  ~ find_deps(attrs = .x,sources = c("href","src"),dep_keys = c("bookmanager"))),
    #Determine if the site relies on cloudflare assets
    is_cloudflare = purrr::map_lgl(.x = header_links,  ~ find_deps(attrs = .x,sources = c("href","src"),dep_keys = c("cloudflare"))),
    #Determine if the site relies on netlify assets
    is_netlify = purrr::map_lgl(.x = header_links,  ~ find_deps(attrs = .x,sources = c("href","src"),dep_keys = c("netlify"))),
    #Determine if the site relies on gtm assets
    on_gtm = purrr::map_lgl(.x = header_links,   ~ find_deps(attrs = .x,sources = c("href","src"),dep_keys = c("googletagmanager"))),
    #Determine if the site relies on google analytics assets
    on_ga = purrr::map_lgl(.x = header_links,  ~ find_deps(attrs = .x,sources = c("href","src"),dep_keys = c("googleanalytics","google-analytics"))),
    #Determine if the site relies on universal analytics google analytics assets
    on_ua = purrr::map2_lgl(.x = header_links, .y = on_gtm,  ~ { if (.y) find_deps(attrs = .x,sources = c("href","src"),dep_keys = c("UA-(.*)")) else return(F)}),
    #Determine if the site includes GTM or GA assets
    on_gtm_ga = on_gtm | on_ga,
    #Determine if the site includes GA4 assets
    on_ga4 = purrr::map2_lgl(.x = header_links, .y = on_gtm_ga,  ~ { if (.y) find_deps(attrs = .x,sources = c("href","src"),dep_keys = c("G-(.*)")) else return(F) }),
    #Determine if the site relies on bold assets
    on_bold = purrr::map2_lgl(.x = header_links, .y = on_gtm_ga,  ~ {
      if (.y)find_deps(attrs = .x,sources = c("href","src"),dep_keys = c("boldapps"))
      else return(F)
    }),
  )
end <- Sys.time()
print(end - start)
# }
#Save the development algorithms into an intermediate dataset
saveRDS(object = smbs_dev,
        file = paste0(
          here::here('data'),
          "/smbs_dev_",
          format(Sys.time(), '%Y%m%d'),
          ".RDS"
        ))
str(smbs_dev)
#
smbs_dev2 <-
  readRDS(file = paste0(
    here::here('data'),
    '/smbs_dev_',
    format(Sys.time()-86400*3, "%Y%m%d"),
    '.RDS'
))

# smbs_dev2%>%
#   dplyr::filter(main_operating)%>%
#   .[sample.int(nrow(.), 20), ]%>%
#   dplyr::tibble(.)%>%
#   # dplyr::select(4)%>%
#   dplyr::mutate(social = map(.x = main_page,~{.x%>%html_nodes("a")%>%html_attrs(.)}))


"https://themakestation.ca/" %>% read_html %>% html_nodes("a") %>% html_attrs(.) %>%
  lapply(., function(x) {
    if ("href" %in% names(x)) {
      # print(names(x))
      if (grepl("instagram", x[["href"]]))
        return(x[["href"]])
      else
        return(NULL)
    }
  }) %>% purrr::compact(.) %>% unlist(.)%>% 
  gsub("/p/\\w+","",.)%>%
  gsub(paste0(ig_stopwords,collapse = "|"), "", .)%>%.[nchar(.)>0]%>%unique(.)


purrr::map_chr(smbs_dev$Name, as.character)
table(smbs$`Link to Website`) %>% .[order(., decreasing = T)] %>% .[. ==
                                                                      1] %>% length(.)
table(smbs_dev$main_operating) %>% .[order(., decreasing = T)]
sum(smbs_dev$is_shopify)
smbs_dev %>%
  dplyr::filter(main_operating) %>%
  dplyr::group_by(is_shopify, on_gtm_ga) %>%
  dplyr::summarise(not_on_ga = n())
require(rvest)
smbs_dev %>%
  dplyr::filter(main_operating) %>%
  select(
    Name,
    `Link to Website`,
    Category,
    header_links,
    is_shopify,
    is_squarespace,
    is_bookmanager,
    is_wordpress,
    on_gtm_ga,
    on_ua,
    on_ga4,
    on_bold
  ) %>%
  tibble()#%>%
# tidyr::unnest(header_links)%>%
# tidyr::unnest(header_links)
smbs_dev %>%
  dplyr::filter(main_operating) %>%
  select(Name, `Link to Website`)
