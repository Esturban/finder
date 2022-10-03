# nat_algo
# Load all of the transformed data into an R object, data frame
source(here::here("setup.R"),F)
smbs <-
  readRDS(file = paste0(
    here::here('data'),
    '/smbs_tformed_',
    format(Sys.time(), "%Y%m%d"),
    '.RDS'
  ))

#Begin runtime
start<-Sys.time()
smbs_dev <- smbs %>%
  #Remove etsy, IG and FB
  #Approximately 14% removal
  dplyr::filter(!on_etsy, !on_instagram, !on_fb) %>%
  # .[sample.int(nrow(.), 20), ] %>%
  # smbs_dev<-smbs%>%
  dplyr::mutate(
    #Determine if the link is an independent website or if it's a down stream page of the site
    domain = purrr::map_chr(`Link to Website`,~{
      if(grepl(".",basename(.x)))return(gsub("[[:blank:]]","",.x))
      else return (paste0("https://",gsub("[[:blank:]]","",domain(.x)),"/"))
    }),
    #Collect the entire page
    main_page = purrr::map(domain,  ~ {
      #Debug: See how the domains are written
      # print(domain(.x))
      tryCatch({
        page <- rvest::read_html(.x)
        unlink(.x)
        page
      }
      , error = function(e)
        {#Return a NULL if missing, checked in the next step
        print(paste0("Failed: ",.x))
        print(e)
        return(NULL)})
    }),
    #Check if the main page is operational and if it's been collected
    main_operating = purrr::map_lgl(.x = main_page,  ~ !is.null(.x)),
    #For all operating websites,
    scripts_links = purrr::map2(.x = main_page, .y = main_operating,  ~
                                  {
                                    
                                    if (.y)
                                      .x %>% html_nodes("head > link, head > script") %>% html_attrs(.) %>% return(.)
                                    else
                                      return(NULL)
                                  }),
    #Determine if the site relies on shopify assets
    is_shopify = purrr::map_lgl(.x = scripts_links,  ~ {
      lapply(.x, function(x) {
        param_names <- names(x)
        if ("href" %in% param_names)
          return(grepl("shopify", x))
        if ("src" %in% param_names)
          return(grepl("shopify", x))
        return(F)
      }) %>% unlist(.) %>% sum(.) > 0
    }),
    #Determine if the site relies on squarespace assets
    is_squarespace = purrr::map_lgl(.x = scripts_links,  ~ {
      lapply(.x, function(x) {
        param_names <- names(x)
        if ("href" %in% param_names)
          return(grepl("squarespace", x))
        if ("src" %in% param_names)
          return(grepl("squarespace", x))
        return(F)
      }) %>% unlist(.) %>% sum(.) > 0
    }),
    #Determine if the site relies on wordpress assets
    is_wordpress = purrr::map_lgl(.x = scripts_links,  ~ {
      lapply(.x, function(x) {
        param_names <- names(x)
        if ("href" %in% param_names)
          return(grepl("wordpress", x))
        if ("src" %in% param_names)
          return(grepl("wordpress", x))
        return(F)
      }) %>% unlist(.) %>% sum(.) > 0
    }),
    #Determine if the site relies on book manager assets
    is_bookmanager = purrr::map_lgl(.x = scripts_links,  ~ {
      lapply(.x, function(x) {
        param_names <- names(x)
        # print(param_names)
        if ("href" %in% param_names)
          return(grepl("bookmanager", x))
        if ("src" %in% param_names)
          return(grepl("bookmanager", x))
        return(F)
      }) %>% unlist(.) %>% sum(.) > 0
    }),
    #Determine if the site relies on cloudflare assets
    is_cloudflare = purrr::map_lgl(.x = scripts_links,  ~ {
      lapply(.x, function(x) {
        param_names <- names(x)
        # print(param_names)
        if ("href" %in% param_names)
          return(grepl("cloudflare", x))
        if ("src" %in% param_names)
          return(grepl("cloudflare", x))
        return(F)
      }) %>% unlist(.) %>% sum(.) > 0
    }),
    #Determine if the site relies on netlify assets
    is_netlify = purrr::map_lgl(.x = scripts_links,  ~ {
      lapply(.x, function(x) {
        param_names <- names(x)
        # print(param_names)
        if ("href" %in% param_names)
          return(grepl("netlify", x))
        if ("src" %in% param_names)
          return(grepl("netlify", x))
        return(F)
      }) %>% unlist(.) %>% sum(.) > 0
    }),
    #Determine if the site relies on gtm assets
    on_gtm = purrr::map_lgl(.x = scripts_links,  ~ {
      lapply(.x, function(x) {
        param_names <- names(x)
        # print(param_names)
        if ("href" %in% param_names)
          return(grepl("googletagmanager", x))
        if ("src" %in% param_names)
          return(grepl("googletagmanager", x))
        return(F)
      }) %>% unlist(.) %>% sum(.) > 0
    }),
    #Determine if the site relies on google analytics assets
    on_ga = purrr::map_lgl(.x = scripts_links,  ~ {
      lapply(.x, function(x) {
        param_names <- names(x)
        # print(param_names)
        if ("href" %in% param_names)
          return(grepl("googleanalytics|google-analytics", x))
        if ("src" %in% param_names)
          return(grepl("googleanalytics|google-analytics", x))
        return(F)
      }) %>% unlist(.) %>% sum(.) > 0
    }),
    #Determine if the site relies on universal analytics google analytics assets
    on_ua = purrr::map2_lgl(.x = scripts_links, .y = on_gtm,  ~ {
      if (.y == T)
        lapply(.x, function(x) {
          param_names <- names(x)
          # print(param_names)
          # print(x)
          if ("href" %in% param_names)
            return(grepl("UA-(.*)", x, fixed = F))
          if ("src" %in% param_names)
            return(grepl("UA-(.*)", x, fixed = F))
          return(F)
        }) %>% unlist(.) %>% sum(.) > 0
      else
        return(F)
    }),
    #Determine if the site includes GTM or GA assets
    on_gtm_ga = on_gtm | on_ga,
    #Determine if the site includes GA4 assets
    on_ga4 = purrr::map2_lgl(.x = scripts_links, .y = on_gtm_ga,  ~ {
      if (.y)
        lapply(.x, function(x) {
          param_names <- names(x)
          # print(param_names)
          if ("href" %in% param_names)
            return(grepl("G-(.*)", x))
          if ("src" %in% param_names)
            return(grepl("G-(.*)", x))
          return(F)
        }) %>% unlist(.) %>% sum(.) > 0
      else
        return(F)
    }),
    #Determine if the site relies on bold assets
    on_bold = purrr::map2_lgl(.x = scripts_links, .y = on_gtm_ga,  ~ {
      if (.y)
        lapply(.x, function(x) {
          param_names <- names(x)
          # print(param_names)
          if ("href" %in% param_names)
            return(grepl("boldapps", x))
          if ("src" %in% param_names)
            return(grepl("boldapps", x))
          return(F)
        }) %>% unlist(.) %>% sum(.) > 0
      else
        return(F)
    }),
  )
end<-Sys.time()
end-start
#Save the development algorithms into an intermediate dataset 
saveRDS(object = smbs_dev,file = paste0(here::here('data'),"/smbs_dev_",format(Sys.time(),'%Y%m%d'),".RDS"))
str(smbs_dev)
# 
# smbs_dev <-
#   readRDS(file = paste0(
#     here::here('data'),
#     '/smbs_dev_',
#     format(Sys.time(), "%Y%m%d"),
#     '.RDS'
#   ))

purrr::map_chr(smbs_dev$Name, as.character)
table(smbs$`Link to Website`) %>% .[order(., decreasing = T)] %>% .[. ==1] %>% length(.)
table(smbs_dev$main_operating) %>% .[order(., decreasing = T)] 
sum(smbs_dev$is_shopify)
smbs_dev%>%
  dplyr::filter(main_operating)%>%
  dplyr::group_by(is_shopify,on_gtm_ga)%>%
  dplyr::summarise(not_on_ga = n())
require(rvest)
smbs_dev %>%
  dplyr::filter(main_operating) %>%
  select(
    Name,
    `Link to Website`,
    Category,
    scripts_links,
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
# tidyr::unnest(scripts_links)%>%
# tidyr::unnest(scripts_links)
smbs_dev %>%
  dplyr::filter(main_operating) %>%
  select(Name, `Link to Website`)

