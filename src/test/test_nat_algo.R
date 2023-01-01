#test_nat_algo


#Load up yesterday's data

# if(interactive())smbs <-
#   readRDS(file = paste0(
#     here::here('data'),
#     '/smbs_tformed_',
#     format(Sys.time()-86400, "%Y%m%d"),
#     '.RDS'
#   ))


#See the sites that are operating and their social node attributes
#OLD

# smbs_dev2%>%
#   dplyr::filter(main_operating)%>%
#   .[sample.int(nrow(.), 20), ]%>%
#   dplyr::tibble(.)%>%
#   # dplyr::select(4)%>%
#   dplyr::mutate(social = map(.x = main_page,~{.x%>%html_nodes("a")%>%html_attrs(.)}))


#Testing out the collection of similar html attributes
# "https://themakestation.ca/" %>% read_html %>% html_nodes("a") %>% html_attrs(.) %>%
#   lapply(., function(x) {
#     if ("href" %in% names(x)) {
#       # print(names(x))
#       if (grepl("instagram", x[["href"]]))
#         return(x[["href"]])
#       else
#         return(NULL)
#     }
#   }) %>% purrr::compact(.) %>% unlist(.)%>% 
#   gsub("/p/\\w+","",.)%>%
#   gsub(paste0(ig_stopwords,collapse = "|"), "", .)%>%.[nchar(.)>0]%>%unique(.)

#See all of the existing names of an array as a character
purrr::map_chr(smbs_dev$Name, as.character)

#Determine if there are websites with duplicates
table(smbs$`Link to Website`) %>% .[order(., decreasing = T)] %>% .[. == 1] %>% length(.)
#Determine how many websites are operating
table(smbs_dev$main_operating) %>% .[order(., decreasing = T)]
#How many websites are currently hosted on shopify
sum(smbs_dev$is_shopify)

#Determine how many sites do and do not have gtm and are hosted on shopify
smbs_dev %>%
  dplyr::filter(main_operating) %>%
  dplyr::group_by(is_shopify, on_gtm_ga) %>%
  dplyr::summarise(not_on_ga = n())


require(rvest)
# smbs_dev %>%
#   dplyr::filter(main_operating) %>%
#   select(
#     Name,
#     `Link to Website`,
#     Category,
#     header_links,
#     is_shopify,
#     is_squarespace,
#     is_bookmanager,
#     is_wordpress,
#     on_gtm_ga,
#     on_ua,
#     on_ga4,
#     on_bold
#   ) %>%
#   tibble()#%>%
# tidyr::unnest(header_links)%>%
# tidyr::unnest(header_links)
# smbs_dev %>%
#   # dplyr::filter(main_operating) %>%
#   select(Name, `Link to Website`)
