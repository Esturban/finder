# test_c247 <-readRDS(here::here("data", "canada247", "explore_details_1_to_40000.RDS"))
test_c247<-lapply(list.files(here::here('data','canada247'),full.names = T,pattern = "explore_details_*"),readRDS)%>%dplyr::bind_rows(.)
test_c247[grepl("Vet",test_c247$categories),]%>%
  select(categories,website,facebook,linkedin,company_name)%>%
  tidyr::unnest(website)%>%print(n=30)

test_c247%>%
  tidyr::unnest(website)%>%
  dplyr::filter(!grepl("youtube",website))%>%
  dplyr::pull(website)%>%
  unique(.)%>%
  length(.)

test_c247%>%
  select(categories,website,facebook,linkedin,company_name)%>%
  unique(.)%>%
  dplyr::mutate(unq_cats = purrr::map(.x = categories,~{unlist(strsplit(.x,";"))}),
                unq_cats_cnt = purrr::map_dbl(.x = unq_cats,~{length(.x)}))%>%
  dplyr::select(company_name,unq_cats,unq_cats_cnt)%>%
  dplyr::arrange(desc(unq_cats_cnt))%>%
  head(.)%>%
  tidyr::unnest(unq_cats)%>%
  print(n = 100)
  
test_c247[grepl("Pet",test_c247$company_name),]%>%
  select(categories,website,facebook,linkedin,company_name)%>%
  unique(.)%>%
  dplyr::mutate(unq_cats = purrr::map(.x = categories,~{unlist(strsplit(.x,";"))}),
                unq_cats_cnt = purrr::map_dbl(.x = unq_cats,~{length(.x)}))%>%
  dplyr::select(company_name,unq_cats,unq_cats_cnt)%>%
  dplyr::arrange(desc(unq_cats_cnt))%>%
  head(.)%>%
  tidyr::unnest(unq_cats)%>%
  print(n = 100)
  
test_c247 %>% dplyr::mutate(n_websites = purrr::map_int(website,  ~ {
  length(.x)
})) %>% dplyr::filter(
  n_websites >= 2,
  # path == "https://canada247.info/explore/canada/ontario/middlesex_county/london/hart-direction-519-630-1400.html"
) %>% tidyr::unnest(email) %>% pull(email)%>%unique(.)->c247_emails

test_c247 %>% dplyr::mutate(n_websites = purrr::map_int(website,  ~ {
  length(.x)
})) %>% dplyr::filter(
  n_websites >= 2,
  # path == "https://canada247.info/explore/canada/ontario/middlesex_county/london/hart-direction-519-630-1400.html"
) %>% tidyr::unnest(website) %>% pull(website)

table(test_c247$categories)%>%.[order(.,decreasing = T)]%>%head(30)
test_c247%>%
  dplyr::filter(categories=="Point of interest;Establishment;Local government office")
