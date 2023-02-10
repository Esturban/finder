# Canada 247 info
# From scouring the explore pages, it has been determined that there are
# close to 50,000 pages of available businesses and their corresponding contacts
#with 20 on each page
require(tidyverse)
c(
  "https://canada247.info/explore",
  paste0("https://canada247.info/explore/page/", 1:48492)
) -> canada247_links
sapply(list.files(here::here("src", "fns"), full.names = T), source, local = F)

start<-Sys.time()
#From 1 to 25000
#Time: 11.7 hours
#From 25001 to end
#Time: 16.9 hours
#
if(!file.exists(here::here("data","canada247","explore","explore_companies_all.RDS"))){
df_list<-lapply(canada247_links,function(x){
  # browser()
  print(x)
  
  x %>% page_dl(.) -> html_page
  unlink(x)
  if(!is.null(html_page$page)){
  html_page$page %>% 
    html_nodes("h3 a") %>% 
    html_attr("href") -> c247_path
  html_page$page %>% 
    html_nodes("h3 a") %>% 
    html_text(.) -> company_name
  html_page$page %>% 
    html_nodes("#left-content h3+ div") %>% 
    html_text(.) %>% 
    gsub("(?<=[a-z])[[:blank:]](?=[A-Z])", ";", ., perl = T) -> categories
  
  
  html_page$page %>% 
    html_nodes(".explore-addr") %>% 
    html_text(.)%>%gsub("Phone: ","",.)->phone
  
  html_page$page %>% 
    html_nodes(".place-address") %>% 
    html_text(.)%>%
    gsub("Address:  ","",.)->address
  
  tibble(link = x,
         path = c247_path,
         company_name = company_name,
         categories = categories,
         phone = phone,
         address = address)}
  
})%>%purrr::compact(.)

c247_df<-dplyr::bind_rows(df_list)

saveRDS(c247_df,here::here("data","canada247","explore","explore_companies_all.RDS"))}
end<-Sys.time()
end-start
