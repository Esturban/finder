# Canada 247 info
c(
  "https://canada247.info/explore",
  paste0("https://canada247.info/explore/page/", 1:48492)
) -> canada247_links

start<-Sys.time()
test_df_list<-lapply(canada247_links[1:50],function(x){
  
  x %>% read_html(.) -> html_page
  
  html_page %>% 
    html_nodes("h3 a") %>% 
    html_attr("href") -> c247_path
  html_page %>% 
    html_nodes("h3 a") %>% 
    html_text(.) -> company_name
  html_page %>% 
    html_nodes("#left-content h3+ div") %>% 
    html_text(.) %>% 
    gsub("(?<=[a-z])[[:blank:]](?=[A-Z])", ";", ., perl = T) -> categories
  
  
  html_page %>% 
    html_nodes("#left-content strong") %>% 
    html_text(.)->phone
  
  html_page %>% 
    html_nodes(".place-address") %>% 
    html_text(.)%>%
    gsub("Address:  ","",.)->address
  
  tibble(link = x,
         path = c247_path,
         company_name = company_name,
         categories = categories,
         phone = phone,
         address = address)
  
})

c247_df<-dplyr::bind_rows(test_df_list)

# require(dialr)
# get_carrier(phone(c247_df$phone[sample.int(length(c247_df$phone),1)],"CA"))
# phone(c247_df$phone[sample.int(length(c247_df$phone),1)],"CA")
# get_example("CA",type = "MOBILE")
end<-Sys.time()
end-start