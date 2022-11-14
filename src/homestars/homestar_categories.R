# hs_categories
#Scraping all of the homestars categories

# sapply(list.files(here::here("src","fns"),full.names = T),source,local = F)
test_it <- F
#Gather all of the categories up front
"https://homestars.com/on/toronto/categories" %>% read_html -> all_cat_page
#Gather all of the toronto specifuc URLs
all_cat_page %>% html_nodes("body > div.single-column-page > div > div > ul > li > a") %>%
  html_attr("href") %>% paste0("https://homestars.com", .) -> toronto_urls

cat_suffix<-gsub("https://homestars.com/on/toronto","",toronto_urls)
#Get all of the named categories
all_cat_page %>% html_nodes(".category-group__category-name") %>% html_text ->
  all_cats
#Replace toronto with a sprintable text
cat_sprint_urls <- gsub("toronto", "%s", toronto_urls)


'https://homestars.com/on/cities'%>%read_html%>%html_nodes('.city__link')%>%html_attr("href")%>%paste0("https://homestars.com",.)%>%gsub("[[:blank:]]","%20",.)->city_links

#Corresponding cities in Ontario with whom to start
cities <-'https://homestars.com/on/cities'%>%read_html%>%html_nodes('.city__link')%>%html_attr("href")%>%gsub("/on/","",.)
# x<-1
#Create all of the city categories in a data frame
city_cats <- lapply(1:length(cities), function(x) {
   paste0(city_links[x],cat_suffix)-> cat_links
  data.frame(
    City = rep(cities[x], length(all_cats)),
    Category = all_cats,
    Link = cat_links,
    stringsAsFactors = F
  )
}) %>% dplyr::bind_rows(.)

# city_cats%>%dplyr::filter(City =='toronto')%>%readr::write_csv(x = .,file = "data/homestars/toronto_categories_sample.csv")
#Testing not enabled at this time
if(test_it)source(here::here("src","homestars","TEST","test_homestar_categories.R"),F)

