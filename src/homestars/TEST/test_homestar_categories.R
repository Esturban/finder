last_max <- 0

for (i in 1:100)
{
  if (test_it)
  {
    # browser()
    #Random test link to see what the page outcomes look like in UI vs scrape.
    city_cats[sample.int(nrow(all_city_cats), 1), 'Link'] -> test_link
    #print the testable link
    print(test_link)
    #Scrape the testable link
    test_link %>% read_html -> page_test
    #Unlink the test page
    unlink(test_link)
    
    #Gather the results of the scraped page
    page_test %>% html_nodes(
      "#search-page > div.search-page__head-container > div.search-page__result-links > div"
    ) %>% html_attr("data-react-props") %>% rjson::fromJSON(.) %>% .[['totalHits']] %>%
      as.numeric(.) ->
      test_number
    #Find the max in 100 iterations of 1890 categories
    if (last_max < test_number){
      
      last_max <- test_number
    }    
    page_test %>% html_nodes("#results-sponsors > section > section > div") %>%
      html_attr("data-react-props") %>% lapply(., rjson::fromJSON)%>%unique(.) -> test_page_1
    class(test_page_1)
    if (last_max == test_number)largest_page<-test_page_1
    print(sapply(test_page_1,function(x)x[['company']][['direct_phone']]))
  }
}


