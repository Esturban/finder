# Downloading the registered skilled trades in Ontario
# Another reference table notable for a later date

#Test
# "https://www.skilledtradesontario.ca/about-trades/trades-information" %>%
#   read_html %>% 
#   html_nodes('th , td') %>% 
#   html_text -> skilled_trades_table

#Create all of the page links
c("https://www.skilledtradesontario.ca/about-trades/trades-information",
  paste0("https://www.skilledtradesontario.ca/about-trades/trades-information/page/",1:10))->all_trades_pages

if(!file.exists(here::here("data","ref","skilled_trades.csv")))
{#For each of the page links...
lapply(all_trades_pages,function(x){
  
  #Get the text elements in the table available
  x %>%
    read_html %>% html_nodes('th , td') %>% html_text -> skilled_trades_table
  
  #Separately, collect the URL reference of the skilled trades to find the information
  x%>%
    read_html %>% html_nodes('#search-advisory-results > div > table > tbody > tr > td:nth-child(6) > a')%>%html_attr("href") -> skilled_trades_url
  
  #Get the contents of the page arrays and dump it into a data frame
  data.frame(
    Trade_Name = skilled_trades_table[seq(7, length(skilled_trades_table) - 5, by = 6)],
    Code = skilled_trades_table[seq(8, length(skilled_trades_table) - 4, by = 6)],
    Classification = skilled_trades_table[seq(9, length(skilled_trades_table) - 3, by = 6)],
    Exam = skilled_trades_table[seq(10, length(skilled_trades_table) - 2, by = 6)],
    Red_Seal = skilled_trades_table[seq(11, length(skilled_trades_table) - 1, by = 6)],
    Trade_Details = skilled_trades_table[seq(12, length(skilled_trades_table), by = 6)],
    Trade_Link = skilled_trades_url
  )
  })%>%dplyr::bind_rows(.)->all_skilled_trades_data
  #Save the skilled trades as a csv file
  readr::write_csv(all_skilled_trades_data,file = here::here("data","ref","skilled_trades.csv"))
}
