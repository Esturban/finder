# c247_df1<-readRDS(here::here("data","canada247","explore_companies_1_to_25000.RDS"))
# c247_df2<-readRDS(here::here("data","canada247","explore_companies_25000_to_end.RDS"))
# 
# c247_df<-rbind(c247_df1,c247_df2)%>%unique(.)
#
#Collect all of the companies for explore
c247_df<-readRDS(here::here("data","canada247","explore","explore_companies_all.RDS"))

#Determine how many elements of data are available from the explore pages
length(unique(c247_df$address))
length(unique(c247_df$phone))
length(unique(c247_df$address))
#Only for Ontario
# Around 330,000 unique ontario businesses
length(unique(c247_df$address[grepl(", ON",c247_df$address)])) 

#Based on the addresses, gather all of the ontario companies 
#with a distinct path to avoid redownloading the same information
#
#Total ~ 440k
c247_on<-c247_df%>% 
  dplyr::filter(grepl(", ON",.$address))%>%
  .[!duplicated(.$path),]

