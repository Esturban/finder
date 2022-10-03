# nat_eda.R
require(apexcharter)
require(tidyverse)

# str(smbs_dev)

# 
# smbs_dev%>%

library(apexcharter)
# data("mpg", package = "ggplot2")
require(microbenchmark)
smb_categories<-tapply(smbs_dev$Category,smbs_dev$Category, length)%>%.[order(.,decreasing = T)]%>%head(30)
smbs_dev_operating<-smbs_dev%>%dplyr::filter(main_operating)
smbs_dev_gtm<-smbs_dev%>%dplyr::filter(on_gtm_ga)
smb_operating<-tapply(smbs_dev$main_operating,smbs_dev$main_operating, length)%>%.[order(.,decreasing = T)]
smb_ga<-tapply(smbs_dev$on_gtm_ga,smbs_dev$on_gtm_ga, length)%>%.[order(.,decreasing = T)]
smb_ga4<-tapply(smbs_dev_gtm$on_ga4,smbs_dev_gtm$on_ga4, length)%>%.[order(.,decreasing = T)]
smb_op_shopify<-tapply(smbs_dev_operating$is_shopify,smbs_dev_operating$is_shopify, length)%>%.[order(.,decreasing = T)]
smb_op_square<-tapply(smbs_dev_operating$is_squarespace,smbs_dev_operating$is_squarespace, length)%>%.[order(.,decreasing = T)]
smb_op_wp<-tapply(smbs_dev_operating$is_wordpress,smbs_dev_operating$is_wordpress, length)%>%.[order(.,decreasing = T)]
# Construct 3 charts
a_categories <- apexchart() %>% 
  ax_chart(type = "bar") %>% 
  ax_plotOptions(bar = bar_opts(
    horizontal = TRUE,
    endingShape = "flat",
    columnWidth = "70%",
    dataLabels = list(
      position = "top"
    ))
  ) %>% 
  ax_grid(
    show = TRUE,
    position = "front",
    borderColor = "#FFF"
  ) %>% 
  ax_series(list(
    name = "Count",
    data = smb_categories
  )) %>% 
  ax_colors("#112446") %>% 
  ax_xaxis(categories = names(smb_categories))%>%
  # ax_xaxis(categories = unique(smbs_dev$Category))%>%
  ax_title(text = "Number of Businesses by Category") %>% 
  ax_subtitle(text = "Source: Not Amazon Aggregator") 

a_operating<-apexchart() %>%
  ax_series2(smb_operating)%>% 
  ax_chart(type = "donut")%>% 
  ax_plotOptions(
    pie = pie_opts(
      donut = list(size = "50%", background = "#BABABA")
    ))%>%
  ax_labels2(
    unique(smb_operating)
  )%>%  
  ax_title(text = "% Operating") %>% 
  ax_subtitle(text = "Source: Not Amazon Aggregator") 



a_shopify<-apexchart() %>%
  ax_series2(smb_op_shopify)%>% 
  ax_chart(type = "donut")%>% 
  ax_plotOptions(
    pie = pie_opts(
      donut = list(size = "50%", background = "#BABABA")
    ))%>%
  ax_labels2(
    smb_op_shopify%>%unique(.)
  )%>%  
  ax_title(text = "% Shopify") #%>% 
# ax_subtitle(text = "Source: EV Advisory") 



a_square<-apexchart() %>%
  ax_series2(smb_op_square)%>% 
  ax_chart(type = "donut")%>% 
  ax_plotOptions(
    pie = pie_opts(
      donut = list(size = "50%", background = "#BABABA")
    ))%>%
  ax_labels2(
    smb_op_square%>%unique(.)
  )%>%  
  ax_title(text = "% Squarespace")#%>% 
# ax_subtitle(text = "Source: EV Advisory") 


a_wp<-apexchart() %>%
  ax_series2(smb_op_wp)%>% 
  ax_chart(type = "donut")%>% 
  ax_plotOptions(
    pie = pie_opts(
      donut = list(size = "50%", background = "#BABABA")
    ))%>%
  ax_labels2(
    smb_op_wp%>%unique(.)
  )%>%  
  ax_title(text = "% Wordpress") #%>% 
# ax_subtitle(text = "Source: EV Advisory") 


a_ga<-apexchart() %>%
  ax_series2(smb_ga)%>% 
  ax_chart(type = "donut")%>% 
  ax_plotOptions(
    pie = pie_opts(
      donut = list(size = "50%", background = "#BABABA")
    ))%>%
  ax_labels2(
    smb_ga%>%unique(.)
  )%>%  
  ax_title(text = "% w/ GA") #%>% 
# ax_subtitle(text = "Source: EV Advisory") 


a_ga4<-apexchart() %>%
  ax_series2(smb_ga4)%>% 
  ax_chart(type = "donut")%>% 
  ax_plotOptions(
    pie = pie_opts(
      donut = list(size = "50%", background = "#BABABA")
    ))%>%
  ax_labels2(
    smb_ga4%>%unique(.)
  )%>%  
  ax_title(text = "% w/ GA4") #%>% 
  # ax_subtitle(text = "Source: EV Advisory") 


names(smbs_dev)

# a2 <- apex(smbs_dev, aes(main_operating), type = "column")
a3 <- apex(mpg, aes(drv), type = "pie")
a3 <- apex(mpg, aes(drv), type = "pie")

# Assemble them in a grid
apex_grid(
  a_categories, a_operating,a_ga,a_ga4, a_shopify,a_square,a_wp, 
  grid_area = c("1 / 1 / 6 / 4", "1 / 4 / 3 / 6", "3 / 4 / 4 / 5", "3 / 5 / 4 / 6", "4 / 4 / 5 / 5","4 / 5 / 5 / 6","5 / 4 / 6 / 5"),
  ncol = 5, 
  nrow = 5,
  height = "600px"
)
 