# nat_eda.R
smbs_dev <-
  readRDS(file = paste0(
    here::here('data'),
    '/smbs_dev_',
    format(Sys.time(), "%Y%m%d"),
    '.RDS'
  ))
smb_categories <-
  tapply(smbs_dev$Category, smbs_dev$Category, length) %>% .[order(., decreasing = T)] %>%
  head(30)
smbs_dev_operating <- smbs_dev %>% dplyr::filter(main_operating)
smbs_dev_gtm <- smbs_dev %>% dplyr::filter(on_gtm_ga)
smb_operating <-
  tapply(smbs_dev$main_operating, smbs_dev$main_operating, length) %>% .[order(., decreasing = F)]
smb_ga <-
  tapply(smbs_dev$on_gtm_ga, smbs_dev$on_gtm_ga, length) %>% .[order(., decreasing = T)]
smb_ga4 <-
  tapply(smbs_dev_gtm$on_ga4, smbs_dev_gtm$on_ga4, length) %>% .[order(., decreasing = T)]
smb_op_shopify <-
  tapply(smbs_dev_operating$is_shopify,
         smbs_dev_operating$is_shopify,
         length) %>% .[order(., decreasing = F)]
smb_op_square <-
  tapply(smbs_dev_operating$is_squarespace,
         smbs_dev_operating$is_squarespace,
         length) %>% .[order(., decreasing = T)]
smb_op_wp <-
  tapply(smbs_dev_operating$is_wordpress,
         smbs_dev_operating$is_wordpress,
         length) %>% .[order(., decreasing = F)]
smb_op_ig <-
  tapply(!is.na(smbs_dev_operating$instagram_user),
         !is.na(smbs_dev_operating$instagram_user),
         length) %>% .[order(., decreasing = F)]
smb_op_tw <-
  tapply(!is.na(smbs_dev_operating$twitter_user),
         !is.na(smbs_dev_operating$twitter_user),
         length) %>% .[order(., decreasing = F)]

smb_op_cf <-
  tapply(smbs_dev_operating$is_cloudflare,
         smbs_dev_operating$is_cloudflare,
         length) %>% .[order(., decreasing = F)]
# Construct 3 charts
a_categories <- apexchart() %>%
  ax_chart(type = "bar") %>%
  ax_plotOptions(bar = bar_opts(
    horizontal = TRUE,
    endingShape = "flat",
    columnWidth = "70%",
    dataLabels = list(position = "top")
  )) %>%
  ax_grid(show = TRUE,
          position = "front",
          borderColor = "#FFF") %>%
  ax_series(list(name = "Count",
                 data = smb_categories)) %>%
  ax_colors("#112446") %>%
  ax_xaxis(categories = names(smb_categories)) %>%
  # ax_xaxis(categories = unique(smbs_dev$Category))%>%
  ax_title(text = "Number of Businesses by Category") %>%
  ax_subtitle(text = "Source: Not Amazon Aggregator")

a_operating <- apexchart() %>%
  ax_series2(smb_operating) %>%
  ax_chart(type = "donut") %>%
  ax_plotOptions(pie = pie_opts(donut = list(size = "50%", background = "#BABABA"))) %>%
  ax_labels2(unique(names(smb_operating))) %>%
  ax_title(text = "% Operating") %>%
  ax_subtitle(text = "Source: Not Amazon Aggregator") %>%
  ax_legend(show = F)



a_shopify <- apexchart() %>%
  ax_series2(smb_op_shopify) %>%
  ax_chart(type = "donut") %>%
  ax_plotOptions(pie = pie_opts(donut = list(size = "50%", background = "#BABABA"))) %>%
  ax_labels2(names(smb_op_shopify) %>% unique(.)) %>%
  ax_title(text = "% Shopify") %>%
  ax_legend(show = F) #%>%
# ax_subtitle(text = "Source: EV Advisory")



a_square <- apexchart() %>%
  ax_series2(smb_op_square) %>%
  ax_chart(type = "donut") %>%
  ax_plotOptions(pie = pie_opts(donut = list(size = "50%", background = "#BABABA"))) %>%
  ax_labels2(names(smb_op_square) %>% unique(.)) %>%
  ax_title(text = "% Squarespace") %>%
  ax_legend(show = F)#%>%
# ax_subtitle(text = "Source: EV Advisory")


a_ig <- apexchart() %>%
  ax_series2(smb_op_ig) %>%
  ax_chart(type = "donut") %>%
  ax_plotOptions(pie = pie_opts(donut = list(size = "50%", background = "#BABABA"))) %>%
  ax_labels2(names(smb_op_ig) %>% unique(.)) %>%
  ax_title(text = "% Instagram Handles Found") %>%
  ax_legend(show = F) 

a_tw <- apexchart() %>%
  ax_series2(smb_op_tw) %>%
  ax_chart(type = "donut") %>%
  ax_plotOptions(pie = pie_opts(donut = list(size = "50%", background = "#BABABA"))) %>%
  ax_labels2(names(smb_op_tw) %>% unique(.)) %>%
  ax_title(text = "% Twitter Handles Found") %>%
  ax_legend(show = F) 

a_wp <- apexchart() %>%
  ax_series2(smb_op_wp) %>%
  ax_chart(type = "donut") %>%
  ax_plotOptions(pie = pie_opts(donut = list(size = "50%", background = "#BABABA"))) %>%
  ax_labels2(names(smb_op_wp) %>% unique(.)) %>%
  ax_title(text = "% Wordpress") %>%
  ax_legend(show = F) #%>%
# ax_subtitle(text = "Source: EV Advisory")
a_cf <- apexchart() %>%
  ax_series2(smb_op_cf) %>%
  ax_chart(type = "donut") %>%
  ax_plotOptions(pie = pie_opts(donut = list(size = "50%", background = "#BABABA"))) %>%
  ax_labels2(names(smb_op_cf) %>% unique(.)) %>%
  ax_title(text = "% Cloudflare") %>%
  ax_legend(show = F) #%>%
# ax_subtitle(text = "Source: EV Advisory")


a_ga <- apexchart() %>%
  ax_series2(smb_ga) %>%
  ax_chart(type = "donut") %>%
  ax_plotOptions(pie = pie_opts(donut = list(size = "50%", background = "#BABABA"))) %>%
  ax_labels2(names(smb_ga) %>% unique(.)) %>%
  ax_title(text = "% w/ GA") %>%
  ax_legend(show = F) #%>%
# ax_subtitle(text = "Source: EV Advisory")


a_ga4 <- apexchart() %>%
  ax_series2(smb_ga4) %>%
  ax_chart(type = "donut") %>%
  ax_plotOptions(pie = pie_opts(donut = list(size = "50%", background = "#BABABA"))) %>%
  ax_labels2(names(smb_ga4) %>% unique(.)) %>%
  ax_title(text = "% w/ GA4") %>%
  ax_legend(show = F)#%>%
# ax_subtitle(text = "Source: EV Advisory")


names(smbs_dev)

# Assemble them in a grid
apex_grid(
  a_categories,
  a_operating,
  a_ig,
  a_tw,
  a_ga,
  a_ga4,
  a_shopify,
  a_square,
  # a_wp,
  # a_cf,
  grid_area = c(
    "1 / 1 / 6 / 4",
    "1 / 4 / 3 / 6",
    "3 / 4 / 4 / 5",
    "3 / 5 / 4 / 6",
    "4 / 4 / 5 / 5",
    "4 / 5 / 5 / 6",
    "5 / 4 / 6 / 5",
    "5 / 5 / 6 / 6"
  ),
  ncol = 5,
  nrow = 5,
  height = "600px"
)

smbs_dev %>%
  select(Name, Category, instagram_user, twitter_user) %>%
  # .[sample.int(nrow(.),10),]%>%
  tidyr::unnest(instagram_user) %>%
  tidyr::unnest(twitter_user) %>%
  dplyr::tibble()

smbs_dev %>%
  group_by(!is.na(instagram_user)) %>%
  summarise(n = n())

smbs_dev %>%
  .[sample.int(nrow(.),1),]%>%
  dplyr::mutate(script = paste0("Hi ", gsub("The ","",Name),",  \n","I was on your website and wanted to learn a bit about your [PRODUCT DESCRIPTION]. "))%>%
  dplyr::select(Name, Category,script)
