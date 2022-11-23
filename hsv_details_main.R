# hsv_details
source(here::here("setup.R"), F)

hs_load <-F
hs_tform <-F
sapply(list.files(here::here("src","fns"),full.names = T),source,local = F)

source(here::here("src","homestars","category_companies.R"),local = F)