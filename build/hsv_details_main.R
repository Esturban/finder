# hsv_details

#HSV = Homestars Validation of Company details 
# This job is used to scrape the category pages from homestars 
# and download the company information that is available publicly 
# through a genetic algorithm that estimates how many additional pages
# exist for a corresponding category
source(here::here("setup.R"), F)

hs_load <-F
hs_tform <-F
sapply(list.files(here::here("src","fns"),full.names = T),source,local = F)

source(here::here("src","homestars","category_companies.R"),local = F)