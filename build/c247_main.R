#The purpose of this job is to scrape canada247's explore pages in their
#entirety to gather a general dataset of canada's companies and some of their available
#information on this source
require(rjson)
source(here::here("src", "canada247", "c247_source.R"))
source(here::here("src", "canada247", "c247_ontario.R"))
