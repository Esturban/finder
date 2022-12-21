#HSS = Homestars Services 
#
#This is the complete algorithm for the unique companies that 
# were scraped from the details pages that had a 200 status 
# and were succesfully scraped.  This job goes to each of the websites' 
# corresponding domains associated with the company and performs the scrape 
# needed to collect company information on the site such as their website 
# dependencies and the way in which they've made their website
source(here::here("src","homestars","services_algo.R"))