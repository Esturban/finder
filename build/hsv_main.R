# homestars_category_validation
source(here::here("setup.R"), F)

# The purpose of this build job is to find all of the corresponding categories from
# homestars for each of the corresponding cities ( 4000+ cities with 189 categories) 
# and checking if the sites are valid or simply categories that are populated 
# 
# Testing and checking ~ 800k links with httr package
hs_load <-F
hs_tform <-F
sapply(list.files(here::here("src","fns"),full.names = T),source,local = F)

hs_load <-
  tryCatch({
    source(here::here("src","homestars","homestar_categories.R"), F)
    T
  },
  error = function(e) {
    print(e)
    return(F)
  })

if (hs_load)
  hs_tform<-tryCatch({
    source(here::here("src","homestars","category_validation.R"), F)
    T
  }, error = function(e) {
    print(e)
    return(F)
  })
