# homestars_category_validation
source(here::here("setup.R"), F)

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
