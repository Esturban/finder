source(here::here("setup.R"), F)
require(googleCloudStorageR)
if(!interactive())gcs_auth(json_file = Sys.getenv("GCS_JSON")) else gcs_auth()
nat_load <-F
nat_tform <-F
nat_algo <-F
nat_eda <-F
sapply(list.files(here::here("src","fns"),full.names = T),source,local = F)

nat_load <-
  tryCatch({
    source(here::here("src","nat","nat_load.R"), F)
    T
  },
  error = function(e) {
    print(e)
    return(F)
  })

if (nat_load)
  nat_tform<-tryCatch({
    source(here::here("src","nat","nat.R"), F)
    T
  }, error = function(e) {
    print(e)
    return(F)
  })

if (nat_tform)
  nat_algo<-tryCatch({
    source(here::here("src","nat","nat_algo.R"), F)
    T
  }, error = function(e) {
    print(e)
    return(F)
  })

if (nat_algo)
  nat_eda<-tryCatch({
    source(here::here("src","nat","nat_eda.R"), F)
    T
  }, error = function(e) {
    print(e)
    return(F)
  })


if (nat_eda)
  nat_gcs<-tryCatch({
    source(here::here("src","jobs","gcs.R"), F)
    T
  }, error = function(e) {
    print(e)
    return(F)
  })