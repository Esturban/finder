require(magrittr)
require(here)
require(googleCloudStorageR)

gcs_auth()
## get bucket info
bucket <- Sys.getenv("GCS_BUCKET")
bucket_info <- gcs_get_bucket(bucket)
# bucket_info
gcs_get_bucket(bucket)
#This action takes a file location on the local computer
#First loop = Go through the parent folder
parent_folders <- Sys.getenv("PF")
start<-Sys.time()
lapply(parent_folders, function(pf) {
  #Select the parent folder
  gcs_list_objects(bucket = bucket)$name->gcs_objects
  # pf<-parent_folders[3]
  #Second loop = Go through the child folder
  child_folders <-
    list.dirs(here::here('data', pf), full.names = F) %>% 
    .[nchar(.) > 0]
  lapply(child_folders, function(cf) {
    #Select the child folder and all of the corresponding RDS files
    file_names <-
      list.files(here::here('data', pf, cf), pattern = "*.RDS|*.csv")
    local_file_names <-
      list.files(here::here('data', pf, cf),
                 pattern = "*.RDS|*.csv",
                 full.names = T)
    #Create the folder path location for the CS instance
    gcs_folder <- file.path('data','websites', pf, cf)
    #Third loop = Go through the file names and
    lapply(1:length(file_names), function(f) {
      gcs_item_name <- file.path(gcs_folder, file_names[f])
      if(!(gcs_item_name%in%gcs_objects))
      gcs_upload(file = local_file_names[f],
                 name = gcs_item_name,
                 bucket = bucket)
    })
  })
})
end<-Sys.time()
end-start

