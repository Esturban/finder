# Load required libraries
require(magrittr) # For piping %>%
require(here) # For constructing file paths
require(googleCloudStorageR) # For interacting with Google Cloud Storage

# Authenticate with Google Cloud Storage
gcs_auth()

# Get the bucket information
bucket <- Sys.getenv("GCS_BUCKET")
bucket_info <- gcs_get_bucket(bucket)
# Print bucket_info to console (commented out)
# bucket_info

# Get the list of objects in the bucket
gcs_get_bucket(bucket)

# Define the parent folders to be processed
parent_folders <- Sys.getenv("PF")

# Record the start time
start <- Sys.time()

# Loop through each parent folder (first loop)
lapply(parent_folders, function(pf) {
  # Get the list of objects in the bucket
  gcs_list_objects(bucket = bucket)$name -> gcs_objects
  
  # Loop through each child folder in the parent folder (second loop)
  child_folders <-
    list.dirs(here::here('data', pf), full.names = F) %>% .[nchar(.) > 0]
  lapply(child_folders, function(cf) {
    # Get the list of RDS and CSV files in the child folder
    file_names <-
      list.files(here::here('data', pf, cf), pattern = "*.RDS|*.csv")
    local_file_names <-
      list.files(here::here('data', pf, cf),
                 pattern = "*.RDS|*.csv",
                 full.names = T)
    
    # Define the folder path location for the Google Cloud Storage instance
    gcs_folder <- file.path('data', 'websites', pf, cf)
    
    # Loop through each file in the child folder (third loop)
    lapply(1:length(file_names), function(f) {
      # Construct the full path to the file in the Google Cloud Storage
      gcs_item_name <- file.path(gcs_folder, file_names[f])
      
      # Check if the file does not exist in the bucket and upload it
      if (!(gcs_item_name %in% gcs_objects)) {
        gcs_upload(file = local_file_names[f],
                   name = gcs_item_name,
                   bucket = bucket)
      }
    })
  })
})

# Record the end time
end <- Sys.time()

# Calculate and display the time taken to process the files
end - start
