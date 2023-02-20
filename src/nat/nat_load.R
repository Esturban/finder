#From the not amazon website, gather the API source to be able to recall the records
#Here, launch all of the records
nat_json<-jsonlite::fromJSON('https://not-amazon-to.pory.app/api/app/data/6009ce7d9f8f0300178ed10c/records?timezone=America%2FCoral_Harbour')

#Determine all of the elements of the records
#
# str(nat_json$records$fields)
# 'data.frame':	12 obs. of  5 variables:
#   $ Name           : chr  "RAVEN READS" "GLAD DAY BOOKSHOP" "TYPE BOOKS" "A DIFFERENT BOOK LIST" ...
# $ Category       : chr  "BOOKS" "BOOKS" "BOOKS" "BOOKS" ...
# $ City           : chr  "Toronto" "Toronto" "Toronto" "Toronto" ...
# $ Link to Website: chr  "https://ravenreads.org/" "https://www.gladdaybookshop.com/" "https://typebooks.ca/" "https://www.adifferentbooklist.com/" ...
# $ Photo          : chr  "Screen Shot 2020-12-14 at 8_15_45 AM.png (https://dl.airtable.com/.attachments/434331dfa4dec8c8bc173ca4f18e3baf"| __truncated__ "Screen Shot 2020-12-14 at 8_37_23 AM.png (https://dl.airtable.com/.attachments/c05f2be79ee5dcdc95799f964b714e9a"| __truncated__ "Screen Shot 2020-12-14 at 8_38_20 AM.png (https://dl.airtable.com/.attachments/b02ccf9fdcc00168d07d5434a1eaa0ca"| __truncated__ "Screen Shot 2020-12-14 at 8_38_54 AM.png (https://dl.airtable.com/.attachments/97380e0126092b046ad5eb2801c5f05f"| __truncated__ ...

#Here is the starting base URL of the api data from NAT
base_url<-'https://not-amazon-to.pory.app/api/app/data/6009ce7d9f8f0300178ed10c/records?timezone=America%2FCoral_Harbour'
#Collect the results from JSON
nat_data<-jsonlite::fromJSON(base_url)
#Save the dataset to a dataframe
end_df<-nat_data$records$fields
#Append the corresponding offset of the records to understand the origin of the fields to follow
end_df$offset<-nat_data$offset
end_df$last_update<-Sys.time()

for(i in 1:300){
  
  #Delay the next API call by 3 seconds
  Sys.sleep(3)
  #From the last dataset, get the offset value needed to collect the next batch of API data
  nat_offset<-nat_data$offset
  #Collect the JSON data from NAT for the next corresponding dataset
  nat_data<-jsonlite::fromJSON(paste0(base_url,'&offset=',nat_offset))
  #Save a temporary interim dataset with only the records' fields
  nat_df<-nat_data$records$fields
  #Append the offset values to the dataframe
  nat_df$offset<-nat_offset
  nat_df$last_update<-Sys.time()
  
  #Combine all of the records from the NAT source
  end_df<-dplyr::bind_rows(end_df,nat_df)
  
}

#Upon completion, save the full final DF as an object called SMBs
smbs<-end_df  
#Save the file into the data folder for access later on in the application
saveRDS(object = smbs,file = paste0(here::here('data',"nat",'raw'),'/smbs_',format(Sys.time(),"%Y%m%d"),'.RDS'))
