library(rvest)
library(tidyverse)
library(httr)
library(jsonlite)
library(RSelenium)

# Load the proxy list data from the API and combine them into a single data frame
ip_proxies_p1 <- fromJSON("https://proxylist.geonode.com/api/proxy-list?limit=500&page=1&sort_by=lastChecked&sort_type=desc")[['data']]
ip_proxies_p2 <- fromJSON("https://proxylist.geonode.com/api/proxy-list?limit=500&page=2&sort_by=lastChecked&sort_type=desc")[['data']]
ip_proxies_p3 <- fromJSON("https://proxylist.geonode.com/api/proxy-list?limit=500&page=3&sort_by=lastChecked&sort_type=desc")[['data']]
ip_proxies <- unique(rbind(ip_proxies_p1, ip_proxies_p2, ip_proxies_p3))
http_ip_proxies <- filter(ip_proxies, protocols == "http")

# Loop through the file indices from 1 to 8481
for (i in 1:8481) {
  destfile <- paste0("data/myipms/", i, ".html")
  
  # Check if the file already exists and is larger than 50KB, then skip the download
  if (file.exists(destfile) && file.size(destfile) > 50 * 1024) {
    message(paste("File", destfile, "already exists and is greater than 50KB. Skipping download."))
    next
  }
  
  attempts <- 0
  # Retry the download until successful or reached the maximum number of attempts
  repeat {
    Sys.sleep(5)
    
    # Randomly select a proxy from the list
    proxy_item <- http_ip_proxies[sample.int(nrow(http_ip_proxies), 1),]
    proxy <- paste0(proxy_item[['ip']], ":", proxy_item[['port']])
    
    print(proxy_item %>% tibble(.))
    
    # Set the Chrome browser capabilities with the selected proxy
    chrome_capabilities <- list(
      chromeOptions = list(
        args = c(
          "--proxy-server=http://%s" %>% sprintf(proxy),
          "--no-sandbox",
          "--disable-dev-shm-usage"
        )
      )
    )
    
    # Start the RSelenium driver with the specified capabilities
    driver <- rsDriver(browser = "chrome", port = 4567L, extraCapabilities = chrome_capabilities)
    remote_driver <- driver[["client"]]
    
    # Navigate to the desired URL
    url <- paste0("https://myip.ms/browse/sites/", i, "/ipID/23.227.38.0/ipIDii/23.227.38.255/sort/2/asc/1#sites_tbl_top")
    remote_driver$navigate(url)
    
    # Check for the presence of a CAPTCHA and click the submit button if found
    captcha_submit <- remote_driver$findElements(using = "css selector", "#captcha_submit")
    if (length(captcha_submit) > 0) {
      captcha_submit[[1]]$clickElement()
      message("Button clicked.")
    } else {
      message("Button not found.")
    }
    
    # Get the page source and write it to the destination file
    html_content <- remote_driver$getPageSource()[[1]]
    write(html_content, destfile)
    
    # Check if the downloaded file is larger than 50KB and exit the loop if successful
    if (file.size(destfile) > 50 * 1024) {
      message(paste("File", destfile, "downloaded successfully."))
      break
    } else {
      # Increment the attempts counter and display a message
      attempts <- attempts + 1
      message(paste("File", destfile, "is not greater than 50KB or there was an error. Retrying download. (Attempt:", attempts, ")"))
      
      # If the maximum number of attempts is reached, display a message and move on to the next file
      if (attempts >= 10) {
        message(paste("Reached 10 download attempts for file", destfile, ". Moving on to the next file."))
        break
      }
    }
    
    # Close the remote driver and stop the server
    remote_driver$close()
    driver[["server"]]$stop()
  }
}
