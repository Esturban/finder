page_dl<-function(url){
  tryCatch({
    page <- rvest::read_html(url)
    unlink(url)
    page
  }
  , error = function(e)
  {
    #Return a NULL if missing, checked in the next step
    print(paste0("Failed: ", url))
    unlink(url)
    print(e)
    return(tryCatch({
      # Check if the source is simply not secure (http:// domain)
      page_link<-paste0("http://", domain(url), "/")
      print(paste0("Trying unsecure link: ",page_link))
      page <- rvest::read_html(page_link)
      unlink(page_link)
      page
    }, error = function(err)
    {
      #Return a NULL if missing, checked in the next step
      unlink(page_link)
      print(paste0("Failed: ", page_link))
      print(err)
      return(NULL)
    }))
  })
}