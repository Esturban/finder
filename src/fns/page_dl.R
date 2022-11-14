page_dl <- function(url,
                    character_min = 5,
                    skip = F,
                    verbose = T) {
  if (nchar(gsub("https://|https://|[[:blank:]]", "", url)) > character_min)
    tryCatch({
      page <- rvest::read_html(url)
      unlink(url)
      list(page = page, check = 0)
    }
    , error = function(e)
    {
      #Return a NULL if missing, checked in the next step
      if (verbose)
        print(paste0("Failed: ", url))
      unlink(url)
      if (verbose)
        print(e)
      return(tryCatch({
        # Check if the source is simply not secure (http:// domain)
        #Selected option to skip the item.  If it's an amalgamated source, don't skip
        if (!skip)
        {
          page_link <- paste0("http://", domain(url), "/")
          if (verbose)
            print(paste0("Trying unsecure link: ", page_link))
          page <- rvest::read_html(page_link)
          unlink(page_link)
          list(page = page,
               check = 0,
               err = e)
        } else{
          #If we are skipping the source, well explain a bit as to why it crapped out
          list(page = NULL,
               check = -1,
               err = e)
        }
      }, error = function(err)
      {
        #Return a NULL if missing, checked in the next step
        unlink(page_link)
        if (verbose)
          print(paste0("Failed: ", page_link))
        if (verbose)
          print(err)
        return(list(
          page = NULL,
          check = -1,
          err = err
        ))
      }))
    })
  else
    return(list(
      page = NULL,
      check = -1,
      err = "Too few characters"
    ))
}