#The page_dl function takes a URL and attempts to download the contents of the provided URL 
#by first checking that the link is a complete URL, then to determine whether the page's 
#content can be downloaded
#
#The resulting output is a 3 variable named list:
#
# page = HTML page (if exists)  
# check = error codes whether it was successful or not  
# error = message for the error (if exists)

page_dl <- function(url,
                    character_min = 5,
                    skip = F,
                    verbose = T,
                    seconds_elapsed = 10) {
  if (nchar(gsub("https://|https://|[[:blank:]]", "", url)) > character_min)
    tryCatch({
      #From the R.utils package, include the timeout of the function to 
      #avoid ongoing hanging when the site cannot be resolved from
      # the original call for the data
      R.utils::withTimeout( {
        page <- rvest::read_html(url)
        unlink(url)
        list(page = page, check = 0)
      },
        timeout = seconds_elapsed)
      
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
        R.utils::withTimeout( {
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
        }},
        timeout = seconds_elapsed)
      }, error = function(err)
      {
        #Return a NULL if missing, checked in the next step
        unlink(url)
        if (verbose)
          print(paste0("Failed: ", url))
        if (verbose)
          print(err)
        return(list(
          page = NULL,
          check = -1,
          err = err
        ))
      },
      TimeoutException = function(ex) cat("[Skipped due to timeout]\n")))
    },
    TimeoutException = function(ex) cat("[Skipped due to timeout]\n"))
  else
    return(list(
      page = NULL,
      check = -1,
      err = "Too few characters"
    ))
}