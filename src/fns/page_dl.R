#' Download page content from URL
#'
#' This function accepts a URL and attempts to download the contents of the provided URL by first checking that the link is a complete URL, then determining whether the page's content can be downloaded. The resulting output is a 3-variable named list: the HTML page (if it exists), error codes indicating whether the download was successful, and an error message (if applicable).
#'
#' @param url A character string specifying the URL to download.
#' @param character_min An integer specifying the minimum number of characters required in the URL for the download to be attempted.
#' @param skip A logical value indicating whether to skip the download if it is unsuccessful.
#' @param verbose A logical value indicating whether to print verbose output during the download process.
#' @param seconds_elapsed An integer specifying the number of seconds to wait before timing out the download process.
#'
#' @return A list containing the HTML page (if it exists), error codes indicating whether the download was successful, and an error message (if applicable).
#'
#' @examples
#' page_dl(url = "https://www.example.com",
#'         character_min = 5,
#'         skip = FALSE,
#'         verbose = TRUE,
#'         seconds_elapsed = 10)
#'
#' @export
page_dl <- function(url,
                    character_min = 5,
                    skip = FALSE,
                    verbose = TRUE,
                    seconds_elapsed = 10) {
  if (nchar(gsub("https://|https://|[[:blank:]]", "", url)) > character_min) {
    tryCatch({
      # From the R.utils package, include the timeout of the function to
      # avoid ongoing hanging when the site cannot be resolved from
      # the original call for the data
      R.utils::withTimeout({
        page <- rvest::read_html(url)
        unlink(url)
        list(page = page, check = 0)
      },
      timeout = seconds_elapsed)
    },
    error = function(e) {
      # Return NULL if missing, checked in the next step
      if (verbose)
        print(paste0("Failed: ", url))
      unlink(url)
      if (verbose)
        print(e)
      return(tryCatch({
        # Check if the source is simply not secure (http:// domain)
        # Selected option to skip the item.  If it's an amalgamated source, don't skip
        R.utils::withTimeout({
          if (!skip) {
            page_link <- paste0("http://", domain(url), "/")
            if (verbose)
              print(paste0("Trying unsecure link: ", page_link))
            page <- rvest::read_html(page_link)
            unlink(page_link)
            list(page = page,
                 check = 0,
                 err = e)
          } else {
            # If we are skipping the source, we'll explain a bit as to why it failed
            list(page = NULL,
                 check = -1,
                 err = e)
          }
        },
        timeout = seconds_elapsed)
      },
      error = function(err) {
        # Return NULL if missing, checked in the next step
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
      TimeoutException = function(ex)
        cat("[Skipped due to timeout]\n")))
    },
    TimeoutException = function(ex)
      cat("[Skipped due to timeout]\n"))
  } else
    return(list(
      page = NULL,
      check = -1,
      err = "Too few characters"
    ))
}