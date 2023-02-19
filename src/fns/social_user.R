#' Determine if a social media source exists
#'
#' This function takes in page attributes and determines if the corresponding social media source exists. If the source exists, it is cleaned up by removing parts of the stop content in an unclear sense. The resulting output is the cleaned-up social media link.
#'
#' @param x A list of page attributes.
#' @param social A character string specifying the social media platform to search for.
#' @param verbose A logical value indicating whether to print verbose output during the function's execution.
#' @param stopwords A character vector specifying stop content to remove from the social media link.
#' @param social_min_char An integer specifying the minimum number of characters required in the social media link.
#'
#' @return A cleaned-up social media link if one exists, otherwise `NA_character_`.
#'
#' @examples
#' social_user(x = list(href = "https://www.instagram.com/example/",
#'                      src = "https://www.example.com/assets/images/instagram.png"),
#'             social = "instagram",
#'             verbose = FALSE,
#'             stopwords = character(0),
#'             social_min_char = 2)
#'
#' @export
social_user <-
  function(x,
           social = "instagram",
           verbose = FALSE,
           stopwords = character(0),
           social_min_char = 2) {
    lapply(x, function(iter) {
      # If the `href` attribute exists in the list of attributes
      if ("href" %in% names(iter)) {
        if (grepl(social, iter[["href"]])) {
          return(iter[["href"]])
        } else {
          return(NULL)
        }
      }
      # If the `src` attribute exists in the list of attributes
      if ("src" %in% names(iter)) {
        if (grepl(social, iter[["src"]])) {
          return(iter[["src"]])
        } else {
          return(NULL)
        }
      }
    }) %>%
      # Remove all of the empty elements
      purrr::compact(.) %>%
      unlist(.) %>%
      gsub("/p/\\w+|scontent\\w+|/reel/\\w+|more.ctv\\w+", "", .) %>%
      # If there are no stopwords, then pass the link through as normal
      {
        ifelse(length(stopwords) >= 1 , gsub(paste0(stopwords, collapse = "|"), "", .), .)
      } %>%
      # The text must have a minimum of 2 characters
      .[nchar(.) > social_min_char] %>%
      unique(.) -> res
    if (verbose == TRUE)
      print(res)
    
    if (identical(res, character(0)))
      return(NA_character_)
    res
  }
