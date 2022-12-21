#From the page attributes, determine if the corresponding social source exists
#
#If the item exists, then find the source and remove parts of the stop content
#in an unclear sense

social_user <-
  function(x,
           social = "instagram",
           verbose = F,
           stopwords = character(0),
           social_min_char = 2) {
    lapply(x, function(iter) {
      #If the `href` attribute exists in the list of attributes
      if ("href" %in% names(iter)) {
        if (grepl(social, iter[["href"]])) {
          return(iter[["href"]])
        } else {
          return(NULL)
        }
      }
      #If the `src` attribute exists in the list of attributes
      if ("src" %in% names(iter)) {
        if (grepl(social, iter[["src"]])) {
          return(iter[["src"]])
        } else {
          return(NULL)
        }
      }
    }) %>%
      #Remove all of the empty elements
      purrr::compact(.) %>%
      unlist(.) %>%
      gsub("/p/\\w+|scontent\\w+|/reel/\\w+|more.ctv\\w+", "", .) %>%
      #If there are no stopwords, then pass the link through as normal
      {
        ifelse(length(stopwords) >= 1 , gsub(paste0(stopwords, collapse = "|"), "", .), .)
      } %>%
      #The text must have a minimum of 2 characters
      .[nchar(.) > social_min_char] %>%
      unique(.) -> res
    if (verbose == T)
      print(res)
    
    if (identical(res, character(0)))
      return(NA_character_)
    res
  }