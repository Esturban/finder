social_user<-function(x,social = "instagram",verbose = F, stopwords){
  lapply(x, function(iter) {
    if ("href" %in% names(iter)) {
      # print(names(x))
      if (grepl(social, iter[["href"]]))
      {
        # print(x)
        return(iter[["href"]])}
      else
        return(NULL)
    }
  }) %>% 
    purrr::compact(.) %>% 
    unlist(.) %>% 
    gsub("/p/\\w+|scontent\\w+|/reel/\\w+|more.ctv\\w+","",.)%>%
    gsub(paste0(stopwords,collapse = "|"), "", .)%>%.[nchar(.)>2]%>%unique(.)->res
  if(verbose==T) print(res)
  
  if(identical(res, character(0)))return(NA_character_)
  res
}