find_deps<-function(attrs,sources,dep_keys){
  lapply(attrs, function(x) {
    param_names <- names(x)
    if (sum(sources %in% param_names)>0)
      return(grepl(paste0(dep_keys,collapse = "|"), x))
    return(F)
  }) %>% unlist(.) %>% sum(.) > 0
}