get_deps <- function(attrs, sources, dep_keys) {
  lapply(attrs, function(x) {
    param_names <- names(x)
    if (sum(sources %in% param_names) > 0) {
      return(x[grepl(paste0(dep_keys, collapse = "|"), x)])
    }
    return(NA_character_)
  }) %>% purrr::compact(.) %>% unlist(.) %>% unique(.) %>% .[1] %>% gsub(paste0(dep_keys, collapse = "|"), "", .)->deps_res
  
  if(identical(deps_res,character(0)))return(NA_character_) else return(deps_res)
}