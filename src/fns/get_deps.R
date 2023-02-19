#' Get dependencies from node attributes
#'
#' This function accepts a list of node attributes from an HTML file and returns any dependencies that exist within them, based on a list of specified keys.
#'
#' @param attrs A list of node attributes to search for dependencies.
#' @param sources A character vector of attribute names to check for dependency keys.
#' @param dep_keys A character vector of dependency keys to search for.
#'
#' @return A character vector of dependency names found in the input attributes.
#'
#' @examples
#' get_deps(attrs = list(id = "node1", class = "node-class", data = "node-data"),
#'          sources = c("id", "data"),
#'          dep_keys = c("d3", "D3", "d3.js", "D3.js"))
#'
#' @export
get_deps <- function(attrs, sources, dep_keys) {
  lapply(attrs, function(x) {
    param_names <- names(x)
    if (sum(sources %in% param_names) > 0) {
      return(x[grepl(paste0(dep_keys, collapse = "|"), x)])
    }
    return(NA_character_)
  }) %>% purrr::compact(.) %>% unlist(.) %>% unique(.) %>% .[1] %>% gsub(paste0(dep_keys, collapse = "|"), "", .) -> deps_res
  
  if (identical(deps_res, character(0))) {
    return(NA_character_)
  } else {
    return(deps_res)
  }
}
