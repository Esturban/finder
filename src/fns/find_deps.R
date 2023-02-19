#' Find dependencies within node attributes
#'
#' This function accepts a list of node attributes from an HTML file and determines if any dependencies exist within them, based on a list of specified keys.
#'
#' @param attrs A list of node attributes to search for dependencies.
#' @param sources A character vector of attribute names to check for dependency keys.
#' @param dep_keys A character vector of dependency keys to search for.
#'
#' @return A logical value indicating if any dependencies were found in the input attributes.
#'
#' @examples
#' find_deps(attrs = list(id = "node1", class = "node-class", data = "node-data"),
#'           sources = c("id", "data"),
#'           dep_keys = c("d3", "D3", "d3.js", "D3.js"))
#'
#' @export
find_deps <- function(attrs, sources, dep_keys) {
  lapply(attrs, function(x) {
    param_names <- names(x)
    if (sum(sources %in% param_names) > 0) {
      return(grepl(paste0(dep_keys, collapse = "|"), x))
    }
    return(FALSE)
  }) %>% unlist(.) %>% sum(.) > 0
}
