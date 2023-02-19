#' Extract node attributes from HTML file
#'
#' This function accepts an HTML file and a CSS selector, and returns a list of attributes for each node that matches the selector.
#'
#' @param x An HTML file.
#' @param selected A CSS selector to use to select nodes to extract attributes from.
#' @param condition A logical value indicating whether to apply error handling to the function.
#'
#' @return A list of attributes for each node that matches the selected CSS selector.
#'
#' @examples
#' node_attrs(x = read_html("<html><head><link rel='stylesheet' href='style.css'></head><body><p class='paragraph'>Hello, world!</p></body></html>"),
#'            selected = "head > link, head > script",
#'            condition = TRUE)
#'
#' @export
node_attrs <- function(x, selected = "head > link, head > script", condition = TRUE) {
  if (condition) {
    tryCatch({
      x %>% 
        html_nodes(selected) %>% 
        html_attrs(.) %>% 
        return(.)
    }, error = function(e) {
      print(condition)
      print(x)
      print(e)
      return(NULL)
    })
  } else {
    return(NULL)
  }
}
