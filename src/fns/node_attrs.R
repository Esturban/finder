node_attrs<-function(x,selected = "head > link, head > script",condition = T){
  if (condition)
    x %>% html_nodes(selected) %>% html_attrs(.) %>% return(.)
  else
    return(NULL)
}