node_attrs<-function(x,selected = "head > link, head > script",condition = T){
  if (condition){
    tryCatch({x %>% html_nodes(selected) %>% html_attrs(.) %>% return(.)},error=function(e){
      print(condition)
      print(x)
      print(e)
      return(NULL)
    })
}  else
    return(NULL)
}