nullif<-function(item)ifelse(is.null(item),NA_character_,item)
nullifN<-function(item)ifelse(is.null(item),NA_real_,item)
nullifI<-function(item)ifelse(is.null(item),NA_integer_,item)