panel_ui<-function(id,...){
  ns<-NS(id)
  uiOutput(ns("panel-ui"),...)
}

panel_server<-function(input,output,session,...){
  #Get the session namespace
  session$ns -> ns
  
  #Collect all of the passed parameters
  dotlist <- list(...)
  
  #From all of the collected parameters...
  reactive_list <- setNames(lapply(dotlist, function(x) {
    # Ensure that the item, if multiple exist, has a numeric naming index
    if (is.null(names(x)))
      names(x) <- 1:length(x)
    
    #Convert the item to a reactive function
    out <- do.call(
      what = to_reactive,
      args = list(x),
      envir = globalenv()
    )
    return(out)
    
  }), nm = names(dotlist))# Export the items into a named list
  
  
  output[['panel-ui']] <- renderUI({
    #Ensure that the selection UI parameters
    req(exists("reactive_list"))
    
    #With do.call, convert all of the items into parameters that can be rendered as UI configurations
    do.call(shinyMobile::f7Panel, c(
      list(id = ns("panel")),
      lapply(reactive_list, function(x)
        isolate(x()))
    ), quote = T)
  })
  
  # return(list(selected = reactive(input[['select']])))
}