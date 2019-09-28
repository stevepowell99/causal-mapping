output$combineLink <- renderUI({
  ins <- valuesCoding$nodesSelected
  vfs <- valuesCoding$fromStack
  tagList(
    if (0 < length(vfs)) {
      div(paste0(
        paste0("From: ", vfs %>% paste0(collapse = ", ")),
        if (!is.null(ins)) paste0(", To? ", ins[[1]])
      ), style = "display:inline-block")
    }
  )
})

observeEvent(input$recodeButton, {
  # browser()
  vfs <- req(valuesCoding$fromStack) %>% as.numeric()
  ins <- req(valuesCoding$nodesSelected[[1]]) %>% as.numeric()
  
  vpag <- values$pag
  iot <- input$onlyThisStatement
  
  ved <- values$rawGraf %>%
    edges_as_tibble()
  
  
  ved <- ved %>%
    mutate(thisStatement = (ved$statement_id == vpag | !iot)) %>%
    mutate(from = ifelse(thisStatement & from %in% vfs, ins, from)) %>%
    mutate(to = ifelse(thisStatement & to %in% vfs, ins, to))
  
  values$rawGraf <-
    tbl_graph(values$rawGraf %>% nodes_as_tibble(), ved) # kinda stupid not to use tidygraph functions
  
  
  delay(1000, refresh_and_filter_net(values$rawGraf, vpag, iot))
  doNotification("Recoded variable(s)", 9)
  valuesCoding$nodesSelected <- NULL
  valuesCoding$edgesSelected <- NULL
  valuesCoding$fromStack <- NULL
})

