output$infoBar <- renderUI({
  if(input$sides=="Display"){
    req(values$netCoding)
    tagList(
      
      glue("Original. Rows: {nrow(values$netCoding$x$nodes)}, Edges: {nrow(values$netCoding$x$edges)}, Edges in filter: {valuesCoding$filterVec %>% sum}, Statements: {values$netCoding$x$edges$statement_id %>% unique %>% length}, Statements in filter: {values$net$x$edges$statement_id %>% unique() %>% str_split(',') %>% unlist %>% unique %>% length}") %>% p()
    )
  }
  # p("asdf")
})
