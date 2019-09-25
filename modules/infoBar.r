output$infoBar <- renderUI({
  if(input$sides=="Display"){
    req(values$netCode)
    tagList(
      
      glue("Original. Rows: {nrow(values$netCode$x$nodes)}, Edges: {nrow(values$netCode$x$edges)}, Edges in filter: {valuesCoding$filterVec %>% sum}, Statements: {values$netCode$x$edges$statement_id %>% unique %>% length}, Statements in filter: {values$net$x$edges$statement_id %>% unique() %>% str_split(',') %>% unlist %>% unique %>% length}") %>% p()
    )
  }
  # p("asdf")
})
