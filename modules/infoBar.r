output$infoBar <- renderUI({
  if(input$sides=="Display"){
    req(values$codeNet)
    tagList(
      
      glue("Original. Rows: {nrow(values$codeNet$x$nodes)}, Edges: {nrow(values$codeNet$x$edges)}, Edges in filter: {valuesCoding$filterVec %>% sum}, Statements: {values$codeNet$x$edges$statement_id %>% unique %>% length}, Statements in filter: {values$net$x$edges$statement_id %>% unique() %>% str_split(',') %>% unlist %>% unique %>% length}") %>% p()
    )
  }
  # p("asdf")
})
