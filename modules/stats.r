output$stats <- renderFormattable(if(input$sides=="Display"){
  values$netCoding$x$edges %>% 
    group_by(statement_group) %>% 
    select(one_of(colnames_for_mean)) %>% 
    mutate(N=n()) %>% 
    summarize_all(mean) %>% 
    formattable()
})
