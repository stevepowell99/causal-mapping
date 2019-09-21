output$statSelector=renderUI({
  selectInput("statSelect","Select predictor",choices=xc("auto_group Name.of.kebele"))
})


output$stats <- renderFormattable(if(input$sides=="Display"){
  values$netCoding$x$edges %>% 
    group_by(UQ(sym(input$statSelect))) %>% 
    select_if(is.numeric) %>% 
    select(-c(from,to,statement_id,package,N,packageNote,id)) %>% 
    # select(one_of(colnames_for_mean)) %>% 
    mutate(N=n()) %>% 
    select_if(~sum(!is.na(.)) > 0) %>% 
    summarize_all(function(x)round(mean(x,na.rm=T),2))  %>% 
    # t %>% 
  formattable(list(
    align=c("r")
    # `attribution` = color_bar("#AAEEAA"),
    # `attributionValence` = color_bar("#AAEEAA"),
    # `Negative` = color_bar("#EEAAAA")
  ))
})

output$statsSigs=renderFormattable({
  
ve <- values$netCoding$x$edges
ve[,c(19,20,35,41,40,36,63,64,65,67)] %>% select(-attributionValence) %>%
  map(~ lm(attributionValence~.x,data=ve) %>% summary %>%  `[`("adj.r.squared") %>% unlist %>% round(2)) %>% as.tibble  %>% formattable
})
