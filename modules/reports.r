

# reports -----------------------------------------------------
# not important. 
output$reportTable <-  renderFormattable({
  if ("frequency" %in% colnames(values$net$x$nodes)) {
  # browser()
    values$net$x$nodes %>%
      transmute(label, frequency = replace_na(frequency, 0), from = replace_na(from.frequency), to = replace_na(to.frequency)) %>%
      arrange(desc(frequency)) %>%
      formattable(list(
        `frequency` = color_bar("#AAEEAA"),
        `from` = color_bar("#EEAAAA"),
        `to` = color_bar("#AAAAEE")
      ))
  } else {
    tibble(warning = "You have to merge arrows with variables in order to get a report.") %>%
      
      formattable(list())
  }
})
# output$reportTable2 <- renderFormattable({
#     req(values$net$x$edges) %>% group_by(domain) %>% summarize(valence=round(mean(attributionValence),2)) %>% 
#       arrange(desc(valence)) %>%
#       formattable(list(
#         `valence` = color_bar("#EEAAAA")
#       ))
# })

output$reportTable2 <- renderFormattable({
# browser()
  values$edges %>% group_by(domain) %>% 
    mutate(positive=attributionValence>0,negative=attributionValence<=0) %>% 
    summarise(positive=sum(positive),negative=sum(negative)) %>% 
      arrange(desc(positive)) %>%
    formattable(
      align=c("r","r","l"),
      list(
      `positive` = color_bar("#AAEEAA"),
      `negative` = color_bar("#EEAAAA")
    ))
})

output$reportTable3 <- renderFormattable({
values$nodes %>% 
    group_by(cluster) %>% 
    summarize(N=n()) %>% 
    filter(!is.na(cluster)) %>% 
    arrange(desc(N)) %>%
    formattable(
      align=c("r","r","l"),
      list(
      `N` = color_bar("#AAEEAA"),
      `negative` = color_bar("#EEAAAA")
    ))
})


output$reportTable4 <- renderFormattable({
  values$edges %>% 
    group_by(domain) %>% 
    filter(attributionValence>0) %>% 
    group_by(attributionExplicit) %>% 
    select(domain) %>% 
    rownames_to_column() %>%  
    mutate(x=1) %>% 
    spread(key=attributionExplicit,value=x) %>% 
    select(-rowname) %>% 
    group_by(domain) %>% 
    summarise(positive_explicit=sum(`3`,na.rm=T),positive_implicit=sum(`2`,na.rm=T),positive_other=sum(`1`,na.rm=T)) %>%
    formattable(
      align=c("r"),
      list(area(T,2:4)~ color_bar("#EEAAEE")
    ))
})

output$reportTable5 <- renderFormattable({
  values$edges %>% 
    group_by(domain) %>% 
    filter(attributionValence<=0) %>% 
    group_by(attributionExplicit) %>% 
    select(domain) %>% 
    rownames_to_column() %>%  
    mutate(x=1) %>% 
    spread(key=attributionExplicit,value=x) %>% 
    select(-rowname) %>% 
    group_by(domain) %>% 
    summarise(negative_explicit=sum(`3`,na.rm=T),negative_implicit=sum(`2`,na.rm=T),negative_other=sum(`1`,na.rm=T)) %>%
    formattable(
      align=c("r"),
      list(area(T,2:4)~ color_bar("#EEAAAA")
        
    ))
})


output$reportPlot1 <- renderPlot({
  values$grafMerged %>% nodes_as_tibble() %>% 
    select(Education=`What is the education of the main respondent?`,attributionValence) %>% 
    ggplot(aes(attributionValence,Education))+
    geom_point()+
    geom_smooth()
})

