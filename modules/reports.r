

# reports -----------------------------------------------------
# not important. 
output$reportTable <- renderFormattable({
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
output$reportTable2 <- renderFormattable({
    req(values$net$x$edges) %>% group_by(domain) %>% summarize(valence=round(mean(attributionValence),2)) %>% 
      arrange(desc(valence)) %>%
      formattable(list(
        `valence` = color_bar("#EEAAAA")
      ))
})

