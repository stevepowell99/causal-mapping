

# reports -----------------------------------------------------
# not important. 
output$reportTable <- renderFormattable({
  if ("frequency" %in% colnames(values$net$x$nodes)) {
    values$net$x$nodes %>%
      transmute(label, frequency = replace_na(frequency, 0), from = replace_na(from.frequency_sum, 0), to = replace_na(to.frequency_sum, 0)) %>%
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

