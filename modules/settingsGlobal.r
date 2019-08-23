# produce  settings widgets --------------------------------

observeEvent(input$settingsTableGlobalUp, {
  # browser()
  vals <- values$settingsGlobal
  vs <- values$settingsGlobal %>% mutate_all(as.character)
  output$inputs <- renderUI({
    lapply(1:nrow(vs), function(x) {
      row <- vs[x, ]
      rg <- replace_na(row$widget, "")
      rt <- paste0(row$type, row$setting, collapse = "")
      rt <- replace_na(rt, "")
      # if(rg=="color")  {checkboxInput("asdf","asdf")}
      div(
        if (rg == "color") {
          # browser()
          colourInput(paste0("input", rt), rt,
            palette = "limited",
            showColour = "background",
            value = if (is.null(findset(rt,vals))) findset(rt,vals) else findset(rt,values),
            allowedCols = allcols1
          )
        }
        else if (rg == "slider") {
          sliderInput(paste0("input", rt), rt, min = 0, max = 100, value = findset(rt,vals))
        }
        else if (rg == "checkbox") {
          # browser()
          checkboxInput(paste0("input", rt), rt, value = as.logical(findset(rt,vals)))
        }
        else
          if (rg == "input") paste0(row$type, row$setting, collapse = ""),
        style = "display:inline-block;"
      )
    })
  })
})


output$settingsTableGlobal <- renderRHandsontable({
  vs <- values$settingsGlobal %>% mutate_all(as.character)
  
  ds <- defaultSettingsGlobal %>% mutate_all(as.character)
  
  vs <- bind_rows(vs, ds) %>%
    distinct(type, setting, .keep_all = T)
  # browser()
  
  rhandsontable(vs %>%
      mutate(type = factor(type)), height = NULL, rowHeaders = FALSE, usetypes = T) %>%
    hot_context_menu(allowRowEdit = T) %>%
    hot_cols(colWidths = c(80, 120, 250, 80))
})


observeEvent(input$settingsTableGlobalUp, {
  doNotification("updating from settingsTableGlobal")
  # browser()
  values$settingsGlobal <- hot_to_r(input$settingsTableGlobal)
})


