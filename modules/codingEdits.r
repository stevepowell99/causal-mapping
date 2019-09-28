
# widgets to edit and delete selected nodes and edges. not complete ----
#

output$varForm <- renderUI({
  if (length(req(valuesCoding$nodesSelected)) > 0) {
    # browser()
    df <- values$rawGraf %>%
      nodes_as_tibble() %>%
      mutate(id = row_number())
    
    rows <- df[valuesCoding$nodesSelected, ]
    div(tagList(
      if (length(valuesCoding$nodesSelected) > 0) {
        tagList(
          div(p("Edit: "), style = "display:inline-block;width:5%"),
          if (length(valuesCoding$nodesSelected) == 1) div(textInput("editVarFormText", NULL, placeholder = "label", value = df[valuesCoding$nodesSelected, "label"], width = "95%"), style = "display:inline-block;"),
          div(textInput("editdetails", NULL, placeholder = "details", value = ifelse(length(rows$details) > 1, "", rows$details), width = "95%"), style = "display:inline-block;"),
          div(textInput("editcluster", NULL, placeholder = "cluster", value = rows$cluster %>% paste0(collapse = ","), width = "95%"), style = "display:inline-block"),
          div(actionButton("editVarForm", "Save"), style = "display:inline-block;background-color:white;padding:10px;border-radius:5px"),
          div(actionLink("deleteVarForm", paste0("Delete: ", valuesCoding$nodesSelected %>% paste0(collapse = ";"), "?")), style = "padding:2px;display:inline-block;color:red")
        )
      },
      hr(style = "margin:2px")
    ), style = "background-color:#EEFFEE;padding:10px;border:1px green solid")
  }
})
observeEvent(input$editVarForm, {
  vg <- values$rawGraf %>%
    activate(nodes)
  
  
  
  if (input$editdetails != "") {
    vg <- vg %>%
      mutate(details = if_else(row_number() %in% valuesCoding$nodesSelected, input$editdetails, details)) # hmm what if details form is empty
  }
  
  if (length(valuesCoding$nodesSelected) == 1 && input$editVarFormText != "") {
    vg <- vg %>%
      mutate(label = if_else(row_number() %in% valuesCoding$nodesSelected, input$editVarFormText, label))
  }
  if (input$editcluster != "") {
    vg <- vg %>%
      mutate(cluster = if_else(row_number() %in% valuesCoding$nodesSelected, input$editcluster, cluster))
  }
  
  values$rawGraf <- vg
})

observeEvent(input$deleteVarForm, {
  # browser()
  whichtarg <- values$rawGraf %>%
    nodes_as_tibble() %>%
    mutate(id = row_number()) %>%
    filter(id %in% valuesCoding$nodesSelected) %>%
    pull(id)
  
  values$rawGraf <- values$rawGraf %>%
    activate(nodes) %>%
    filter(!(row_number() %in% whichtarg))
})

observeEvent(input$savePackage, {
  vg <- values$rawGraf %>%
    activate(edges)
  
  ise <- valuesCoding$edgesSelected
  
  
  if (input$package != "") {
    vg <- vg %>%
      mutate(package = if_else(row_number() %in% ise, input$package, package))
  }
  
  
  if (input$packageNote != "") {
    vg <- vg %>%
      mutate(packageNote = if_else(row_number() %in% ise, input$packageNote, packageNote))
  }
  
  
  if (input$quote != "") {
    vg <- vg %>%
      mutate(quote = if_else(row_number() %in% ise, input$quote, quote))
  }
  
  
  if (input$arrLabel != "") {
    vg <- vg %>%
      mutate(label = if_else(row_number() %in% ise, input$arrLabel, label))
  }
  
  values$rawGraf <- vg
  
  vpag <- values$pag
  iot <- input$onlyThisStatement
  delay(1000, refresh_and_filter_net(vg, vpag, iot))
})




observeEvent(input$deletePackage, {
  # browser()
  if (!is.null(valuesCoding$edgesSelected)) {
    values$rawGraf <- values$rawGraf %>%
      activate(edges) %>%
      mutate(id = row_number()) %>%
      filter(!(id %in% valuesCoding$edgesSelected)) %>%
      select(-id)
  }
})
