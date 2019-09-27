
# widgets to edit and delete selected nodes and edges. not complete ----
#

output$varForm <- renderUI({
  if (length(req(valuesCoding$nodesSelected)) > 0) {
    # browser()
    df <- values$graf %>%
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
  vg <- values$graf %>%
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
  
  values$graf <- vg
})

observeEvent(input$deleteVarForm, {
  # browser()
  whichtarg <- values$graf %>%
    nodes_as_tibble() %>%
    mutate(id = row_number()) %>%
    filter(id %in% valuesCoding$nodesSelected) %>%
    pull(id)
  
  values$graf <- values$graf %>%
    activate(nodes) %>%
    filter(!(row_number() %in% whichtarg))
})

