

observeEvent(input$highlightedText, {
  if (!is.null(input$highlightedText)) values$highlightedText <- input$highlightedText
  if (input$sides == "Code") updateTextAreaInput(session, "quote", value = values$highlightedText)
})



# create valuesCoding$tot -------------------------------------------------------

#   just the total number of statements, to provide the maximum value of the pager
observe(({
  if (!is.null(values$statements)) {
    # tot=nrow(values$statements)
    #
    # if (!("statement" %in% colnames(values$statments))) {
    #   values$statements <- values$statements %>% mutate(statement = row_number())
    # }
    tot <- values$statements %>%
      pull(statement_id) %>%
      na.omit() %>%
      as.numeric() %>%
      max()
    valuesCoding$tot <- tot
    # browser()
  } # if(!is.na(tot)) if(tot>1) browser()
  else {
    valuesCoding$tot <- 9
  }
}))

# Pager----

#   the pager allows user to view interview statements one by one
output$pagerBig <- renderUI({
  tagList(
    div(
      pageruiInput("pager", pages_total = valuesCoding$tot),
      style = "display:inline-block;"
    ),
    div(
      actionButton("firstuncoded", "First uncoded"),
      style = "display:inline-block;"
    ),
    div(
      # hilarious widget which scrolls through statements one by one
      if (F) {
        sliderInput(
          "timeslider",
          NULL,
          min = 0,
          max = valuesCoding$tot,
          value = 0,
          animate = animationOptions(loop = T),
          ticks = F,
          width = "50px"
        )
      },
      style = "display:inline-block;"
    ),
    div(
      # if("source" %in% colnames(values$statements))
      div(actionButton("resetSelection", label = NULL, icon = icon("refresh")), style = "position:absolute;right:35px") %>% bs_embed_tooltip("Reset the view for this statement"),
      div(actionButton("overview_col", label = "Read more"), style = "display:inline-block"), #   if one interview source has made more than one statement, show all of them
      div(checkboxInput("onlyThisStatement", label = "Only this", value = T), style = "display:inline-block"), #   if one interview source has made more than one statement, show all of them
      style = "display:inline-block;margin-left:20px"
    )
  )
})






observeEvent(input$overview_col, {
  vs <- values$statements
  # browser()
  
  # col=findset("diagramoverview_column")
  if (!("source_id" %in% colnames(vs))) {
    vs$source_id <- 1
  }
  
  vs <- vs %>%
    mutate(newSource = ifelse(source_id != lag(source_id), source_id, "")) %>%
    mutate(newSource = ifelse(is.na(newSource), source_id, newSource))
  
  pointer <- vs$source_id[req(values$pag)]
  content <- vs$statement_id[(vs$source_id == pointer)]
  # content=vs$statement[]
  
  showModal(modalDialog(
    title = "All statements", footer = NULL, easyClose = T, size = "l",
    tagList(
      # checkboxInput("showAllSources","Show all sources"),
      lapply(seq_along(content), function(y) if (!is.na(vs$text[[y]])) {
        x <- content[[y]]
        
        btName <- paste0("gosource", x)
        
        if (T) {
          # make sure to use <<- to update global variable obsList
          values$obsList[[btName]] <- observeEvent(input[[btName]], {
            updatePageruiInput(session, "pager", page_current = as.numeric(x))
            removeModal(session = getDefaultReactiveDomain())
          })
        }
        # browser()
        tagList(
          if (vs$newSource[y] == "") div() else div(h3(paste0("Source: ", pointer))),
          div(h4(paste0("Statement: ", vs$statement_id[x]))),
          # actionButton(paste0("gosource",x),"Go!"),
          if (str_detect(vs$text[x], "pdf$")) {
            tags$iframe(src = "pdf.pdf", style = "height:800px; width:100%;scrolling=yes")
          } else {
            div(id = paste0("allStatements-", y), actionLink(btName, label = vs$text[x]))
          },
          br()
        )
      })
    )
  ))
}, ignoreInit = T)

# display statements one by one ----

output$displayStatementPanel <- renderUI({
  # browser()
  
  quote <- req(values$graf) %>%
    edges_as_tibble() %>%
    filter(statement_id == values$pag) %>%
    pull(quote) %>%
    replace_na("")
  
  
  tagList(
    icon("quote-left"),
    values$statements$text[values$statements$statement_id == values$pag] %>%
      highlight_text(quote) %>%
      HTML() %>% div(class = "textbanner", id = "textbanner"),
    # span(values$statements$text[values$statements$statement==values$pag], class = "textbanner", id = "textbanner"),
    # span(add_highlight((ve$quote[ve$statement==values$pag])[1],(values$statements$text[values$statements$statement==values$pag])), class = "textbanner", id = "textbanner"),
    hr()
  )
})

# observeEvent({c(input$pager,input$timeslider)},{
observeEvent({
  c(input$pager)
}, {
  if (!is.null(input$pager)) {
    values$pag <- input$pager[[1]]
    
    # if (input$timeslider > 0 & nrow(values$statements) > 2) updatePageruiInput(session, "pager", page_current = input$timeslider)
    if (!is.null(input$pager) && !is.null(input$quote)) updateTextAreaInput(session = session, inputId = "quote", value = "", placeholder = "quote")
  }
})

observeEvent(input$firstuncoded, {
  # browser()
  slist <- values$statements %>%
    filter(text != "") %>%
    pull(statement_id) %>%
    na.omit() %>%
    as.numeric() %>%
    sort()
  
  elist <- edges_as_tibble(values$graf)$statement_id %>%
    na.omit() %>%
    as.numeric()
  
  min <- slist[sapply(slist, function(y) !(y %in% elist))] %>% min()
  
  if (is.infinite(min)) {
    doNotification("You have coded everything!", level = 2)
  } else {
    updatePageruiInput(session, "pager", page_current = min)
  }
})




output$quotesOutput <- renderUI({
  ins <- valuesCoding$nodesSelected
  inse <- valuesCoding$edgesSelected
  if (!is.null(ins)) {
    quotes <- values$netCode$x$nodes %>%
      filter(id %in% ins) %>%
      pull(quote) %>%
      str_remove_all(",NA") %>%
      paste0(collapse = "; ")
  }
  
  else if (!is.null(inse)) {
    quotes <- values$netCode$x$edges %>%
      filter(id %in% ins) %>%
      pull(quote) %>%
      str_remove_all(",NA") %>%
      paste0(collapse = "; ")
  }
  else {
    quotes <- "Click on a variable or arrow to see the quotes"
  }
  
  tagList(
    icon("quote-left"),
    quotes %>%
      HTML() %>% div(class = "quotes")
  )
})