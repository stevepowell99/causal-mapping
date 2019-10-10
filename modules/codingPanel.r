

# output$highlightedTextGo <- renderUI({
#       values$highlightedText <- input$highlightedText
#   if(""!=(req(values$highlightedText))){
#     actionButton("highlightedTextGoButton","Add text to quote")
#     }
# })
# 


observeEvent(input$highlightedText, {
  if (input$highlightedText != ""){
    delay(1500,updateTextAreaInput(session, "quote", value = input$highlightedText))
    # input$highlightedText <- ""
    # values$highlightedText <- ""
    
}
  })



observeEvent(req(input$onlyThisStatement),{
  if(input$onlyThisStatement) enable("pager")
  if(!input$onlyThisStatement) disable("pager")
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
  
  
  req(values$rawGraf)
  
  valuesCoding$last=edges_as_tibble(values$rawGraf) %>% filter(quote!="") %>% pull(statement_id) %>% max %>% as.numeric
  # {
  # updatePageruiInput(session, "pager", page_current = valuesCoding$last+1)
}))

# Pager----

#   the pager allows user to view interview statements one by one
output$pagerBig <- renderUI({
  
  # browser()
  # req(input$pager__page_current)
  # pag <- input$pager__page_current_vars()
  # current <- req(valuesCoding$last)+1
  tagList(
    div(
      pageruiInput("pager", pages_total = valuesCoding$tot,page_current = 1),
      style = "display:inline-block;"
    ),
    div(
      actionButton("firstuncoded", icon("fast-forward")),
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
      div(actionButton("overview_col", label = icon("book-reader")), style = "display:inline-block"), #   if one interview source has made more than one statement, show all of them
      div(materialSwitch("onlyThisStatement", label = icon("bullseye"), value = T), class="myelement",STYLE="padding:3px;background-color:#cceeaa;WIDTH:70PX"), #   if one interview source has made more than one statement, show all of them
      div(materialSwitch("showStatementInfo", label = icon("info"), value = F), class="myelement",STYLE="padding:3px;background-color:#aaeecc;WIDTH:70PX"), #   if one interview source has made more than one statement, show all of them
      style = "display:inline-block;margin-left:10px"
    )
  )
})

output$statementInfo=renderFormattable({
  pag <- req(input$pager__page_current)
  if(!is.null(values$statements_extra)) values$statements_extra %>% filter(statement_id==pag) %>% select(-1) %>% formattable
  
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
          valuesCoding$obsList[[btName]] <- observeEvent(input[[btName]], {
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
  
  quote <- req(values$rawGraf) %>%
    edges_as_tibble() %>%
    filter(statement_id == values$pag) %>%
    pull(quote) %>%
    replace_na("")
  
  if(length(quote)==0) quote <- ""
  
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
  # slist <- values$statements %>%
  #   filter(text != "") %>%
  #   pull(statement_id) %>%
  #   na.omit() %>%
  #   as.numeric() %>%
  #   sort()
  # 
  # elist <- edges_as_tibble(values$rawGraf)$statement_id %>%
  #   na.omit() %>%
  #   as.numeric()
  # 
  # min <- slist[sapply(slist, function(y) !(y %in% elist))] %>% min()
  # 
  # if (is.infinite(min)) {
  #   doNotification("You have coded everything!", level = 2)
  # } else
  
  last=edges_as_tibble(values$rawGraf) %>% filter(quote!="") %>% pull(statement_id) %>% max %>% as.numeric
    # {
    updatePageruiInput(session, "pager", page_current = last+1)
  # }
})




output$quotesOutput <- renderUI({
  ins <- valuesCoding$nodesSelected
  inse <- valuesCoding$edgesSelected
  if (!is.null(ins)) {
    quotes <- values$codeNet$x$nodes %>%
      filter(id %in% ins) %>%
      pull(quote) %>%
      str_remove_all(",NA") %>%
      paste0(collapse = "; ")
  }
  
  else if (!is.null(inse)) {
    quotes <- values$codeNet$x$edges %>%
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
