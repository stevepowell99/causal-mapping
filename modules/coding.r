# Code panel -----------------------------------------------------------

# observing edge and node selections --------------------------------------

# this is necessary because you can't programmatically deselect, you have to actually click. so store active node / edge in valuesCoding$nodesSelected instead, and nullify it manually when neccessary
values$highlightedText <- "" # part of a system to copy any text highlighted with mouse in browser i.e. from interview quotes and insert into the edge information
values$pag <- 1 # stores value of pager in Code panel

observeEvent(input$net_selected, {
  # browser()
  valuesCoding$nodesSelected <- input$net_selected
})
observeEvent(input$net_selectedEdges, {
  # browser()
  valuesCoding$edgesSelected <- input$net_selectedEdges
})


observe({
  if (is.null(input$net_selected)) valuesCoding$nodesSelected <- NULL
}) # this is necessary when user clicks on canvas to deselect a node


observeEvent(input$net_selectedEdges, {
  valuesCoding$edgesSelected <- input$net_selectedEdges
})




observeEvent(input$highlightedText, {
  if (!is.null(input$highlightedText)) values$highlightedText <- input$highlightedText
  if (input$sides == "Code") updateTextAreaInput(session, "quote", value = values$highlightedText)
})



valuesCoding <- reactiveValues(fromStack = NULL, toStack = NULL, foundIDs = NULL, readyForEndArrow = F, nodeSelected = NULL, edgeSelected = NULL)
# i added an extra reactive variable because you said not to have them all in one :-)    Not sure if they need splitting up more

# This keeps a record of the previous page, and so ensures that we only update
# the visNetwork if we transition to/from the 'Code' tab.
#
# You can change when updates happen by either updating the conditions here,
# or adding dependencies further down, like I have for tab$change
tab <- reactiveValues(old = "", change = 0)
observeEvent(input$sides, {
  if (tab$old == "Code" ) {
    tab$change <- tab$change + 1
  } else if (input$sides == "Code") {
    tab$change <- tab$change + 1
  }
  tab$old <- input$sides
})

# create values$tot -------------------------------------------------------

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
    values$tot <- tot
    # browser()
  } # if(!is.na(tot)) if(tot>1) browser()
  else {
    values$tot <- 9
  }
}))

# Pager----

#   the pager allows user to view interview statements one by one
output$pagerBig <- renderUI({
  tagList(
    div(
      pageruiInput("pager", pages_total = values$tot),
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
          max = values$tot,
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

values$obsList <- list() #   to show all the statemtns from one source


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
  
  quote <- values$codingGraf %>%
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
  
  elist <- edges_as_tibble(values$codingGraf)$statement_id %>%
    na.omit() %>%
    as.numeric()
  
  min <- slist[sapply(slist, function(y) !(y %in% elist))] %>% min()
  
  if (is.infinite(min)) {
    doNotification("You have coded everything!", level = 2)
  } else {
    updatePageruiInput(session, "pager", page_current = min)
  }
})




observeEvent(input$updateE2, {
  selectizeInput(session, "new1_edge", selected = input$new2_edge)
})

observeEvent(input$updateE3, {
  updatePageruiInput(session, inputId = "pager", page_current = as.numeric(values$pag) + 1)
})



# fromStack display

output$fromStackInfo <- renderUI({
})


output$combineLink <- renderUI({
  ins <- valuesCoding$nodesSelected
  vfs <- valuesCoding$fromStack
  tagList(
    if (0 < length(vfs)) {
      div(paste0(
        paste0("From: ", vfs %>% paste0(collapse = ", ")),
        if (!is.null(ins)) paste0(", To? ", ins[[1]])
      ), style = "display:inline-block")
    }
    #     ,
    #
    #        if(0<length(vfs) & !is.null(ins)){
    #          if(vfs!=ins){
    #       div(actionLink("combineLink",label = "Recode ?"),style="display:inline-block")
    # }  }
  )
})

observeEvent(input$recodeButton, {
  # browser()
  vfs <- req(valuesCoding$fromStack) %>% as.numeric()
  ins <- req(valuesCoding$nodesSelected[[1]]) %>% as.numeric()
  
  vpag <- values$pag
  iot <- input$onlyThisStatement
  
  ved <- values$codingGraf %>%
    edges_as_tibble()
  
  
  ved <- ved %>%
    mutate(thisStatement = (ved$statement_id == vpag | !iot)) %>%
    mutate(from = ifelse(thisStatement & from %in% vfs, ins, from)) %>%
    mutate(to = ifelse(thisStatement & to %in% vfs, ins, to))
  
  values$codingGraf <-
    tbl_graph(values$codingGraf %>% nodes_as_tibble(), ved) # kinda stupid not to use tidygraph functions
  
  
  delay(1000, refresh_and_filter_net(values$codingGraf, vpag, iot))
  doNotification("Recoded variable(s)", 9)
  valuesCoding$nodesSelected <- NULL
  valuesCoding$edgesSelected <- NULL
  valuesCoding$fromStack <- NULL
})


# Add edges widget----

output$add_edges_widget <- renderUI({
  # varlist=values$codingGraf %>% nodes_as_tibble() %>% pull(label) %>% unique() %>% as.character()
  # varlist <- na.omit(varlist)
  
  df <- values$codingGraf %>%
    edges_as_tibble() %>%
    mutate(id = row_number())
  
  ise <- valuesCoding$edgesSelected
  row <- df[ise, ]
  isrow <- !is.null(ise)
  tagList(
    div(
      
      div(if (isrow) {
        ise %>%
          paste0(collapse = "; ") %>%
          as.character() %>%
          paste0("Edit arrow(s): ", .) %>%
          tagList(
            actionLink("deletePackage", " (Delete?)"),
            actionLink("savePackage", " (Save?)")
          )
      }),
      
      
      div(
        div(textAreaInput("quote", NULL, value = ifelse(ise, row$quote, ""), placeholder = "quote", rows = 3, width = "100%"), style = "") %>%
          bs_embed_tooltip(title = if (T) ("If you select text in the Statement panel above using your mouse, it will appear here. You can also edit this text.")),
        style = "margin-top:20px"
      ),
      awesomeCheckbox("edgeDetails", "Details", value = F),
      
      conditionalPanel(
        "input.edgeDetails",
        # open="Details",
        tagList(
          div(
            div(textInput("arrLabel", NULL, value = ifelse(ise, row$label, ""), placeholder = "label"), style = "display:inline-block;"),
            div(style = "display:inline-block;width:5%"),
            div(selectizeInput("definition.type", NULL, choices = c("", "Defined, directed", "Defined, undirected")), style = "display:inline-block;width:20%"),
            div(selectizeInput("function.type", NULL, choices = c("+", "-", "NECC", "SUFF")), style = "display:inline-block;width:20%"),
            div(textInput("package", NULL, value = ifelse(ise, row$package, ""), placeholder = "package"), style = "display:inline-block;"),
            div(textInput("packageNote", NULL, value = ifelse(ise, row$packageNote, ""), placeholder = "packageNote"), style = "display:inline-block;")
          ),
          
          div(
            id = "sliders",
            div(style = "display:inline-block;width:5%"),
            div(sliderInput("strength", "Strength", min = 0, max = 1, step = .25, value = .5, ticks = F), style = "display:inline-block;width:40%"),
            div(style = "display:inline-block;width:5%"),
            div(sliderInput("trust", "Trust", min = 0, max = 1, step = .25, value = .5, ticks = F), style = "display:inline-block;width:40%"),
            div(style = "display:inline-block;width:5%"),
            
            div(sliderInput("confidence", "Confidence", min = 0, max = 1, step = .25, value = .5, ticks = F), style = "display:inline-block;width:40%"),
            style = ""
          )
        )
      ),
      style = "padding:10px;background-color:#EEFFEE;border:1px solid green"
    )
  )
})


observeEvent(input$savePackage, {
  vg <- values$codingGraf %>%
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
  
  values$codingGraf <- vg
  
  vpag <- values$pag
  iot <- input$onlyThisStatement
  delay(1000, refresh_and_filter_net(vg, vpag, iot))
})

# observeEvent(input$flip, {
#   updateSliderInput(session, inputId = "strength", value = -input$strength)
# })


output$combo <- renderUI(if (T) {
  if (length(valuesCoding$fromStack) > 1) {
    div(selectizeInput("combo", "Arrows interact?",
      choices = c("", "AND", "OR", "min", "max", "SAME", "mean", "sum"), width = "150px", selected = NULL,
      options =
        list(create = T, placeholder = "", onInitialize = I('function() { this.setValue(""); }')),
    ), style = "background-color:#EEFFEE;padding:10px;border:1px gray solid")
  }
})



# new widget for making arrows (and also for recoding nodes) --------------------------------------------


output$selectBoxButtons <- renderUI({
  varlist <- values$codingGraf %>%
    nodes_as_tibble() %>%
    pull(label) %>%
    unique() %>%
    as.character()
  varlist <- na.omit(varlist)
  
  tagList(
    div(selectizeInput("selectBoxValue",
      label = NULL, selected = NULL, multiple = F,
      options =
        list(create = T, placeholder = "Type to select or add variables", onInitialize = I('function() { this.setValue(""); }')),
      choices = varlist, width = "400px"
    ), style = "display:inline-block"),
    # these four widgets really need a better metahpor
    div((actionButton("addFrom", NULL,icon=icon("circle-o"))) %>% bs_embed_tooltip("Click to START new arrow(s) at the variable(s) which are listed in the dropdown and/or selected on the graph"), style = "display:inline-block"),
    div((actionButton("addTo", NULL,icon=icon("circle"))) %>% bs_embed_tooltip("Click to END the new arrow(s) at the variable which is listed in the dropdown or selected on the graph"), style = "display:inline-block"),
    div(style = "display:inline-block;margin-left:5px"),
    div(actionButton("recodeButton",NULL,icon=icon("chain")) %>%
        bs_embed_tooltip(title = "Click to RECODE the variable in the orange box into the variable which is listed in the dropdown or selected on the graph"), style = "display:inline-block")
  )
})






observeEvent(c(input$addFrom), ignoreInit = TRUE, {
  ns <- valuesCoding$nodesSelected
  if (is.null(ns)) ns <- ""
  if ("" != (ns) | !is.null(input$selectBoxValue)) {
    # browser()
    
    isb <- input$selectBoxValue
    if ("" == isb) isb <- NULL
    inpfrom <- NULL
    
    
    if (!is.null(isb)) {
      vg <- values$codingGraf
      inpfrom <- vg %>%
        mutate(id = row_number()) %>%
        filter(label == isb) %>%
        pull(id)
      
      if (length(inpfrom) == 0) {
        values$codingGraf <- vg %>%
          bind_nodes(tibble(label = isb, cluster = ""))
        doNotification("Adding Node", 2)
        inpfrom <- vg %>%
          nodes_as_tibble() %>%
          nrow() %>%
          `+`(1)
        
        tmp <- req(values$codingGraf) # has to be agg2 because of statements, but shouldn't be because some missed out
        vpag <- values$pag
        iot <- input$onlyThisStatement
        delay(1000, refresh_and_filter_net(tmp, vpag, iot))
      }
    }
    
    # browser()
    
    valuesCoding$fromStack <- c(ns, valuesCoding$fromStack, inpfrom) %>% unique()
    
    valuesCoding$fromStack <- valuesCoding$fromStack[valuesCoding$fromStack != ""]
    # doNotification("from stack ",99)
  }
  
  
  visNetworkProxy("netCoding") %>%
    visSetSelection(unselectAll = TRUE)
  
  updateSelectizeInput(session = session, inputId = "selectBoxValue", selected = "")
  
  session$sendCustomMessage("refocus", list(NULL))
})


observeEvent(c(input$selectBoxValue, valuesCoding$nodesSelected, valuesCoding$fromStack), {
  # browser()
  ins <- valuesCoding$nodesSelected
  isb <- input$selectBoxValue
  vcf <- valuesCoding$fromStack
  
  if (is.null(ins)) ins <- ""
  if (is.null(isb)) isb <- ""
  if (is.null(vcf)) vcf <- ""
  if (length(vcf) == 0) vcf <- ""
  if (("" != (ins) | "" != isb) & "" == (vcf)) enable("addFrom") else disable("addFrom")
  if (("" != (ins) | "" != isb) & "" != (vcf)) enable("addTo") else disable("addTo")
  if (("" != (ins) | "" != isb) & "" != (vcf)) enable("recodeButton") else disable("recodeButton")
})



observeEvent(input$addTo, {
  # browser()
  # valuesCoding$readyForEndArrow <- F
  
  # hiddenStore <- values$netCoding$x$nodes$hidden
  
  if (!is.null(input$quote)) {
    qq <- input$quote %>% as.character()
  } else {
    qq <- ""
  }
  
  # qq="" #FIXME
  # browser()
  
  inpfrom <- req(valuesCoding$fromStack)
  inpto <- NULL
  
  # browser()
  
  isb <- input$selectBoxValue
  if (isb == "") isb <- NULL
  
  if (!is.null(isb)) {
    vg <- values$codingGraf
    
    inpto <- vg %>%
      mutate(id = row_number()) %>%
      filter(label == isb) %>%
      pull(id)
    
    if (length(inpto) == 0) {
      values$codingGraf <- vg %>%
        bind_nodes(tibble(label = isb, cluster = ""))
      doNotification("Adding Node", 2)
      inpto <- vg %>%
        nodes_as_tibble() %>%
        nrow() %>%
        `+`(1)
    }
  }
  if (is.null(inpto)) {
    inpto <- req(valuesCoding$nodesSelected)[1]
  }
  
  newEdges <- tibble(
    from = inpfrom %>% as.integer(),
    to = inpto %>% as.integer(),
    trust = ifelse(input$crowd, .5, input$trust),
    strength = ifelse(input$crowd, .5, input$strength),
    label = ifelse(input$crowd, "", input$arrLabel),
    fun = "",
    combo.type = ifelse(input$crowd, "", ifelse(is.null(input$combo), "", input$combo)),
    definition.type = ifelse(input$crowd, "", input$definition.type),
    statement_id = ifelse(input$crowd, 1, values$pag %>% as.integer()),
    quote = ifelse(input$crowd, "", qq),
    # full.quote = ifelse(input$crowd, "", values$statements$text[values$statements$statement == values$pag])
  )
  
  values$codingGraf <- values$codingGraf %>%
    bind_edges(newEdges)
  
  if (!is.null(input$combo)) {
    if (input$combo != "") {
      values$codingGraf <- values$codingGraf %>%
        N_() %>%
        mutate(fun = ifelse(inpto == label, input$combo, fun))
    }
  }
  
  valuesCoding$fromStack <- NULL
  updateTextInput(session = session, "selectBoxValue", value = "")
  
  tmp <- req(values$codingGraf) # has to be agg2 because of statements, but shouldn't be because some missed out
  vpag <- values$pag
  iot <- input$onlyThisStatement
  delay(4000, refresh_and_filter_net(tmp, vpag, iot)) # TODO the 4 seconds is just a lucky guess
  valuesCoding$nodesSelected <- NULL
  valuesCoding$edgesSelected <- NULL
})



observeEvent(c(input$selectBoxValue), {
  if (req(input$sides) == "Code") {
    
    if (input$selectBoxValue != "" && nchar(input$selectBoxValue) > 2) {
      
      vag <- values$codingGraf %>%
        nodes_as_tibble() %>%
        pull(label)
      
      ids <- which(vag == input$selectBoxValue)
      
      # wipe <- setdiff(valuesCoding$foundIDs, ids)
      
      if (length(ids) != 0 && length(ids) < length(vag)) {
        visNetworkProxy("netCoding") %>%
          visUpdateNodes(tibble(id = ids, hidden = F)) %>%
          visSelectNodes(id = ids)
      }
      
      # if (length(wipe) > 0) {
      #   # visNetworkProxy("netCoding") %>%
      #   #   visUpdateNodes(tibble(id=wipe,hidden=T))
      # }
      
      visNetworkProxy("netCoding") %>%
        visFit(animation = list(duration = 500))
      
      valuesCoding$foundIDs <- c(valuesCoding$foundIDs, ids)
    }
  }
})
#

observeEvent(c(input$resetSelection, req(input$pager), input$onlyThisStatement), {
  
  
  
  tmp <- req(values$codingGraf) # has to be agg2 because of statements, but shouldn't be because some missed out
  vpag <- values$pag
  iot <- input$onlyThisStatement
  # browser()
  valuesCoding$fromStack <- NULL
  updateSelectizeInput(session = session, inputId = "selectBoxValue", selected = "")
  
  refresh_and_filter_net(tmp, vpag, iot)
})


# widgets to edit and delete selected nodes and edges. not complete ----
#

output$varForm <- renderUI({
  if (length(req(valuesCoding$nodesSelected)) > 0) {
    # browser()
    df <- values$codingGraf %>%
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
  vg <- values$codingGraf %>%
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
  
  values$codingGraf <- vg
})

observeEvent(input$deleteVarForm, {
  # browser()
  whichtarg <- values$codingGraf %>%
    nodes_as_tibble() %>%
    mutate(id = row_number()) %>%
    filter(id %in% valuesCoding$nodesSelected) %>%
    pull(id)
  
  values$codingGraf <- values$codingGraf %>%
    activate(nodes) %>%
    filter(!(row_number() %in% whichtarg))
})

# ode/variables panel----


# varSelectInput("variables", "Variable:", mtcars, multiple = TRUE) could be useful here

output$combineVars <- renderUI({
  varlist <- values$nodes$label %>%
    unique() %>%
    as.character()
  varlist <- na.omit(varlist)
  
  tagList(
    # h5("Combine variables")
    # ,
    
    ### this will fail if duplicate labels TODO
    selectizeInput("combineSelect",
      label = NULL, selected = NULL, multiple = T,
      options =
        list(create = T, placeholder = "start typing the name of the variables you want to combine", onInitialize = I('function() { this.setValue(""); }')),
      choices = varlist
    ),
    textInput("newName", "Type a name for this new combined variable"),
    actionButton("combine", "Combine!"),
    hr()
  )
})


output$quotesOutput <- renderUI({
  ins <- valuesCoding$nodesSelected
  inse <- valuesCoding$edgesSelected
  if (!is.null(ins)) {
    quotes <- values$netCoding$x$nodes %>%
      filter(id %in% ins) %>%
      pull(quote) %>%
      str_remove_all(",NA") %>%
      paste0(collapse = "; ")
  }
  
  else if (!is.null(inse)) {
    quotes <- values$netCoding$x$edges %>%
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



observeEvent(input$deletePackage, {
  # browser()
  if (!is.null(valuesCoding$edgesSelected)) {
    values$codingGraf <- values$codingGraf %>%
      activate(edges) %>%
      mutate(id = row_number()) %>%
      filter(!(id %in% valuesCoding$edgesSelected)) %>%
      select(-id)
  }
})



observeEvent(valuesCoding$edgesSelected, {
  # browser()
  
  vce <- valuesCoding$edgesSelected
  
  targetStatement <- values$codingGraf %>%
    edges_as_tibble() %>%
    filter(vce == row_number()) %>%
    pull(statement_id)
  
  if (!is.null(vce) & !input$onlyThisStatement) updatePageruiInput(session, "pager", page_current = as.numeric(targetStatement))
})

