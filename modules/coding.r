# Code panel -----------------------------------------------------------

# observing edge and node selections --------------------------------------

# this is necessary because you can't programmatically deselect, you have to actually click. so store active node / edge in valuesCoding$nodesSelected instead, and nullify it manually when neccessary

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






# buttons for making arrows (and also for recoding nodes) --------------------------------------------


output$selectBoxButtons <- renderUI({
  varlist <- req(values$rawGraf) %>%
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
  ,checkboxInput("editVar","Edit variable(s)")
    )
})

# show hidden vars --------------------------------------------------

observeEvent(req(input$selectBoxValue), {
  # browser()
  vno <- isolate(values$codeNet$x$nodes)
  lab <- input$selectBoxValue
  labid <- which(vno$label==lab)
  # browser()
  if(length(labid)>0){
      refresh_and_filter_net(isolate(values$codeGraf),vpag = input$pager__page_current,iot = input$onlyThisStatement,fromStack = NULL,reveal=labid)
      
      visNetworkProxy("codeNet") %>%
      visSelectNodes(id=labid)
      
  } else {  # node not exists 

        visNetworkProxy("codeNet") %>% 
      visUpdateNodes(nodes=tibble(id=1:(nrow(vno)+1),label=c(vno$label,lab))) 
  }
  
  # refresh_and_filter_net(req(values$grafCode),req(input$pager__page_current),req(input$onlyThis),req(values$fromStack))
  
})

# disable enable buttons --------------------------------------------------

observeEvent(req(valuesCoding$nodesSelected), {
  if ("" != valuesCoding$nodesSelected[1] ) enable("editVar") else disable("editVar")
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
  # if (("" != (ins) | "" != isb) & "" == (vcf)) enable("addFrom") else disable("addFrom")
  # browser()
  if (("" != (ins) | "" != isb) & "" != (vcf)[[1]]) enable("addTo") else disable("addTo")
  if (("" != (ins) | "" != isb) & "" != (vcf)[[1]]) enable("recodeButton") else disable("recodeButton")
  
  
  
})

# Add edges widget. Provides the additional fields for the edge ----

output$add_edges_widget <- renderUI({
  # varlist=values$rawGraf %>% nodes_as_tibble() %>% pull(label) %>% unique() %>% as.character()
  # varlist <- na.omit(varlist)
  
  df <- req(values$rawGraf) %>%
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


# widet for attributes for combo edges ------------------------------------

output$combo <- renderUI(if (T) {
  if (length(valuesCoding$fromStack) > 1) {
    div(selectizeInput("combo", "Arrows interact?",
      choices = c("", "AND", "OR", "min", "max", "SAME", "mean", "sum"), width = "150px", selected = NULL,
      options =
        list(create = T, placeholder = "", onInitialize = I('function() { this.setValue(""); }')),
    ), style = "background-color:#EEFFEE;padding:10px;border:1px gray solid")
  }
})






# addFrom -----------------------------------------------------------------

observeEvent(c(input$addFrom), ignoreInit = TRUE, {
  ns <- valuesCoding$nodesSelected
  vpag <- input$pager__page_current
  iot <- input$onlyThisStatement

        
  if (is.null(ns)) ns <- ""
  # if ("" != (ns) | !is.null(input$selectBoxValue)) {
    # browser()
    
    isb <- isolate(input$selectBoxValue)
    if ("" == isb) isb <- NULL
    inpfrom <- NULL
    
    
    if (!is.null(isb)) {
      vg <- isolate(values$rawGraf)
      inpfrom <- vg %>%
        mutate(id = row_number()) %>%
        filter(label == isb) %>%
        pull(id)
      
      if (length(inpfrom) == 0) {
        values$rawGraf <- vg %>%
          bind_nodes(tibble(label = isb, cluster = ""))
        doNotification("Adding Node", 2)
        inpfrom <- vg %>%
          nodes_as_tibble() %>%
          nrow() %>%
          `+`(1)
      }
    }
        valuesCoding$fromStack <- c(ns, inpfrom, isolate(valuesCoding$fromStack)) %>% unique()
        
        valuesCoding$fromStack <- valuesCoding$fromStack[valuesCoding$fromStack != ""]
        
        # browser()
        
        delay(1000, refresh_and_filter_net(values$rawGraf, vpag, iot,valuesCoding$fromStack))
    
  updateSelectizeInput(session = session, inputId = "selectBoxValue", selected = "")
  
  session$sendCustomMessage("refocus", list(NULL))   # puts cursor back in box
})



# addTo -----------------------------------------------------------------

observeEvent(input$addTo, {

    
  if (!is.null(input$quote)) {
    qq <- input$quote %>% as.character()
  } else {
    qq <- ""
  }
  # browser()
  
  inpfrom <- req(valuesCoding$fromStack)
  inpto <- NULL
  
  
  isb <- input$selectBoxValue
  if (isb == "") isb <- NULL
  
  if (!is.null(isb)) {
    vg <- values$rawGraf
    
    inpto <- vg %>%
      mutate(id = row_number()) %>%
      filter(label == isb) %>%
      pull(id)
    
    if (length(inpto) == 0) {
      values$rawGraf <- vg %>%
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
    trust = ifelse(F, .5, input$trust),
    strength = ifelse(F, .5, input$strength),
    label = ifelse(F, "", input$arrLabel),
    fun = "",
    combo.type = ifelse(F, "", ifelse(is.null(input$combo), "", input$combo)),
    definition.type = ifelse(F, "", input$definition.type),
    statement_id = ifelse(F, 1, input$pager__page_current %>% as.integer()),
    quote = ifelse(F, "", qq),
    # quote = qq,
    # full.quote = ifelse(input$crowd, "", values$statements$text[values$statements$statement == input$pager__page_current])
  )
  
  values$rawGraf <- values$rawGraf %>%
    bind_edges(newEdges)
  
  if (!is.null(input$combo)) {
    if (input$combo != "") {
      values$rawGraf <- values$rawGraf %>%
        N_() %>%
        mutate(fun = ifelse(inpto == label, input$combo, fun))
    }
  }
  
  # browser()
  
  valuesCoding$fromStack <- NULL
  updateTextInput(session = session, "selectBoxValue", value = "")
  
  tmp <- req(values$rawGraf) 
  vpag <- input$pager__page_current
  iot <- input$onlyThisStatement
  delay(1000, refresh_and_filter_net(tmp, vpag, iot)) # TODO the 4 seconds is just a lucky guess
  valuesCoding$nodesSelected <- NULL
  valuesCoding$edgesSelected <- NULL
})






# reset selection and refresh on pressing button etc ----------------------------------
observeEvent(c(input$resetSelection, req(input$pager), input$onlyThisStatement), {

  tmp <- req(values$rawGraf) # has to be agg2 because of statements, but shouldn't be because some missed out
  vpag <- input$pager__page_current
  iot <- input$onlyThisStatement
  # browser()
  valuesCoding$fromStack <- NULL
  updateSelectizeInput(session = session, inputId = "selectBoxValue", selected = "")
# doNotification(glue("resetting: pag is {input$pager__page_current}"))
  refresh_and_filter_net(tmp, vpag, iot)
})







# switch pager to correct statement when selecting an edge ----------------


observeEvent(valuesCoding$edgesSelected, {
  # browser()
  
  vce <- valuesCoding$edgesSelected
  
  targetStatement <- values$rawGraf %>%
    edges_as_tibble() %>%
    filter(vce == row_number()) %>%
    pull(statement_id)
  
  if (!is.null(vce) & !input$onlyThisStatement) 
    updatePageruiInput(session, "pager", page_current = as.numeric(targetStatement))
})

