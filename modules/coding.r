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
  )
})

observeEvent(input$recodeButton, {
  # browser()
  vfs <- req(valuesCoding$fromStack) %>% as.numeric()
  ins <- req(valuesCoding$nodesSelected[[1]]) %>% as.numeric()
  
  vpag <- values$pag
  iot <- input$onlyThisStatement
  
  ved <- values$graf %>%
    edges_as_tibble()
  
  
  ved <- ved %>%
    mutate(thisStatement = (ved$statement_id == vpag | !iot)) %>%
    mutate(from = ifelse(thisStatement & from %in% vfs, ins, from)) %>%
    mutate(to = ifelse(thisStatement & to %in% vfs, ins, to))
  
  values$graf <-
    tbl_graph(values$graf %>% nodes_as_tibble(), ved) # kinda stupid not to use tidygraph functions
  
  
  delay(1000, refresh_and_filter_net(values$graf, vpag, iot))
  doNotification("Recoded variable(s)", 9)
  valuesCoding$nodesSelected <- NULL
  valuesCoding$edgesSelected <- NULL
  valuesCoding$fromStack <- NULL
})


# Add edges widget----

output$add_edges_widget <- renderUI({
  # varlist=values$graf %>% nodes_as_tibble() %>% pull(label) %>% unique() %>% as.character()
  # varlist <- na.omit(varlist)
  
  df <- req(values$graf) %>%
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
  vg <- values$graf %>%
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
  
  values$graf <- vg
  
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
  varlist <- req(values$graf) %>%
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
      vg <- values$graf
      inpfrom <- vg %>%
        mutate(id = row_number()) %>%
        filter(label == isb) %>%
        pull(id)
      
      if (length(inpfrom) == 0) {
        values$graf <- vg %>%
          bind_nodes(tibble(label = isb, cluster = ""))
        doNotification("Adding Node", 2)
        inpfrom <- vg %>%
          nodes_as_tibble() %>%
          nrow() %>%
          `+`(1)
        
        tmp <- req(values$graf) # has to be agg2 because of statements, but shouldn't be because some missed out
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
  
  
  visNetworkProxy("codeNet") %>%
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
  # if (("" != (ins) | "" != isb) & "" == (vcf)) enable("addFrom") else disable("addFrom")
  if (("" != (ins) | "" != isb) & "" != (vcf)[[1]]) enable("addTo") else disable("addTo")
  if (("" != (ins) | "" != isb) & "" != (vcf)[[1]]) enable("recodeButton") else disable("recodeButton")
})



observeEvent(input$addTo, {

    
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
    vg <- values$graf
    
    inpto <- vg %>%
      mutate(id = row_number()) %>%
      filter(label == isb) %>%
      pull(id)
    
    if (length(inpto) == 0) {
      values$graf <- vg %>%
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
  
  values$graf <- values$graf %>%
    bind_edges(newEdges)
  
  if (!is.null(input$combo)) {
    if (input$combo != "") {
      values$graf <- values$graf %>%
        N_() %>%
        mutate(fun = ifelse(inpto == label, input$combo, fun))
    }
  }
  
  valuesCoding$fromStack <- NULL
  updateTextInput(session = session, "selectBoxValue", value = "")
  
  tmp <- req(values$graf) # has to be agg2 because of statements, but shouldn't be because some missed out
  vpag <- values$pag
  iot <- input$onlyThisStatement
  delay(4000, refresh_and_filter_net(tmp, vpag, iot)) # TODO the 4 seconds is just a lucky guess
  valuesCoding$nodesSelected <- NULL
  valuesCoding$edgesSelected <- NULL
})



observeEvent(c(input$selectBoxValue), {
  if (req(input$sides) == "Code") {
    
    if (input$selectBoxValue != "" && nchar(input$selectBoxValue) > 2) {
      
      vag <- values$graf %>%
        nodes_as_tibble() %>%
        pull(label)
      
      ids <- which(vag == input$selectBoxValue)
      
      # wipe <- setdiff(valuesCoding$foundIDs, ids)
      
      if (length(ids) != 0 && length(ids) < length(vag)) {
        visNetworkProxy("codeNet") %>%
          visUpdateNodes(tibble(id = ids, hidden = F)) %>%
          visSelectNodes(id = ids)
      }
      
      
      visNetworkProxy("codeNet") %>%
        visFit(animation = list(duration = 500))
      
      valuesCoding$foundIDs <- c(valuesCoding$foundIDs, ids)
    }
  }
})
#

observeEvent(c(input$resetSelection, req(input$pager), input$onlyThisStatement), {
  
  tmp <- req(values$graf) # has to be agg2 because of statements, but shouldn't be because some missed out
  vpag <- values$pag
  iot <- input$onlyThisStatement
  # browser()
  valuesCoding$fromStack <- NULL
  updateSelectizeInput(session = session, inputId = "selectBoxValue", selected = "")
  
  refresh_and_filter_net(tmp, vpag, iot)
})



# varSelectInput("variables", "Variable:", mtcars, multiple = TRUE) could be useful here
# 
# output$combineVars <- renderUI({
#   varlist <- values$nodes$label %>%
#     unique() %>%
#     as.character()
#   varlist <- na.omit(varlist)
#   
#   tagList(
#     # h5("Combine variables")
#     # ,
#     
#     ### this will fail if duplicate labels TODO
#     selectizeInput("combineSelect",
#       label = NULL, selected = NULL, multiple = T,
#       options =
#         list(create = T, placeholder = "start typing the name of the variables you want to combine", onInitialize = I('function() { this.setValue(""); }')),
#       choices = varlist
#     ),
#     textInput("newName", "Type a name for this new combined variable"),
#     actionButton("combine", "Combine!"),
#     hr()
#   )
# })



observeEvent(input$deletePackage, {
  # browser()
  if (!is.null(valuesCoding$edgesSelected)) {
    values$graf <- values$graf %>%
      activate(edges) %>%
      mutate(id = row_number()) %>%
      filter(!(id %in% valuesCoding$edgesSelected)) %>%
      select(-id)
  }
})



observeEvent(valuesCoding$edgesSelected, {
  # browser()
  
  vce <- valuesCoding$edgesSelected
  
  targetStatement <- values$graf %>%
    edges_as_tibble() %>%
    filter(vce == row_number()) %>%
    pull(statement_id)
  
  if (!is.null(vce) & !input$onlyThisStatement) updatePageruiInput(session, "pager", page_current = as.numeric(targetStatement))
})

