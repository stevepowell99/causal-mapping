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


observeEvent(input$statementNoteGo,{
  values$statements <- values$statements %>% 
    mutate(note=if_else(statement_id==input$pager__page_current,input$statementNoteText,note)) 
})

output$selectBoxButtons <- renderUI({
  varlist <- req(values$rawGraf) %>%
    nodes_as_tibble() %>%
    pull(label) %>%
    unique() %>%
    as.character()
  varlist <- na.omit(varlist)
  
  # doNotification("regenerating")
  
  tagList(
    # div(actionButton("putTo", NULL,icon=icon("caret-left")) ,style="background-color:red"),
    div(
    div(selectizeInput("selectBoxValue1",
      label = NULL, selected = NULL, multiple = T,
      options =
        list(create = T, placeholder = "Type to select or add items at the start", onInitialize = I('function() { this.setValue(""); }')),
      choices = varlist, width = "400px"
    ), style = "display:inline-block"),
       div(actionButton("putFrom", NULL,icon=icon("caret-left")) %>% 
        bs_embed_tooltip("Click to add the highlighted item"),style="width:30px",class="myelement"),
    div(selectizeInput("selectBoxValue2",
      label = NULL, selected = NULL, multiple = T,
      options =
        list(create = T, placeholder = "Type to select or add items at the end", onInitialize = I('function() { this.setValue(""); }')),
      choices = varlist, width = "400px"
    ), style = "display:inline-block"),
    # these four widgets really need a better metahpor
      # div(actionButton("addFrom", NULL,icon=icon("circle-o")) %>% 
      #   bs_embed_tooltip("Click to START new arrow(s) at the variable(s) which are listed in the dropdown and/or selected on the graph"),style="width:30px",class="myelement"),
    div(actionButton("putTo", NULL,icon=icon("caret-left"))%>% 
        bs_embed_tooltip("Click to add the highlighted item"),style="width:30px",class="myelement" ),
    div(actionButton("addTo", NULL,icon=icon("circle")) %>% 
        bs_embed_tooltip("Click to END the new arrow(s) at the variable which is listed in the dropdown or selected on the graph"),style="width:30px",class="myelement"),
    div(actionButton("saveReroute", NULL,icon=icon("save")) %>% 
        bs_embed_tooltip("Click to save rerouted arrow"),style="width:30px",class="myelement"),
    div(actionButton("recodeButton",NULL,icon=icon("chain")),style="width:30px",class="myelement") 
    )
  
     # div(actionLink("deleteVarForm", paste0("Delete: ", valuesCoding$nodesSelected %>% paste0(collapse = ";"), "?")), style = "color:red",class="myelement")
    )
})


observe({
  if(is.null(input$net_selectedEdges)) disable("saveReroute") else enable("saveReroute")
})


output$editVar <- renderUI({

  if(!is.null(valuesCoding$nodesSelected))
    div(
      div(checkboxInput("editVar","Edit variable(s)"),class="myelement",width="150px")
      ,
      div(actionLink("deleteVarForm", paste0("Delete: ", valuesCoding$nodesSelected %>% paste0(collapse = ";"), "?")), style = "color:red",class="myelement")
    )
})


# show hidden vars --------------------------------------------------

# observeEvent(req(input$selectBoxValue), {
#   # browser()
#   vno <- isolate(values$codeNet$x$nodes)
#   lab <- input$selectBoxValue
#   labid <- which(vno$label==lab)
#   if(length(labid)>0){
#       refresh_and_filter_net(isolate(values$codeGraf),vpag = input$pager__page_current,iot = input$onlyThisStatement,fromStack = NULL,reveal=labid)
#       
#       visNetworkProxy("codeNet") %>%
#       visSelectNodes(id=labid)
#       
#   } else if(!is.null(vno)){  # node not exists 
# 
#   # browser()
#         visNetworkProxy("codeNet") %>%
#       visUpdateNodes(nodes=tibble(id=1:(nrow(vno)+1),label=c(vno$label,lab)))
#   } else doNotification("cannot show new node, sorry")
#   
#   # refresh_and_filter_net(req(values$grafCode),req(input$pager__page_current),req(input$onlyThis),req(values$fromStack))
#   
# })




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
      div(sliderInput("strength", "Strength", min = -1, max = 1, step = .25, value = .5, ticks = F), style = "width:30%",class="myelement"),
      div(textInput("arrNote", NULL, value = ifelse(ise, row$note, ""), placeholder = "note"), style = "width:25%",class="myelement"),
      div(awesomeCheckbox("edgeDetails", "Details", value = T),class="myelement", style = "width:12%"),
      
      conditionalPanel(
        "input.edgeDetails",
        # open="Details",
        tagList(
          
          
          
          div(
            div(textAreaInput("quote", NULL, value = ifelse(ise, row$quote, ""), placeholder = "quote", rows = 3, width = "100%"), style = "") %>%
              bs_embed_tooltip(title = if (T) ("If you select text in the Statement panel above using your mouse, it will appear here. You can also edit this text.")
              ),
            style = "margin-top:20px"
          ),
          div(
            div(selectizeInput("function.type", NULL, choices = c("+", "-", "NECC", "SUFF")), style = "display:inline-block;width:20%"),
            div(selectInput("attribution_code", NULL, choices = xc("positive-other negative-other")), style = "display:inline-block;width:20%"),
            div(textInput("package", NULL, value = ifelse(ise, row$package, ""), placeholder = "package"), style = "display:inline-block;"),
            div(textInput("packageNote", NULL, value = ifelse(ise, row$packageNote, ""), placeholder = "packageNote"), style = "display:inline-block;")
          ),
          
          div(
            id = "sliders",
            div(style = "display:inline-block;width:5%"),
      div(selectizeInput("definition.type", NULL, choices = c("", "Defined, directed", "Defined, undirected")), style = "width:20%",class="myelement"),
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

# observeEvent(c(input$addFrom), ignoreInit = TRUE, {
#   ns <- valuesCoding$nodesSelected
#   vpag <- input$pager__page_current
#   iot <- input$onlyThisStatement
# 
#         
#   if (is.null(ns)) ns <- ""
#   # if ("" != (ns) | !is.null(input$selectBoxValue)) {
#     # browser()
#     
#     isb <- isolate(input$selectBoxValue)
#     if ("" == isb) isb <- NULL
#     inpfrom <- NULL
#     
#     
#     if (!is.null(isb)) {
#       vg <- isolate(values$rawGraf)
#       inpfrom <- vg %>%
#         mutate(id = row_number()) %>%
#         filter(label == isb) %>%
#         pull(id)
#       
#       if (length(inpfrom) == 0) {
#         # browser()
#         values$rawGraf <- vg %>%
#           activate(nodes) %>% 
#           bind_nodes(tibble(label = isb, cluster = ""))
#         doNotification("Adding Node", 2)
#         inpfrom <- vg %>%
#           nodes_as_tibble() %>%
#           nrow() %>%
#           `+`(1)
#       }
#     }
#         valuesCoding$fromStack <- c(ns, inpfrom, isolate(valuesCoding$fromStack)) %>% unique()
#         
#         valuesCoding$fromStack <- valuesCoding$fromStack[valuesCoding$fromStack != ""]
#         
#         # browser()
#         
#         delay(1000, refresh_and_filter_net(values$rawGraf, vpag, iot,valuesCoding$fromStack))
#         
#         visNetworkProxy("codeNet") %>% 
#           visUnselectAll()
#         
#         
#     
#   updateSelectizeInput(session = session, inputId = "selectBoxValue", selected = "")
#   
#   session$sendCustomMessage("refocus", list(NULL))   # puts cursor back in box
# })



# addTo -----------------------------------------------------------------

observeEvent(input$addTo, {
# browser()
    
  if (!is.null(input$quote)) {
    qq <- input$quote %>% as.character()
  } else {
    qq <- ""
  }
  if(qq=="") qq <- input$highlightedText
  # if(qq=="") qq <- values$statements$text[values$statements$statement_id == values$pag]
  
  
  if(qq!="") {
  
  # browser()
  
  inpfrom <- req(input$selectBoxValue1)
  inpto <- req(input$selectBoxValue2)
  
  
  
  if (!is.null(inpfrom)&!is.null(inpto)) {
    vg <- values$rawGraf
    # browser()
    labs <- get_node_column(vg)

    add <- inpfrom[!(inpfrom %in% labs)]
    if (length(add) > 0) {
      # browser()
      vg <- vg %>%
        activate(nodes) %>% 
        bind_nodes(tibble(label = add, cluster = ""))
      doNotification("Adding Node", 2)
    }
      
      inpfromID <- which(nodes_as_tibble(vg)$label %in% inpfrom)

      labs <- vg %>%
        pull(label)
      
      add <- inpto[!(inpto %in% labs)]
      if (length(add) > 0) {
        # browser()
        vg <- vg %>%
          activate(nodes) %>% 
          bind_nodes(tibble(label = add, cluster = ""))
        doNotification("Adding Node", 2)
      }
      
      inptoID <- which(nodes_as_tibble(vg)$label %in% inpto)
      
    
  }
# browser()
    newEdges <- tibble(
    from = inpfromID %>% as.integer(),
    to = inptoID %>% as.integer(),
    trust = ifelse(F, .5, input$trust),
    strength = ifelse(F, .5, input$strength),
    note = ifelse(F, "", input$arrNote),
    fun = "",
    combo.type = ifelse(F, "", ifelse(is.null(input$combo), "", input$combo)),
    definition.type = ifelse(F, "", input$definition.type),
    attribution_code = ifelse(F, "", input$attribution_code),
    statement_id = ifelse(F, 1, input$pager__page_current %>% as.integer()),
    quote = ifelse(F, "", qq),
    # quote = qq,
    # full.quote = ifelse(input$crowd, "", values$statements$text[values$statements$statement == input$pager__page_current])
  )
# browser()  
  values$rawGraf <- vg %>%
    bind_edges(newEdges)
  
  if (!is.null(input$combo)) {
    if (input$combo != "") {
      values$rawGraf <- values$rawGraf %>%
        N_() %>%
        mutate(fun = ifelse(inpto == label, input$combo, fun))
    }
  }
  
  # browser()
  
  
  values$highlightedText <- ""
  updateTextInput(session = session, "selectBoxValue2", value = "")
  
  tmp <- req(values$rawGraf) 
  vpag <- input$pager__page_current
  iot <- input$onlyThisStatement
  delay(1000, refresh_and_filter_net(tmp, vpag, iot)) # TODO the 4 seconds is just a lucky guess
  valuesCoding$nodesSelected <- NULL
  valuesCoding$edgesSelected <- NULL
  
  doNotification("added edge(s)")
  } else {
  doNotification("you have to put a quote")
  }# if(Sys.getenv('SHINY_PORT') == "")system("COPY CMA2 /Y")
})



# insert selected vars --------------------------------------------------

observeEvent(input$putFrom,{
  # browser()
  labs <- get_node_column(values$rawGraf)
  if(!is.null(input$net_selected)){
    
    sel <- labs[input$net_selected]
    updateSelectizeInput(session = session,inputId = "selectBoxValue1",selected=sel)
  }
})

observeEvent(input$putTo,{
  # browser()
  labs <- get_node_column(values$rawGraf)
  if(!is.null(input$net_selected)){

        sel <- labs[input$net_selected]
    updateSelectizeInput(session = session,inputId = "selectBoxValue2",selected=sel)
  }
})

observeEvent(input$net_selectedEdges,{
  if(!is.null(input$net_selectedEdges)){
  labs <- get_node_column(values$rawGraf)
    
    ends <- values$rawGraf %>% 
      edges_as_tibble %>% 
      filter(input$net_selectedEdges==row_number())
    
    updateSelectizeInput(session = session,inputId = "selectBoxValue1",selected=labs[ends$from])
    updateSelectizeInput(session = session,inputId = "selectBoxValue2",selected=labs[ends$to])
    
    
  }
})

observeEvent(input$saveReroute,{
  browser()#this isn't right, it needs to change nodes and edges 
  if(T){
  labs <- get_node_column(values$rawGraf)
  values$rawGraf <- values$rawGraf %>% 
    activate(edges) %>% 
    reroute(from=which(labs==req(input$selectBoxValue1)),to=which(labs==req(input$selectBoxValue2)),
      subset=req(input$net_selectedEdges)[1]) 
}
  })


# reset selection and refresh on pressing button etc ----------------------------------
observeEvent(c(input$resetSelection, input$pager, input$onlyThisStatement), {

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

