# RENDER visnetwork----
# finally we use values$grafAgg2 to generate the viz

observe(if (!is.null(values$grafAgg2)) {
  vga <- req(values$grafAgg2)
  this_tab <- isolate(input$sides)
  vals <- values$settingsGlobal
  
  if ((input$crowd)) {
    values$legend <- ""
  }
  
  doNotification("started viz")
  # browser()
  
  if (is.null(values$pag)) {
    values$pag <- 1
  }
  # browser()
  vn1 <-
    visNetwork(
      nodes =
        vga %>%
        activate(nodes) %>%
        mutate(id = row_number()) %>%
        as_tibble(),
      edges =
        vga %>% activate(edges) %>%
        as_tibble() %>%
        mutate(id = row_number()),
      
      main =
        findset("diagramtitle",vals),
      submain =
        findset("diagramsubtitle",vals),
      background = findset("diagrambackground", v = vals)
    )
  
  # browser()
  
  if(exists("loggedUser")){
    if(!is.null(loggedUser())){
      # browser()
      if(loggedUser()=="free")vn1 <- vn1%>% 
          visEvents(beforeDrawing = 'function(ctx) {		
			ctx.drawImage(document.getElementById("watermark"), 0, 800);
	}')
    }
  }
  
  vn <- vn1 %>%
    visInteraction(
      dragNodes = T,
      dragView = T,
      zoomView = T,
      navigationButtons = F,
      multiselect = T
    ) %>%
    visInteraction(
      tooltipStyle = "position: fixed;visibility:hidden;padding: 5px;
                font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;
                -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                 border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                 max-width:500px;word-break: break-all",
      hoverConnectedEdges = F,
      keyboard = F, # would be nice for navigation but interferes with text editing
      selectConnectedEdges = F
    ) %>%
    visOptions(
      manipulation = F,
      collapse = F,
      highlightNearest = list(
        enabled = T,
        degree = if (findset("diagramdownarrows",vals) %>% as.logical()) list(from = 0, to = 19) else list(from = 19, to = 0),
        # degree = ifelse(input$widgetDownArrows,list(from=0,to=19),list(from=19,to=0)),
        hover = T,
        labelOnly = F,
        algorithm = "hierarchical"
      ),
      selectedBy = if (!("cluster" %in% colnames(vga %>% nodes_as_tibble()))) "" else ifelse((vga %>% nodes_as_tibble() %>% pull(cluster) %>% replace_na("") %>% `==`("") %>% all()), "", "cluster"),
      nodesIdSelection = F
    ) %>%
    visConfigure(
      enabled = input$codeCollapse == "Advanced options",
      container = "advancedAnchor"
    ) %>%
    # visEvents(select = "function(edges) {
    #         Shiny.onInputChange('current_edge_id', edges.edges);
    #         ;}") %>%
    visEvents(select = "function(data) {
                Shiny.onInputChange('net_selected', data.nodes);
                Shiny.onInputChange('net_selectedEdges', data.edges);
                ;}") 
  
  
  
  # visEvents(select = "function(nodes) {
  #         Shiny.onInputChange('net_selected', nodes.nodes);
  #         ;}")
  # 
  #     if (!all(na.omit(vga$group) == "")) {
  #     }
  
  
  # layout ----------------
  # browser()
  
  if (findset("diagramlayout", v = vals) == "layout_with_sugiyama") {
    # browser()
    nods <- vn$x$nodes # saving them from the previous version, as the visigraphlayout in th next step, which shouldn't be necessary, is
    # browser()
    vn <- vn %>%
      visIgraphLayout(layout = "layout_with_sugiyama", randomSeed = 123, type = "full")
    
    if (findset("diagramorientation", v = vals) %in% xc("LR RL")) {
      # browser()
      vn$x$nodes <- nods # transferring from above
      tmp <- vn$x$nodes$x
      vn$x$nodes$x <- vn$x$nodes$y
      vn$x$nodes$y <- tmp
      # vn$x$nodes$color <- 'rgba(120,132,114,.7)'
      vnxn <- vn$x$nodes
      levels <- (length(unique(vnxn$x)))
      maxLen <- vnxn %>%
        group_by(x) %>%
        summarise(len = n()) %>%
        max()
      
      vnxn <- vnxn %>%
        group_by(x) %>%
        mutate(ran = min_rank(y), len = n(), y = rescale(ran, to = c(-1, 1)) * sqrt(len / maxLen) + rnorm(1, 0, .1 / len)) %>%
        ungroup() # had to put in a tiny bit of rnorm to stop some artefacts in visnetwork when nodes have same y
      # mutate(len=n(),ran=min_rank(y)-.5,y=ran*levels/(len*3))
      vnxn <- vnxn %>%
        mutate(x = 1 - scale(x) * 1, y = scale(y)) # 1 is the proportion. not sure why i added this line
      vn$x$nodes <- vnxn
    } else if (findset("diagramorientation", v = vals) %in% xc("DU RL")) {
      vn$x$nodes$x <- 1 - vn$x$nodes$x
      vn$x$nodes$y <- 1 - vn$x$nodes$y
    }
  } else {
    vn <- vn %>%
      visIgraphLayout(layout = "layout_in_circle", randomSeed = 123, smooth = T, type = "full")
  }
  
  # rowser()
  # if(vn$x$nodes$cluster %>% na.omit %>% length %>% `>`(0)  && input$sides=="Code")
  #   vn <- vn %>%
  #   visOptions(
  #   selectedBy=list(variable="cluster")
  #   )
  
  # if (findset("diagramphysics", v = vals) %>% as.logical()) {
  #   vn <- vn %>%
  #     visPhysics(barnesHut = list(avoidOverlap = .7))
  # }
  
  fvw <- ifelse(this_tab=="Code",findset("variablecoding.width",vals),findset("variablewidth",vals))
  
  
  # browser()
  vn <- vn %>%
    visNodes(
      shadow = list(enabled = T, size = 10),
      widthConstraint = if ("" == fvw) NULL else as.numeric(fvw), # ,300-(levels*10),#,(300*levels)-9,
      hidden = F, # findset("variablehidden",global=F) %>% as.logical(),
      scaling = list(label = list(enabled = F)),
      shape = findset("variableshape", v = vals),
      group = T, # findset("variablegroup",global=F),
      
      physics = T#findset("diagramphysics", v = vals)
    ) %>%
    visEdges(
      smooth = T,
      arrowStrikethrough = T,
      shadow =
        list(enabled = F, size = 5),
      hoverWidth = 8, #' function (width) {return width*50;}',
      selectionWidth = 8, # sqrt(nrow(vn$x$nodes)),
      physics = F,
      # color=list(highlight="#000000"),
      arrows =
        list(middle = list(type = "circle", scaleFactor = .5), from = list(type = "circle", scaleFactor = 0.2)),
      # ,
      # dashes = findset("arrowdashes") %>% as.logical()
    ) %>% 
    visPhysics(barnesHut = list(avoidOverlap = .7))
  # browser()
  values$net <- vn
  
  doNotification("Produced viz")
})

observe({
  # browser()
  output$net <- renderVisNetwork({
    doNotification("render viz")
    # browser()
    # doNotification("rendered viz")
    values$net
  })
  # if (T) {
  #   visNetworkProxy("net") %>%
  #     visUpdateNodes(nodes = tibble(id = 1:20, color = "red"))
  # }
})



