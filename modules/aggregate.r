# blank start----


#   the default graph object with no nodes or edges
values$graf <- tbl_graph(
  defaultNodes[0, ],
  defaultEdges[0, ]
)




# AGGREGATE ---------------------------------------------------------------------------
# the long process of aggregating values$graf into values$grafAgg2, adding formatting etc


observe({
  
  # make this code run whenever the tab$change reactive triggers
  tab$change
  # input$projectSelect
  # SP commented out above line
  
  # prevent this code running every time we update values
  
  
  # browser()
  vals <- values$settingsGlobal
  # vals <- isolate(values$settingsGlobal)
  
  # prevent this code running every time we change tab
  this_tab <- isolate(input$sides)
  
  edges_tbl <- edges_as_tibble(req(values$graf))
  
  if (nrow(edges_tbl) > 0) {
    
    # prepare statements, split columns ----
    
    doNotification("starting aggregation")
    legend <- ""
    
    # post-process original version
    
    graph_values <- prepare_vg(values$graf)
    
    
    # infer ----
    
    if (findset("variableinfer", v = vals) %>% as.logical()) {
      graph_values <- infer(graph_values)
      legend <- paste0(legend, "</br>Causal inference carried out")
    }
    
    vno <- graph_values %>% nodes_as_tibble()
    ved <- graph_values %>% edges_as_tibble()
    
    
    
    # merge nodes -------------------------------------------------------------
    
    # browser()
    
    
    if ((findset("variablemerge",vals) %>% as.logical()) & this_tab != "Code") { # need to convert to and froms in edge df
      
      x <- merge_nodes(vno, ved)
      vno <- x[[1]]
      ved <- x[[2]]
    }
    
    
    
    # prepare ved
    
    ved <- prepare_ved(ved)
    # vno <- prepare_vno(vno)
    
    # ved rick inv_multi --------------------------------
    
    if (("from" %in% colnames(ved)) && as.logical(findset("arrowabsence",vals))) { # todo findset
      
      doNotification("rick aggregation")
      
      if (all(is.na(ved$statement))) ved$statement <- 1
      # browser()
      ved <- ved %>%
        inv_multi()
    }
    
    
    # ved join statements--------------------------------
    # browser()
    if (is.null(values$statements$source__id)) values$statements <- values$statements %>% mutate(source__id = 1)
    
    ved <- ved_join_statements(ved, values$statements)
    
    
    # saveRDS(ved, "ved")
    
    
    
    
    ved <- ved %>%
      mutate(statement = as.character(statement)) %>%
      mutate(wstrength = strength * trust)
    # browser()
    
    
    
    # quip stats by question/domain---------------
    
    
    if ("source__id" %in% colnames(ved) && "question" %in% colnames(ved)) {
      
      # browser()
      ved <- ved %>%
        group_by(from, to, question) %>%
        mutate(citation_count = length(unique(source))) %>%
        ungroup() %>%
        group_by(from, to) %>%
        mutate(respondent_count = length(unique(source))) %>%
        ungroup() %>%
        mutate(citation_intensity = citation_count / respondent_count)
    }
    
    # merge edges -------------------------------------------------------------
    
    
    ved <- merge_edges(ved,this_tab,vals)
    
    doNotification("min freq aggregation")
    
    # edge minimum freq ----
    
    # browser()
    if (this_tab != "Code") {
      ved <- ved %>%
        filter(frequency > findset("arrowminimum.frequency", v = vals))
      
      
      
      # join edges with nodes ---------------------------------------------------
      
      
      doNotification("join to edges aggregation")
      
      
      if (findset("variablejoinedges", v = vals) %>% as.logical()) { # todo, should list any functions. variablejoinedges is pointless
        
        vno <- join_nodes_and_edges(vno, ved)
        
        
        # doNotification("merging nodes and arrows")
      }
      
      # minimum freq for vars
      # browser()
      mf <- findset("variableminimum.frequency", v = vals) %>% as.numeric()
      if (this_tab != "Code" && mf > 0) {
        tmp <- tbl_graph(vno, ved) %>%
          N_() %>%
          filter(frequency > mf)
        
        vno <- tmp %>% nodes_as_tibble()
        ved <- tmp %>% edges_as_tibble()
      }
    }
    
    
    doNotification("format aggregation")
    
    
    
    tmp <- tbl_graph(vno, ved)
    # browser()
    
    # layout ----------------
    
    
    layout <- create_layout(tmp, layout = "sugiyama") %>%
      select(x, y, id)
    
    tmp <- tmp %>%
      activate(nodes) %>%
      left_join(layout, by = "id")
    
    tmp <- tmp %>%
      activate(edges) %>%
      mutate(fromLevel = .N()$y[from], toLevel = .N()$y[to], notForwards = fromLevel >= toLevel)
    
    
    vno <- tmp %>% nodes_as_tibble()
    ved <- tmp %>% edges_as_tibble()
    
    # browser()
    if (this_tab != "Code") { 
      vno <- vno %>%
        format_nodes_and_edges(input, type = "node", values$settingsConditional)
      
      
      ved <- ved %>%
        format_nodes_and_edges(input, type = "edge", values$settingsConditional)
    } else {
      # browser()
      vno$font.color <- "#eeeeee"
      vno$color.background <- "#226622"
      vno$font.size <- findset("variablecoding.font.size",vals)
      
      
      ved$width <- 3
    }
    ### make sure text is visibile when highlighted
    vno <- vno %>%
      mutate(color.highlight.background = set_text_contrast_color(font.color)) %>%
      mutate(color.background = add_opacity_vec(color.background, as.numeric(findset("variableopacity", v = vals))))
    
    
    # margin--------
    # browser()
    vno <- vno %>%
      mutate(margin = 10) # decent approximation
    
    
    
    # rationalise----
    
    doNotification("final aggregation")
    
    # hard-coded formatting ----
    # browser()
    ved <- ved %>%
      mutate(label = replace_na(label, "")) %>%
      mutate(label = ifelse(strength < 0, paste0("<U+0001F500> ", label), label)) %>%
      # mutate(combo.type <- replace_na(combo.type,"")) %>%
      # mutate(arrows.middle.enabled = ifelse(combo.type == "", F, T)) %>%
      mutate(arrows.middle.enabled = F) %>%
      # mutate(label = paste0(label, ifelse(arrows.middle.enabled, paste0(" ", combo.type), ""))) %>%
      mutate(dashes = str_remove_all(definition.type,",") != "") %>%
      mutate(arrows.to = definition.type != "Defined, undirected")
    
    if (!is.null(legend)) {
      if (legend != "") values$legend <- glue("</br><b style='font-color:red;'>Legend:</b>{legend}</br></br></br></br>")
    }
    
    # labels ----
    vno <- vno %>%
      mutate(label = if_else(value > 0, paste0(label, " <U+2665>"), label)) %>%
      mutate(label = if_else(value < 0, paste0(label, " <U+2639>"), label)) 
    # browser()
    vno <- vno %>%
      mutate(label = make_labels(findset("variablelabel", v = vals),findset("variablewrap", v = vals), vno,type="none"))
    
    vno <- vno %>%
      mutate(title = make_labels(findset("variabletooltip", v = vals),findset("variablewrap", v = vals), vno,type="html"))
    
    
    ved <- ved %>%
      mutate(
        title = make_labels(findset("arrowtooltip", v = vals),findset("arrowrap", v = vals), ved,type="html"),
        label = make_labels(findset("arrowlabel", v = vals),findset("arrowwrap", v = vals), ved,type="none")
      )
    
    
    # fontsize ceiling so it doesn't crash. doesn't take into account if there are very long words
    # if (this_tab != "Code")      vno <- vno %>% mutate(font.size =  findset("variablecoding.font.size",v = vals)
    # )
    # browser()
    
    tmp <- tbl_graph(vno, ved)
    
    # autogroup
    
    if (findset("variableautogroup", v = vals)) {
      tmp <- tmp %>%
        N_() %>%
        mutate(group = group_walktrap())
    }
    
    
    values$grafAgg2 <- tmp
    
    # browser()
    
    doNotification("Aggregated")
  }
})

