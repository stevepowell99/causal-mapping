# blank start----


#   the default graph object with no nodes or edges
values$graf <- tbl_graph(
  defaultNodes[0, ],
  defaultEdges[0, ]
)




# AGGREGATE ---------------------------------------------------------------------------
# the long process of aggregating values$graf into values$grafAgg2, adding formatting etc


observe({
  # input$upConditionalBut
  # make this code run whenever the tab$change reactive triggers
  tab$change
  vals <- values$settingsGlobal
  # prevent this code running every time we change tab
  this_tab <- isolate(input$sides)
  
  if (nrow(nodes_as_tibble(req(values$graf))) > 0) {
    
    # prepare statements, split columns ----
    
    doNotification("starting aggregation")
    legend <- ""
   
    
    tmp <- values$graf 
    
    tmp <- tmp %>% 
      mutate(original_is_driver=node_is_source(),original_is_outcome=node_is_sink())

    tmp <- prepare_vg(tmp)
    
    
    # prepare ved
    
    vno <- tmp %>% nodes_as_tibble()
    ved <- tmp %>% edges_as_tibble()
    
    ved <- prepare_ved(ved)
    # vno <- prepare_vno(vno)
    
    
    
    ved <- ved %>%
      left_join(values$statements, by = "statement_id") 
    
    
    # browser()
    
    ved <- ved %>%
      left_join(values$statements_extra,by = "statement_id")
    
    
    
    
    ved <- ved %>%
      # mutate(statement = as.character(statement)) %>%
      mutate(wstrength = strength * trust)
    # browser()
    
    vno$font.color <- "#eeeeee"
    vno$color.background <- "#226622"
    vno$font.size <- findset("variablecoding.font.size",vals)
    
    
    if(nrow(ved)>0)ved$width <- 3
    
    # infer ----
  
    
    values$codingGraf <- tbl_graph(vno,ved)
    
    }
  })
  
observe({
  
  vals <- values$settingsGlobal
  # prevent this code running every time we change tab
  this_tab <- isolate(input$sides)
  
  
    if (nrow(nodes_as_tibble(req(values$codingGraf))) > 0 & this_tab!="Code") {
    # browser()
  
      tmp <- values$codingGraf
      
      vno <- tmp %>% nodes_as_tibble()
      ved <- tmp %>% edges_as_tibble()  
    
    if (findset("variableinfer", v = vals) %>% as.logical()) {
      tmp <- infer(tmp)
      legend <- paste0(legend, "</br>Causal inference carried out")
    }
    
    
    
    # merge nodes -------------------------------------------------------------
    
    
    
    if ((findset("variablemerge",vals) %>% as.logical()) & this_tab != "Code") { # need to convert to and froms in edge df
      
      x <- merge_nodes(vno, ved)
      vno <- x[[1]]
      ved <- x[[2]]
    }
    
    
    # ved rick inv_multi --------------------------------
    
    if (("from" %in% colnames(ved)) && as.logical(findset("arrowabsence",vals))) { # todo findset
      
      doNotification("rick aggregation")
      
      if (all(is.na(ved$statement_id))) ved$statement_id <- 1
      # browser()
      ved <- ved %>%
        inv_multi()
    }
    
    
    # ved join statements--------------------------------
    # browser()
    # if (is.null(values$statements$source_id)) values$statements <- values$statements %>% mutate(source_id = 1)
    
    
    
    # quip stats by question/domain---------------
    
    
    if ("source_id" %in% colnames(ved) && "question" %in% colnames(ved)) {
      
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
    
    
    # browser()
    if (input$sides != "Code") {
    # if (this_tab != "Code") {
      
      
      # join edges with nodes ---------------------------------------------------
      
      
      doNotification("join to edges aggregation")
      
      
      if (findset("variablejoinedges", v = vals) %>% as.logical()) { # todo, should list any functions. variablejoinedges is pointless
        
        vno <- join_nodes_and_edges(vno, ved)
        
        
        # doNotification("merging nodes and arrows")
      }

      
            
      # minimum freq for vars and edges
      # browser()
      
      mf <- findset("variableminimum.frequency", v = vals) %>% as.numeric()
      if (this_tab != "Code" && mf > 0) {
        values$grafMerged <- tbl_graph(vno, ved) 
        
        tmp <- values$grafMerged %>%
          N_() %>%
          filter(frequency > mf)
        
        vno <- tmp %>% nodes_as_tibble()
        ved <- tmp %>% edges_as_tibble()  %>%
          filter(frequency > findset("arrowminimum.frequency", v = vals))
        
        
      }
    }
    
    
    doNotification("format aggregation")
    
    
    
    tmp <- tbl_graph(vno, ved)
    # browser()
    
    # layout just for notForwards ----------------
    
    
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
    # if (this_tab != "Code") { 
      vno <- vno %>%
        format_nodes_and_edges(input, type = "node", values$settingsConditional)
      
      
      ved <- ved %>%
        format_nodes_and_edges(input, type = "edge", values$settingsConditional)
    # } else {
      # browser()
      
    # }
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
    
    

# tidygraph calculations --------------------------------------------------

    tmp <- tmp %>% 
      mutate(is_driver=node_is_source(),is_outcome=node_is_sink())
    
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

