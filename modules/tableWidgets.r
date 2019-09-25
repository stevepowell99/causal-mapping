
# first of zillions of handsontables.
observeEvent(input$statementsTableUp, {
  values$statements <- hot_to_r(input$statements) %>%
    mutate(text = str_replace(text, "\'", "")) %>%
    select(1:(ncol(default.statements))) %>%
    mutate(statement = row_number())
  # filter(text != "") %>%
  bind_rows(default.statements[1, ])
})


output$statementsPanel <- renderUI({
  tagList(
    actionButton("statementsTableUp", "Update"),
    rHandsontableOutput("statements")
  )
})

# Import/Statements panel -------------------------------------------------

# create statements table to allow user edit of statements----
output$statements <- renderRHandsontable({
  vs <- values$statements
  
  if (!is.null(valuesCoding$nodesSelected)) {
    if ("" != (valuesCoding$nodesSelected) & input$sides=="Display") {
      ids <- values$grafDisplay %>%
        nodes_as_tibble() %>%
        filter(id == input$net_selected) %>%
        # filter(id == valuesCoding$nodesSelected) %>%
        pull(statement_id) %>%
        str_remove_all("NA") %>%
        str_split(",") %>%
        `[[`(1)
      
      vs <- vs %>%
        filter(statement_id %in% ids)
    }
  }
  rhandsontable(
    vs,
    height = 700,
    rowHeaders = FALSE,
    usetypes = T
  ) %>%
    hot_context_menu(allowRowEdit = T, allowColEdit = T) %>%
    hot_cols(colWidths = c(400, rep(70, ncol(values$statements) - 1))) %>%
    hot_cols(fixedColumnsLeft = 1)
})


observeEvent(input$up.nodes, {
  
  # browser()
  req(input$up.nodes)
  df <- read_csv(input$up.nodes$datapath, T) %>%
    bind_rows(defaultNodes %>% filter(F)) %>%
    mutate(label = strip_symbols(label)) %>%
    tidy_colnames()
  
  values$graf <- tbl_graph(df, defaultEdges)
  doNotification("Updated variables, now update your edges")
})


observeEvent(input$up.edges, {
  req(input$up.edges)
  df <- read_csv(input$up.edges$datapath)[, ] %>%
    mutate_all(strip_symbols) %>%
    tidy_colnames()
  
  if (is.null(input$up.nodes)) {
    nodes <- tibble(label = (unique(unlist(c(df$from, df$to))))) %>%
      bind_rows(defaultNodes %>% filter(F))
  } else {
    nodes <- values$graf %>% nodes_as_tibble()
  }
  
  
  if (input$use.labels) {
    df <- df %>%
      mutate(
        from = id.finder(from, nodes),
        to = id.finder(to, nodes)
      )
  }
  
  # browser()
  df <- df %>%
    mutate(from = as.numeric(from), to = as.numeric(to)) %>%
    select(one_of(xc("from to label strength trust statement value"))) %>%
    bind_rows(defaultEdges %>% filter(F))
  
  
  
  # if (select(df, from, to) %>% unlist() %>% max() > max) doNotification("You have edges which don't make sense",level=2)
  # browser()
  values$graf <- tbl_graph(nodes, df)
  doNotification("Updated arrows")
})

# observe import statements -----------------------------------------------

observeEvent(input$up.statements, {
  req(input$up.statements)
  # browser()
  vstat <- read_csv(input$up.statements$datapath)[, ] %>%
    tidy_colnames()
  
  # vstat <- values$statements
  # fs <- findset("diagramsplitColumnNames")
  if (str_detect(colnames(vstat), "\\.") %>% any()) {
    col <- colnames(vstat) %>%
      str_detect("\\.") %>%
      which() %>%
      first()
    
    fsx <- str_split(colnames(vstat)[col], "\\.")[[1]] %>% str_trim()
    
    if (!is.na(col)) {
      vstat <- vstat %>%
        separate(col, into = fsx, sep = "\\.")
    }
    # values$statements <- vstat
  } else
    if (str_detect(colnames(vstat), ",") %>% any()) {
      col <- colnames(vstat) %>%
        str_detect(",") %>%
        which() %>%
        first()
      
      fsx <- str_split(colnames(vstat)[col], ",")[[1]] %>% str_trim()
      widths <- fsx %>% str_extract("[0-9]*$")
      names <- fsx %>% str_remove("[0-9]*$")
      
      if (!is.na(col)) {
        vstat <- vstat %>%
          separate(col, into = names, sep = cumsum(widths), convert = T)
      }
    }
  # %>%
  #   bind_rows(default.statements %>% filter(F))
  #
  # if (ncol(vs) > 9) vs <- vs[, 1:8]
  colnames(vstat)[1] <- "text"
  values$statements <- vstat %>%
    mutate(statement = row_number())
  
  
  # <- vs
  doNotification("Updated statements")
  # browser()
})


# automerge ----

observeEvent(input$autoMerge, { #               this is ancient. not used at moment
  # browser()
  values$edgesAgg %>%
    select(from, to, frequency) %>%
    mutate(frequency = as.numeric(frequency)) %>%
    spread(to, frequency, fill = 0, drop = F) %>%
    column_to_rownames("from") ->
    mat
  
  
  # need to add empty cols to both
  
  dummy <- data.frame(id = values$nodes$id)
  
  
  mat0 <- left_join(dummy, mat %>% rownames_to_column("id")) %>% select(-id) # to add any missing rows
  mat1 <- left_join(dummy, mat %>% t() %>% as.data.frame() %>% rownames_to_column("id")) %>% select(-id) # to add any missing rows
  
  mat2 <- cbind(mat0, mat1) %>% t()
  mat2[is.na(mat2)] <- 0
  mat2 <- mat2 %>% as.data.frame() # %>% rownames_to_column((id))
  
  # need to see bloth incoming and outgoing
  p <- psych::iclust(mat2, nclus = ncol(mat) - 5)
  
  clus <- p$clusters
  clus2 <- clus[, colSums(clus) > 1]
  
  pp <- apply(clus2, 1, function(x) which(x == 1) %>% as.vector()) %>%
    unlist() %>%
    t() %>%
    t() %>%
    as.data.frame() %>%
    rename(cluster = V1) %>%
    rownames_to_column("id") %>%
    mutate(id = str_remove(id, "V"), cluster = as.character(cluster)) # TODO terrible not even sure if what if missing numbers i seq
  
  
  # pp=p$sorted[[1]] %>% select(cluster)
  
  # because clusters with "" are not a cluster
  
  vn <- left_join(values$nodes, pp %>% rename(newclus = cluster)) %>%
    ungroup() %>%
    mutate(cluster = ifelse(is.na(newclus), id, newclus)) %>%
    group_by(cluster) %>%
    mutate(clusterLabel = paste0(label, collapse = " / ")) %>%
    mutate(clusterLabel = ifelse(cluster == "", label, clusterLabel))
  
  vn$cluster[is.na(vn$cluster)] <- ""
  
  values$nodes <- vn
  
  
  doNotification("Created clusters")
  
  if (T) { # could be zero but do check for vector
  } else {
    doNotification("Not possible to cluster")
  }
})

output$combine_button <- renderUI({
  if (!all(replace_na((nodes_as_tibble(values$graf))$cluster, "") == "")) {
    div(actionButton("node_permanent_combine", "Combine clusters permanently!?"), style = "margin-top:5px;")
  }
})

observeEvent(input$node_permanent_combine, {
  values$graf <- values$tmp.graf
})

output$nodeTable <- renderRHandsontable({
  # browser()
  arrows <- values$graf %>%
    edges_as_tibble() %>%
    select(xid = from, to) %>%
    unlist() %>%
    unclass() %>%
    as.tibble()
  
  vg <- values$graf %>%
    nodes_as_tibble() %>%
    # mutate_all(replaceNA) %>%
    mutate(xid = row_number()) %>%
    left_join(arrows %>% select(xid = value), by = "xid") %>%
    group_by(xid) %>%
    mutate(frequency = n()) %>%
    summarise_all(last)
  
  
  if (!is.null(valuesCoding$nodesSelected)) {
    whichtarg <- values$grafDisplay %>%
      nodes_as_tibble() %>%
      filter(row_number() == valuesCoding$nodesSelected) %>%
      pull(origID)
  }
  else {
    whichtarg <- 0
  }
  
  
  
  doNotification("Creating nodes table")
  rhandsontable(vg,
    rowHeaders = FALSE, selectCallback = T,
    row_highlight = as.numeric(whichtarg) - 1,
    col_highlight = 0,
    row_hide = (unite(vg, "xox") %>% pull(xox) %>% str_detect(input$nodeTableFilter) %>% `!`() %>% which()) - 1,
    usetypes = T
  ) %>%
    hot_context_menu(allowRowEdit = F) %>%
    hot_cols(fixedColumnsLeft = 1) %>%
    hot_cols(columnSorting = T) %>%
    hot_cols(manualColumnMove = T) %>%
    hot_col("type", source = xc("<U+25E8> <U+25EA> <U+265B> <U+058D>"), type = "dropdown") %>%
    hot_cols(renderer = "function(instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 if (instance.params) {
                 mhrows = instance.params.row_highlight;
                 mhrows = mhrows instanceof Array ? mhrows : [mhrows];
                 mhrows2 = instance.params.row_hide;
                 mhrows2 = mhrows2 instanceof Array ? mhrows2 : [mhrows2];
                 hcols = instance.params.col_highlight;
                 hcols = hcols instanceof Array ? hcols : [hcols];

                 }
                 if (instance.params && mhrows2.includes(row)) td.style.display = 'none';
                 if (instance.params &&  mhrows.includes(row)) td.style.background = 'coral';
                 if (instance.params &&  hcols.includes(col)) td.style.background = '#DDEEDD';
    }")
})

output$nodeTableAddCol <- renderUI({
  tagList(
    div(textInput("newNodeName", "Name"), style = "display:inline-block;width:30%"),
    div(selectInput("newNodeType", "Type", choices = xc("text numeric logical")), style = "display:inline-block;width:30%"),
    div(actionButton("newNodeGo", "Add column"), style = "display:inline-block;width:30%"),
    hr()
  )
})

observeEvent(input$newNodeGo, {
  # browser()
  if (!is.na(input$newNodeName)) {
    values$graf <- values$graf %>%
      activate(nodes) %>%
      mutate(!!input$newNodeName := ifelse(input$newNodeType == "numeric", as.numeric(NA), ifelse(input$newNodeType == "logical", as.logical(NA), "")))
    updateCheckboxInput(session, inputId = "nodeTableAddCol", value = F)
    # mutate(x=1)
  }
})

# observe node table----

observeEvent(input$nodeTableUp, {
  # browser()
  doNotification("Creating nodes table")
  x <- hot_to_r(input$nodeTable) %>%
    select(-frequency) %>%
    arrange(xid)
  
  values$graf <- tbl_graph(x, values$graf %>% edges_as_tibble())
})

# edge/arrows panel----

output$test <- renderUI({
  tagList(
    req(input$nodeTable_select$select$r) %>% as.character() %>% p()
  )
  
  # div(paste0(valuesCoding$nodesSelected,"--",input$current_edge_id),style="background:white")
})

observeEvent(input$edgeTableUp, {
  # browser()
  doNotification("Updating edges table")
  x <- hot_to_r(input$edgeTable) %>%
    select(-fromLabel, -toLabel)
  
  node.ids <- values$graf %>%
    nodes_as_tibble() %>%
    mutate(id = row_number()) %>%
    pull(id)
  
  x <- x %>%
    filter(from %in% node.ids & to %in% node.ids)
  
  values$graf <- tbl_graph(values$graf %>% nodes_as_tibble(), x)
})

output$edgeTable <- renderRHandsontable({
  # browser()
  doNotification("Creating edges table")
  # values$edges=values$edges
  ve <- values$graf %>%
    E_() %>%
    mutate(fromLabel = .N()$label[from], toLabel = .N()$label[to]) %>%
    select(fromLabel, toLabel, everything()) %>%
    edges_as_tibble() %>%
    select(-from, -to, everything())
  
  
  
  rhandsontable(ve, height = 600, rowHeaders = FALSE, row_highlight = as.numeric(input$current_edge_id) - 1, usetypes = T) %>%
    hot_context_menu(allowRowEdit = F) %>%
    hot_cols(fixedColumnsLeft = 1) %>%
    hot_cols(columnSorting = TRUE) %>%
    hot_cols(manualColumnMove = TRUE) %>%
    
    hot_cols(renderer = "function(instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 if (instance.params) {
                 mhrows = instance.params.row_highlight;
                 mhrows = mhrows instanceof Array ? mhrows : [mhrows];
                 }
                 if (instance.params && mhrows.includes(row)) td.style.background = 'coral';
    }")
})

