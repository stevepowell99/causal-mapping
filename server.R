server <- function(input, output, session) {


  # TODO pool package to manage
  
  
  
  # initialising ------------------------------------------------------------
  
  
  track_usage(storage_mode = store_json(path = "logs/"))
  
  
  
  
  # autoInvalidate <- reactiveTimer(2000)

  # reactive values ---------------------------------------------------------

  values <- reactiveValues() # nearly all reactive values are stored in values$...
  values$pag <- 1 # stores value of pager in Code panel
  values$statements <- default.statements
  values$sources <- default.sources
  # values$clickArrow <- F # no idea
  values$crowd <- F


  values$settingsConditional <- defaultSettingsConditional
  values$settingsGlobal <- defaultSettingsGlobal

  values$highlightedText <- "" # part of a system to copy any text highlighted with mouse in browser i.e. from interview quotes and insert into the edge information

  
  source("modules/user.r",local=T)
  source("modules/coding.r",local=T)
  
  
  
  
  
  
  
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
    if (tab$old == "Code") {
      tab$change <- tab$change + 1
    } else if (input$sides == "Code") {
      tab$change <- tab$change + 1
    }
    tab$old <- input$sides
  })

  inputtitl <- ""
  makeReactiveBinding("inputtitl")


  



  # the useful "interrupt" button in bottom righthand corner
  observeEvent(input$Interrupt, {
    browser()
  })

  # first of zillions of handsontables.
  observeEvent(input$statementsTableUp, {
    values$statements <- hot_to_r(input$statements) %>%
      mutate(text = str_replace(text, "\'", "")) %>%
      select(1:(ncol(default.statements))) %>%
      mutate(statement = row_number())
    # filter(text != "") %>%
    bind_rows(default.statements[1, ])
  })

  

  # blank start----

  # **provide default nodes and edges if necessary ----

  defaultNodes <- data.frame(
    # id = 1:2,
    # color.background=c("","") ,
    # color.border=c("","") ,
    # color.highlight.border = c("","") ,
    # color.highlight.background = c("","") ,
    # font.size=1:20,
    # frequency="1" ,
    # borderWidth=1:20,
    # shape=rep(20,"box",20)
    label = xc("one two"),
    details = xc("one two"),
    group = c("", ""),
    col1 = c("", ""),
    cluster = c("", ""),
    value = 0,
    level = 0,
    fun = "sumfun",
    type = ("<U+25E8>"),
    is_context = c(F, F),
    clusterLabel = c("", ""),
    stringsAsFactors = F
  )



  #   the default graph object with no nodes or edges
  values$graf <- tbl_graph(
    defaultNodes[0, ],
    defaultEdges[0, ]
  )



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


  # Import/Statements panel -------------------------------------------------

  # create statements table to allow user edit of statements----
  output$statements <- renderRHandsontable({
    vs <- values$statements

    if (!is.null(valuesCoding$nodesSelected)) {
      if ("" != (valuesCoding$nodesSelected)) {
        ids <- values$grafAgg2 %>%
          nodes_as_tibble() %>%
          filter(id == valuesCoding$nodesSelected) %>%
          pull(statement) %>%
          str_remove_all("NA") %>%
          str_split(",") %>%
          `[[`(1)

        vs <- vs %>%
          filter(statement %in% ids)
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
      whichtarg <- values$grafAgg2 %>%
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
      select(fromLabel, toLabel, -full.quote, everything(), full.quote) %>%
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

  output$edge2 <- renderRHandsontable(if (!is.null(values$grafAgg2)) {
    doNotification("Creating edges2 table")
    rhandsontable(values$grafAgg2 %>% edges_as_tibble(), height = 700, rowHeaders = FALSE, usetypes = T) %>%
      hot_context_menu(allowRowEdit = T) %>%
      hot_cols(columnSorting = TRUE) %>%
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_rows(fixedRowsTop = 1)
  })

  output$node2 <- renderRHandsontable(if (!is.null(values$grafAgg2)) {
    doNotification("Creating nodes2 table")
    rhandsontable(values$grafAgg2 %>% nodes_as_tibble(), height = 700, rowHeaders = FALSE, usetypes = T) %>%
      hot_context_menu(allowRowEdit = T) %>%
      hot_cols(columnSorting = TRUE) %>%
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_rows(fixedRowsTop = 1)
  })

  # Display / settings panel ----


  output$condFormattingOutput <- renderUI({

    gr <- xc("label frequency sex Positive notForwards older female ava avp")

    vals <- values$settingsConditional %>%
      mutate(type = if_else(str_detect(attribute, "node"), "node", "edge"))

    lapply(seq_along(all_attributes), function(n) {
      thisAttribute <- all_attributes[n]
      thisType <- ifelse(n > length(node_names), "edge", "node")
      if (is.na(thisAttribute)) browser()
      vals2 <- vals %>% filter(attribute == thisAttribute)
      attribute_clean <- str_replace_all(thisAttribute, "\\.", "_") # because of js condition later

      div(
        div(
          p(thisAttribute %>% str_replace_all("_|\\."," "), style = "width:160px"),
          style = "display:inline-block;vertical-align:top"
        ),



        if (thisAttribute %>% str_detect("color")) {
          div(
            colourInput(paste0("conditional_value_", thisAttribute),
              label = NULL,
              palette = "limited",
              showColour = "background",
              value = vals2 %>% pull(value),
              allowedCols = allcols1
            ),
            style = "display:inline-block;vertical-align:top;width:50px"
          )
        } else {
          div(
            textInput(paste0("conditional_value_", thisAttribute), NULL, value = vals2 %>% pull(value), width = "120px"),
            style = "display:inline-block;vertical-align:top;width:100px",class="conditional_text"
          )
        },

        div(
          selectInput(paste0("conditional_selector_", attribute_clean), label = NULL, choices = c("always", "conditional on ..."), selected = vals2 %>% pull(selector), width = "120px"),
          style = "display:inline-block;vertical-align:top"
        ),
        conditionalPanel(
          paste0("input.conditional_selector_", attribute_clean, '=="conditional on ..."'),
          div(
            div(
              selectInput(paste0("conditional_var_", thisAttribute), label = NULL, choices = gr, selected = vals2 %>% pull(var), width = "120px")
              ,
              style = "display:inline-block;vertical-align:top"
            ),
            
            div(
              selectInput(paste0("agg_type_", thisAttribute), label = NULL, choices = xc("sum mean"), width = "120px"),
              style = "display:inline-block;vertical-align:top"
            ),
            

            div(
              p("up to"),
              style = "display:inline-block;vertical-align:top"
            ),
            if (thisAttribute %>% str_detect("color")) {
              div(
                colourInput(paste0("conditional_value2_", thisAttribute),
                  label = NULL,
                  palette = "limited",
                  showColour = "background",
                  value = vals2 %>% pull(value2),
                  allowedCols = allcols1
                ),
                style = "display:inline-block;vertical-align:top;width:50px"
              )
            } else {
              div(
                textInput(paste0("conditional_value2_", thisAttribute), NULL, value = vals2 %>% pull(value2), width = "120px"),
                style = "display:inline-block;vertical-align:top"
              )
            },
            style = "display:inline-block;"
          ),style = "display:inline-block;background-color:#EEFFEE;margin-left:20px;padding:10px"
        )
        # ,
        # hr(style = "margin:5px")
      ,class="conditional-row")
    })
  })


  output$upConditionalBut <- renderUI({
    actionButton("upConditional", "Update")
  })

  observeEvent(input$upConditional, ignoreInit = T, {
    # browser()
    values$settingsConditional <- make_settingsConditional(input, values$settingsConditional)
  })

  

  # filters, don't work at the moment ---------------------------------------

  
  output$filters <- renderUI({
    clusters <- values$graf %>%
      nodes_as_tibble() %>%
      pull(cluster) %>%
      unique()

    tagList(
      lapply(colnames(values$statements %>% select(-text)), function(y) {
        x <- values$statements[[y]]
        u <- unique(x) %>% na.omit()
        if (length(u) > 1 & length(u) < 12) {
          div(checkboxGroupButtons(paste0("filters", y), y, choices = sort(u), selected = u), style = "display:inline-block;vertical-align:top")
        }
      }),
      if (!is.null(clusters) && length(clusters) > 1) {
        tagList(
          div(checkboxGroupButtons("filterscluster", "cluster", choices = sort(values$graf %>% nodes_as_tibble() %>% pull(cluster) %>% unique())), style = "display:inline-block;vertical-align:top")
        )
      }
    )
  })


  ######## where is the observe event for filters, including for cluster filter?

  # produce  settings widgets --------------------------------

  observeEvent(input$settingsTableGlobalUp, {
    # browser()
    vals <- values$settingsGlobal
    vs <- values$settingsGlobal %>% mutate_all(as.character)
    output$inputs <- renderUI({
      lapply(1:nrow(vs), function(x) {
        row <- vs[x, ]
        rg <- replace_na(row$widget, "")
        rt <- paste0(row$type, row$setting, collapse = "")
        rt <- replace_na(rt, "")
        # if(rg=="color")  {checkboxInput("asdf","asdf")}
        div(
          if (rg == "color") {
            # browser()
            colourInput(paste0("input", rt), rt,
              palette = "limited",
              showColour = "background",
              value = if (is.null(findset(rt,vals))) findset(rt,vals) else findset(rt,values),
              allowedCols = allcols1
            )
          }
          else if (rg == "slider") {
            sliderInput(paste0("input", rt), rt, min = 0, max = 100, value = findset(rt,vals))
          }
          else if (rg == "checkbox") {
            # browser()
            checkboxInput(paste0("input", rt), rt, value = as.logical(findset(rt,vals)))
          }
          else
          if (rg == "input") paste0(row$type, row$setting, collapse = ""),
          style = "display:inline-block;"
        )
      })
    })
  })


  output$settingsTableGlobal <- renderRHandsontable({
    vs <- values$settingsGlobal %>% mutate_all(as.character)

    ds <- defaultSettingsGlobal %>% mutate_all(as.character)

    vs <- bind_rows(vs, ds) %>%
      distinct(type, setting, .keep_all = T)
# browser()

    rhandsontable(vs %>%
      mutate(type = factor(type)), height = NULL, rowHeaders = FALSE, usetypes = T) %>%
      hot_context_menu(allowRowEdit = T) %>%
      hot_cols(colWidths = c(80, 120, 250, 80))
  })


  observeEvent(input$settingsTableGlobalUp, {
    doNotification("updating from settingsTableGlobal")
    # browser()
    values$settingsGlobal <- hot_to_r(input$settingsTableGlobal)
  })


  

  # downloads panel  -----------------------------------------------------------

  output$downloads <- renderUI({
    # browser()
    doNotification("Creating library list")
    if (!is.null(fileFromURL())) {
      name <- gsub("www/", "", fileFromURL())
      tagList(
        h4("Download your files"),
        h5("CSV files"),
        lapply(csvlist, function(x) {
          tagList(a(href = paste0(name, "-", x, ".csv"), x), hr())
        }),
        h5("Graphic files"),
        div(

          actionButton("png", "Create new files", icon = icon("picture")),
          style = "display:inline-block;margin-right:50px;width:10px"
        ),
        hr(),
        tagList(
          a(href = paste0(name, "", ".html"), "interactive html file"), hr(),
          a(href = paste0(name, "", ".png"), "high-quality png file"), hr()
        )
      )
    }
  })

  # main panel  -----------------------------------------------------------
  # description below graph
  output$description <- renderUI({
    x <- findset("diagramdescription",values)
    if (x != "") {
      div(p(x), style = "padding:10px;background-color:whitesmoke;margin-top:10px;border-radius:5px")
    } else {
      ""
    }
  })


  # colorLegend -------------------------------------------------------------
  # just a placeholder right now

  output$colourLegend <- renderPlot({
    emptyplot(main = "Percentage of women mentioning each factor and each link                 Percentage of younger people mentioning each factor", adj = 0)
    colorlegend(
      posx = c(0, 0.1),
      col = intpalette(c("blue", "red"), 100),
      zlim = c(0, 100), zval = c(0, 25, 50, 75, 100)
    )

    colorlegend(
      posx = c(0.5, 0.6),
      col = intpalette(c("black", "white"), 100),
      zlim = c(0, 100), zval = c(0, 25, 50, 75, 100)
    )
  })

  # AGGREGATE ---------------------------------------------------------------------------
  # the long process of aggregating values$graf into values$grafAgg2, adding formatting etc


  observe({

    # make this code run whenever the tab$change reactive triggers
    tab$change
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

        physics = findset("diagramphysics", v = vals)
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
      )
# browser()
    values$net <- vn

    doNotification("Produced viz")
  })

  observe({
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





  # report -----------------------------------------------------
# not important. 
  output$reportTable <- renderFormattable({
    if ("frequency" %in% colnames(values$net$x$nodes)) {
      values$net$x$nodes %>%
        transmute(label, frequency = replace_na(frequency, 0), from = replace_na(from.frequency_sum, 0), to = replace_na(to.frequency_sum, 0)) %>%
        arrange(desc(frequency)) %>%
        formattable(list(
          `frequency` = color_bar("#AAEEAA"),
          `from` = color_bar("#EEAAAA"),
          `to` = color_bar("#AAAAEE")
        ))
    } else {
      tibble(warning = "You have to merge arrows with variables in order to get a report.") %>%

        formattable(list())
    }
  })

  # floating widgets -----------------------------------------------------
  output$floatingWidgets <- renderUI({
    # div(
    # div(actionButton("fitaction", label=NULL,icon=icon("arrows-alt")), style = "display:inline-block;margin-right:20px"),
    # class = "bigpicbut" ,style="z-index:999 !important")
    # h1("asdfasdfasdf")
  })

  # observe(if(exists("fileFromURL")){
  # })


  
  # observe save png button -------------------------------------------------------------
  #  saves a png and html file which are not yet downloadable


  observeEvent(input$png, {
    # browser()
    doNotification("Saving file", 2)
    fn <- paste0(input$titl, ".html")
    visSave(values$net, fn, selfcontained = T)
    doNotification("Saved file", 2)
    file.copy(fn, paste0("www/", fn), overwrite = T) # because there is a bug with htmlwidgets saving to other directories
    file.remove(fn)
    webshot::webshot(file = paste0("www/", input$titl, ".png"), url = paste0("www/", input$titl, ".html"))
    doNotification("Saved png", 2)
  })


  

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
      pull(statement)

    if (!is.null(vce) & !input$onlyThisStatement) updatePageruiInput(session, "pager", page_current = as.numeric(targetStatement))
  })




  observe({
    output$testBut <- if (!is.null(valuesCoding$edgesSelected)) {
      renderUI({
        actionButton("testBut", "Edit selected arrows")
      })
    } else {
      renderUI({
        p()
      })
    }
  })



  



  output$keypr <- renderPrint({
    input$keypressed
  })

  output$push <- renderUI({
    tagList(
      actionButton("statementsTableUp", "Update"),
      rHandsontableOutput("statements")
    )
  })


  output$quotesOutput <- renderUI({
    ins <- valuesCoding$nodesSelected
    inse <- valuesCoding$edgesSelected
    if (!is.null(ins)) {
      quotes <- values$net$x$nodes %>%
        filter(id %in% ins) %>%
        pull(quote) %>%
        str_remove_all(",NA") %>%
        paste0(collapse = "; ")
    }

    else if (!is.null(inse)) {
      quotes <- values$net$x$edges %>%
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


  output$widthControlOutput <- renderUI({
    actionButton("widthControl", NULL, icon = icon("arrows-h")) %>% bs_embed_tooltip("Change the width of the panels")
  })

  observeEvent(input$widthControl, {
    toggleClass("app-content", "col7")
    toggleClass("mainPanel", "col5")
    toggleClass("net", "maindivnet-small")
  })
  
  
  

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")
}
