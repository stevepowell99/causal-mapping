server <- function(input, output, session) {


  # TODO pool package to manage
  
  # sql ---------------------------------------------------------------------
  
  
  if(T) {
    con <- isolate(dbConnect(RSQLite::SQLite(), "CMA"))
  } else {
    con <-  isolate(DBI::dbConnect(RMariaDB::MariaDB(), user = "admin", password = "barnulf99",dbname = "CMA", host = "db1.c3sdt4rwfkjt.us-west-2.rds.amazonaws.com", port = 3306))
  }
  
  track_usage(storage_mode = store_json(path = "logs/"))
  
  
  
  
  # reactive values ---------------------------------------------------------

  values <- reactiveValues() # nearly all reactive values are stored in values$...
  values$statements <- default.statements
  
  
  values$settingsConditional <- defaultSettingsConditional
  values$settingsGlobal <- defaultSettingsGlobal
  values$filterVec <- T
  values$highlightedText <- "" # part of a system to copy any text highlighted with mouse in browser i.e. from interview quotes and insert into the edge information
  values$pag <- 1 # stores value of pager in Code panel
  
  
  valuesCoding <- reactiveValues(fromStack = NULL, tot=9,toStack = NULL, foundIDs = NULL, readyForEndArrow = F, nodeSelected = NULL, edgeSelected = NULL)
  # i added an extra reactive variable because you said not to have them all in one :-)    Not sure if they need splitting up more
  
  # This keeps a record of the previous page, and so ensures that we only update
  # the visNetwork if we transition to/from the 'Code' tab.
  #
  # You can change when updates happen by either updating the conditions here,
  # or adding dependencies further down, like I have for tab$change
  tab <- reactiveValues(old = "", change = 0)
  
  
  
  source("modules/user.r",local=T)
  source("modules/coding.r",local=T)
  source("modules/codingPanel.r",local=T)
  source("modules/codingEdits.r",local=T)
  source("modules/stats.r",local=T)
  source("modules/display.r",local=T)
  source("modules/settingsGlobal.r",local=T)
  source("modules/infoBar.r",local=T)
  source("modules/downloads.r",local=T)
  source("modules/reports.r",local=T)
  source("modules/tableWidgets.r",local=T)
  source("modules/mainPanelWidgets.r",local=T)
  
  
  #   the default graph object with no nodes or edges
  values$graf <- tbl_graph(
    defaultNodes[0, ],
    defaultEdges[0, ]
  )
  
  
  observeEvent(input$sides, {
    if (tab$old == "Code" ) {
      tab$change <- tab$change + 1
    } else if (input$sides == "Code") {
      tab$change <- tab$change + 1
    }
    tab$old <- input$sides
  })
  
  values$obsList <- list() #   to show all the statemtns from one source
  
  
  # create coding graf ------------------------------------------------------
  
  
  observe({
    # make this code run whenever the tab$change reactive triggers
    # tab$change
    
    if (nrow(nodes_as_tibble(req(values$graf))) > 0) {
      doNotification("starting aggregation")
      
      values$codeGraf <- convert_graf_to_codeGraf(values$graf,values$statements,values$statements_extra,values$settingsGlobal)
    }
  })
  
  
  # codeGraf to values$codeNet --------------------------------------------------
  
  
  observe( {
    if (!is.null(values$codeGraf)){
      vga <- req(values$codeGraf)
      this_tab <- isolate(input$sides)
      vals <- values$settingsGlobal
      
      
      
      doNotification("started coding viz")
      # browser()
      
      # if (is.null(values$pag)) {
      #   values$pag <- 1
      # }
      # browser()
      if(vga %>% edges_as_tibble() %>% nrow %>% `>`(0)){
  
        vn <- render_network(vga,vals,type="Coding")
        
        vn$x$nodes <- dag_layout(vn$x$nodes)
        
        
        values$codeNet <- vn
        
        doNotification("Produced viz")}
    } else doNotification("No edges")
  })
    
    
  observe( {
    output$codeNet <- renderVisNetwork({
      doNotification("render coding viz")
      values$codeNet
    })
  })
  
  
  
  
  observe({
    req(values$filterVec)
    # browser()
    doNotification("Starting second agg")
    
    
    this_tab <- input$sides
    
    if (nrow(nodes_as_tibble(req(values$codeGraf))) > 0 & this_tab!="Code") {
      # browser()
      values$displayGraf <- convert_codeGraf_to_displayGraf(values$codeGraf,values$filterVec,values$settingsGlobal,this_tab,input,values$settingsConditional)
      doNotification("Aggregated")
    }
  })

  
  
  # displayGraf to values$displayNet --------------------------------------------------
  
  
  
  observe( {
    # browser()
    
    req(values$displayGraf)
    req(input$sides)
    if (!is.null(values$displayGraf) & input$sides=="Display"){
      vga <- req(values$displayGraf)
      this_tab <- isolate(input$sides)
      vals <- values$settingsGlobal
      
      fvw <- ifelse(this_tab=="Code",findset("variablecoding.width",vals),findset("variablewidth",vals))
      
      
      doNotification("started viz")
      # browser()
      
      if (is.null(values$pag)) {
        values$pag <- 1
      }
      # browser()
      vn <- render_network(vga,vals,type="Display")
      # browser()
      vn$x$nodes <- dag_layout(vn$x$nodes)
      
      
      values$displayNet <- vn
      
      doNotification("Produced main viz")
    }
    
    # browser()
    
    output$net <- renderVisNetwork({
      doNotification("render viz")
      values$displayNet
    })
    
  })
  
  
  

    # the useful "interrupt" button in bottom righthand corner
  observeEvent(input$Interrupt, {
    browser()
  })

  

  output$keypr <- renderPrint({
    input$keypressed
  })


  
  

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")
}
