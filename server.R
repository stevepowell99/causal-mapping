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

  
  
  source("modules/user.r",local=T)
  source("modules/coding.r",local=T)
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
  
  
  
  
  # create coding graf ------------------------------------------------------
  
  
  observe({
    # make this code run whenever the tab$change reactive triggers
    # tab$change
    
    if (nrow(nodes_as_tibble(req(values$graf))) > 0) {
      doNotification("starting aggregation")
      
      values$codingGraf <- convert_graf_to_codingGraf(values$graf,values$statements,values$statements_extra)
    }
  })
  
  
  # codingGraf to values$netCoding --------------------------------------------------
  
  
  observe( {
    if (!is.null(values$codingGraf)){
      vga <- req(values$codingGraf)
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
        
        
        values$netCoding <- vn
        
        doNotification("Produced viz")}
    } else doNotification("No edges")
  })
    
    
  observe( {
    output$netCoding <- renderVisNetwork({
      doNotification("render coding viz")
      values$netCoding
    })
  })
  
  
  
  
  observe({
    req(values$filterVec)
    # browser()
    doNotification("Starting second agg")
    
    
    this_tab <- input$sides
    
    if (nrow(nodes_as_tibble(req(values$codingGraf))) > 0 & this_tab!="Code") {
      # browser()
      values$grafAgg2 <- convert_codingGraf_to_graf2(values$codingGraf,values$filterVec,values$settingsGlobal,this_tab,input,values$settingsConditional)
      doNotification("Aggregated")
    }
  })

  
  
  # grafAgg2 to values$net --------------------------------------------------
  
  
  
  observe( {
    # browser()
    
    req(values$grafAgg2)
    req(input$sides)
    if (!is.null(values$grafAgg2) & input$sides=="Display"){
      vga <- req(values$grafAgg2)
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
      
      
      values$net <- vn
      
      doNotification("Produced main viz")
    }
    
    # browser()
    
    output$net <- renderVisNetwork({
      doNotification("render viz")
      values$net
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
