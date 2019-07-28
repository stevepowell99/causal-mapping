server <- function(input, output, session) {
  autoInvalidate <- reactiveTimer(2000)
  
  values <- reactiveValues()                         # nearly all reactive values are stored in values$...
  values$pag <- 1                         # stores value of pager in Code panel
  values$statements <- default.statements
  values$clickArrow <- F                         # no idea 
  values$crowd = F
  
  
  values$settingsConditional <- defaultSettingsConditional 
  values$settingsGlobal <- defaultSettingsGlobal
  
  values$highlightedText <- ""                         # part of a system to copy any text highlighted with mouse in browser i.e. from interview quotes and insert into the edge information
  
  observeEvent(input$highlightedText,{
    if (!is.null(input$highlightedText)) values$highlightedText <- input$highlightedText
    # if (!is.null(input$highlightedText)) if ("" != (input$highlightedText)) values$highlightedText <- paste0(values$highlightedText," ... ",input$highlightedText)
  })
  
  loaded <- F                         # whether loaded from url permalink
  makeReactiveBinding("loaded")
  
  first <- T
  makeReactiveBinding("first")
  
  
  valuesCoding <- reactiveValues(fromStack=NULL,toStack=NULL,foundIDs=NULL,readyForEndArrow=F)

  values$issaved=F
  
  # This keeps a record of the previous page, and so ensures that we only update
  # the visNetwork if we transition to/from the 'Code' tab.
  # 
  # You can change when updates happen by either updating the conditions here,
  # or adding dependencies further down, like I have for tab$change
  tab <- reactiveValues(old = "", change = 0)
  observeEvent(input$sides, {
    if(tab$old == "Code"){
      tab$change = tab$change+1
    } else if(input$sides == "Code"){
      tab$change = tab$change+1
    }
    tab$old = input$sides
    
  })
  
  inputtitl <- ""
  makeReactiveBinding("inputtitl")
  
  # ts <- reactiveValues(counter = 0)
  
  # findset function to transfer user settings to values$settings -----------
  # this is important and a bit clunky. I think it sucks in the two settings 
  # tables, combines them with defaults in case anything is missing, and then
  #  providess the value of the setting sought. probably most ripe for 
  #  rationalisation
  
  findset <- function(tex, v=values) {
      x <-  v$settingsGlobal %>% 
        bind_rows(defaultSettingsGlobal) %>% 
        group_by(type,setting) %>% 
        summarise_all(.funs = funs(first))
    
      x <- x %>%
        mutate_all(replaceNA) %>%
        mutate(labs = paste0(type, setting)) %>%
        filter(labs == tex) %>%
        pull(value) %>%
        last()
    
    if (is.na(x)) {
      doNotification(paste0(x, " is not in settings"))
      # browser()
      
    }
    x
  }
  
  
  
  
  #the useful "interrupt" button in bottom righthand corner
  observeEvent(input$Interrupt, { 
    browser()
  })
  
  # first of zillions of handsontables.
  observeEvent(input$statementsTableUp, {                         
    
    values$statements <-  hot_to_r(input$statements) %>%
      mutate(text = str_replace(text, "\'", "")) %>% 
      select(1:(ncol(default.statements))) %>%
      mutate(statement = row_number())
    # filter(text != "") %>%
    bind_rows(default.statements[1, ])
    
  })
  
  # update from permalink or example url; -----------
  observe(isolate({
    first <- F
    
    # browser()
    query <- isolate(parseQueryString(session$clientData$url_search))
    
    qh <- query[["help"]]
    if (!is.null(qh)) {
      if (qh == "yes") {
        toggleModal(session,
                    modalId =
                      "cheatModal", toggle = open
        )
      }
    } else {
      ql <- query[["permalink"]]
      qe <- query[["example"]]
      
      if (!is.null(qe)) ql <- qe
      # ql=tolower(ql)
      
      # if(!loaded)currentURL<<-ql
      
      # if (is.null(ql))
      #   ql="default"
      
      if (T) {
        values$current <- ql
        
        if (!is.null(ql)) {
          if (!loaded) {
            
            
            doNotification("Loading files",2)
            
            values$crowd=str_detect(ql,"-crowd")
            if(values$crowd) updateCheckboxInput(session,"crowd",value=T)
            filename <- paste0("www/", ql)
            
            # browser()
            if (filename != "" & !file__exists(paste0(filename,"-nodes.csv"))) {
              createAlert(
                session,
                "notfound",
                title = "Sorry, couldn't find that link.",
                content = "Let me know if you need help: steve@pogol.net",
                append = FALSE)
            }
            
            for(fn in csvlist){
              
              fnn=paste0(filename,"-",fn,".csv")
              
              if(storage=="gsheets"){
                
                doNotification("Reading from google drive",2)
                # browser()
                fnn=paste0(filename,"-",fn) %>% str_remove("^www/")
                fnnn=drive_find(pattern=fnn,type = "spreadsheet")[1,]
                local_file <- drive_download(fnnn, path = tempfile(fileext= ".xlsx"))
                read_sheet(fnnn)
                values[[fn]] <- read_excel(local_file$local_path)   #workaround using readxl because auth not yet available on package
                
                doNotification("Finished reading from google drive",2)
              } else if(storage=="local"){
                
                if (file__exists(fnn)) {
                  values[[fn]] <- read__csv((fnn))
                } 
              }
            }
            
            # browser()
            
            # need to add any new columns
            
            nodes <- values$nodes %>% 
              bind_rows(defaultNodes %>% filter(F)) %>% 
              mutate(cluster=replace_na(cluster,"")) %>% 
              mutate(clusterLabel=replace_na(clusterLabel,""))
            
            edges <- values$edges %>%
              bind_rows(defaultEdges %>% filter(F)) %>%
              mutate(trust = as.numeric(trust)) %>%
              mutate(strength = as.numeric(strength)) %>%
              mutate(statement = as.numeric(statement))
            
            values$graf <- tbl_graph(nodes, edges)
            

            
            doNotification(glue("Loaded{nrow(values$graf %>% nodes_as_tibble)} variables from permalink"))
            
            # browser()
            
            updateTextInput(session, "titl", value = ql)
            values$current <- filename
            # writeLog(c("from url:", session$token, ql))
            
            # if(!grepl("-public",filename) & file.info(filename)$mtime %>% as.Date<as.Date("2016-10-16")) createAlert(session, "found",  title = "Link found",content="Diagrams created in the old version of Theory Maker may look different in Theory Maker 2. Let me know if you have any problems: steve@pogol.net", append = FALSE)
            # browser()
          }
        }
      
        loaded <<- T
      }
      
      # update from text------------
      
      qt <- query[["text"]]
    }
  })
  )
  
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
    type = ("◨"),
    is_context = c(F,F),
    clusterLabel = c("", ""),
    stringsAsFactors = F
  )
  

  
  #   the default graph object with no nodes or edges
  values$graf <- tbl_graph(
    defaultNodes[0,],
    defaultEdges[0,]
  ) 
  
  # ++ Import/Statements panel -------------------------------------------------
  
  # ++create statements table to allow user edit of statements----
  output$statements <- renderRHandsontable({
    vs <- values$statements
    
    if(!is.null(input$net_selected)){
    if(""!=(input$net_selected)){
      ids <- values$grafAgg2 %>% 
        nodes_as_tibble() %>% 
        filter(id==input$net_selected) %>% 
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
  
  # file upload ----
  observeEvent(input$up.nodes, {
    
    # browser()
    req(input$up.nodes)
    df <- read_csv(input$up.nodes$datapath,T) %>%
      bind_rows(defaultNodes %>% filter(F)) %>% 
      mutate(label=strip_symbols(label)) %>% 
      tidy_colnames()
    
    values$graf <- tbl_graph(df, defaultEdges)
    doNotification("Updated variables, now update your edges")
  })
  
  
  observeEvent(input$up.edges, {
    req(input$up.edges)
    df <- read_csv(input$up.edges$datapath)[, ] %>% 
      mutate_all(strip_symbols) %>% 
      tidy_colnames()
    
    if(is.null(input$up.nodes)){
      nodes <- tibble(label=(unique(unlist(c(df$from,df$to))))) %>% 
        bind_rows(defaultNodes %>% filter(F))
    } else {
      nodes <- values$graf %>% nodes_as_tibble()
    }
    # max <- (values$graf %>% nodes_as_tibble() %>% nrow()) + 1
    
    
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
    vstat <- read_csv(input$up.statements$datapath)[, ]  %>% 
      tidy_colnames()
    
    # vstat <- values$statements  
    # fs <- findset("diagramsplitColumnNames")
    if(str_detect(colnames(vstat),"\\.") %>% any){
      
      col <- colnames(vstat) %>%
        str_detect("\\.") %>%
        which %>%
        first
      
      fsx= str_split(colnames(vstat)[col],"\\.")[[1]] %>% str_trim
      
      if(!is.na(col)){
        vstat <- vstat %>%
          separate(col,into=fsx,sep="\\.")
      }
      # values$statements <- vstat
    } else 
      if(str_detect(colnames(vstat),",") %>% any){
        
        col <- colnames(vstat) %>%
          str_detect(",") %>%
          which %>%
          first
        
        fsx= str_split(colnames(vstat)[col],",")[[1]] %>% str_trim
        widths <- fsx %>%  str_extract("[0-9]*$")
        names <- fsx %>%  str_remove("[0-9]*$")
        
        if(!is.na(col)){
          vstat <- vstat %>%
            separate(col,into=names ,sep=cumsum(widths),convert=T)
        } 
      }
    # %>%
    #   bind_rows(default.statements %>% filter(F))
    # 
    # if (ncol(vs) > 9) vs <- vs[, 1:8]
    colnames(vstat)[1]="text"
    values$statements  <- vstat %>%
      mutate(statement = row_number())
    
    
    # <- vs
    doNotification("Updated statements")
    # browser()
  })
  
  # ++ Code panel -----------------------------------------------------------
  
  # create values$tot -------------------------------------------------------
  
  #   just the total number of statements, to provide the maximum value of the pager
  observe(({                                  
    if (!is.null(values$statements)) {
      # tot=nrow(values$statements)
      # 
      if(!("statement" %in% colnames(values$statments))) {
        values$statements = values$statements %>% mutate(statement=row_number())
      }
      tot <- values$statements %>%
        pull(statement) %>%
        na.omit() %>%
        as.numeric() %>%
        max()
      values$tot <- tot
      # browser()
    } # if(!is.na(tot)) if(tot>1) browser()
    else {
      values$tot <- 9
    }
    
  }))
  
  # **Pager----
  
  #   the pager allows user to view interview statements one by one
  output$pagerBig <- renderUI({                                  
    
    tagList(
      div(
        pageruiInput("pager", pages_total = values$tot),
        style = "display:inline-block;"
      ),
      div(
        actionButton("firstuncoded", "First uncoded"),
        style = "display:inline-block;"
      ),
      div(
        # hilarious widget which scrolls through statements one by one
        if(F)sliderInput(
          "timeslider",
          NULL,
          min = 0,
          max = values$tot,
          value = 0,
          animate = animationOptions(loop = T),
          ticks = F,
          width = "50px"
        ),       
        style = "display:inline-block;"
      ),
      div(
        # if("source" %in% colnames(values$statements))
          div(actionButton("resetSelection", label = NULL,icon=icon("refresh")),style="position:absolute;right:25px") ,                                 
          div(actionButton("overview_col", label = "Read more"),style="display:inline-block") ,                                 #   if one interview source has made more than one statement, show all of them
          div(checkboxInput("onlyThisStatement", label = "Only this",value = T),style="display:inline-block") ,                                 #   if one interview source has made more than one statement, show all of them
        style = "display:inline-block;margin-left:20px"
      )
    )
  })
  
  values$obsList <- list()                                                #   to show all the statemtns from one source
  
  
  observeEvent(input$overview_col,{
    vs=values$statements
    # browser()
    
    # col=findset("diagramoverview_column")
    if(!("source" %in% colnames(vs))){
      
      vs$source <- 1
    }
    
    vs <- vs %>% 
      mutate(newSource=ifelse(source!=lag(source),source,"")) %>% 
      mutate(newSource=ifelse(is.na(newSource),source,newSource))
    
      pointer=vs$source[req(values$pag)]
      content=vs$statement[(vs$source==pointer)]
      # content=vs$statement[]

      showModal(modalDialog(
      title = "All statements",footer=NULL,easyClose = T,size="l",
      tagList(
        checkboxInput("showAllSources","Show all sources"),
        lapply(seq_along(content),function(y)if(!is.na(vs$text[[y]])){
          x <- content[[y]]
          
          btName=paste0("gosource",x)
          
          if (T|is.null(values$obsList[[btName]])) {
            # make sure to use <<- to update global variable obsList
            values$obsList[[btName]] <- observeEvent(input[[btName]], {
              updatePageruiInput(session,"pager",page_current = as.numeric(x))
            })
          }
    # browser()
          tagList(
            if(vs$newSource[y]=="")div() else div(h3(paste0("Source: ",vs$newSource[y]))),
            div(h4(paste0("Statement: ",vs$statement[y]))),
            # actionButton(paste0("gosource",x),"Go!"),
            if(str_detect(vs$text[x],"pdf$")) {
              tags$iframe(src="pdf.pdf",style="height:800px; width:100%;scrolling=yes")
            } else {
              div(id=paste0("allStatements-",y),actionLink(btName,label = vs$text[x]))
            },
            br()
          )
        })
      )
    ))
  },ignoreInit=T)
  
  # ** display statements one by one ----
  
  output$displayStatementPanel <- renderUI({
    # browser()
    
    quote <- values$graf %>% edges_as_tibble() %>% 
      filter(statement==values$pag) %>% 
      pull(quote) %>% 
      replace_na("")
    
    
    tagList(
      icon("quote-left"),
      values$statements$text[values$statements$statement==values$pag] %>% 
          highlight_text(quote) %>% 
        HTML %>% div(class = "textbanner", id = "textbanner"),
      # span(values$statements$text[values$statements$statement==values$pag], class = "textbanner", id = "textbanner"),
      # span(add_highlight((ve$quote[ve$statement==values$pag])[1],(values$statements$text[values$statements$statement==values$pag])), class = "textbanner", id = "textbanner"),
      hr()
    )
  })
  
  observeEvent({c(input$pager,input$timeslider)},{
      if (F & !is.null(input$pager)) {
        values$pag <- input$pager[[1]]

        if (input$timeslider > 0 & nrow(values$statements) > 2) updatePageruiInput(session, "pager", page_current = input$timeslider)
        if(!is.null(input$pager) && !is.null(input$quote))updateTextAreaInput(session = session,inputId = "quote",value="",placeholder="quote")
      }
    })

  observeEvent(input$firstuncoded, {
    # browser()
    slist <- values$statements %>%
      filter(text != "") %>%
      pull(statement) %>%
      na.omit() %>%
      as.numeric() %>%
      sort()
    
    elist <- edges_as_tibble(values$graf)$statement %>% na.omit() %>% as.numeric()
    
    min <- slist[sapply(slist, function(y) !(y %in% elist))] %>% min()
    
    if (is.infinite(min)) {
      doNotification("You have coded everything!",level=2)
    } else {
      updatePageruiInput(session, "pager", page_current = min)
    }
  })
  
  
  
  
  observeEvent(input$updateE2, {
    ectizeInput(session, "new1_edge", selected = input$new2_edge)
  })
  
  observeEvent(input$updateE3, {
    updatePageruiInput(session,inputId = "pager",page_current = as.numeric(values$pag)+1)
  })
  
  
  
  # fromStack display
  
  output$fromStackInfo=renderUI({
    if(0<length(valuesCoding$fromStack))p(paste0("Begin arrow with: ",valuesCoding$fromStack %>% paste0(collapse=", ")))
  })
  
  
  # Add edges widget----
  
  output$add_edges_widget <- renderUI({
    varlist=values$graf %>% nodes_as_tibble() %>% pull(label) %>% unique() %>% as.character()
    varlist <- na.omit(varlist)

    tagList(
      div(
        div(
          div(style = "display:inline-block;width:5%"),
          div(textInput("arrLabel", NULL, value = "", placeholder = "label"), style = "display:inline-block;"),
          div(style = "display:inline-block;width:5%"),
          div(selectizeInput("definition.type", NULL, choices = c("", "Defined, directed", "Defined, undirected")), style = "display:inline-block;width:20%"),
          div(selectizeInput("function.type", NULL, choices = c("+", "-", "NECC","SUFF")), style = "display:inline-block;width:20%"),
          div(textAreaInput("quote", NULL, value = values$highlightedText, placeholder = "quote",rows=3,width="100%"), style = ""),
          style = "margin-top:20px"
          
        ),
        div(
          id = "sliders",
          # div(actionLink("flip", "Flip"), style = "display:inline-block;"),
          div(style = "display:inline-block;width:5%"),
          div(sliderInput("strength", "Strength", min = 0, max = 1, step = .25, value = .5, ticks = F), style = "display:inline-block;width:40%"),
          div(style = "display:inline-block;width:5%"),
          div(sliderInput("trust", "Trust", min = 0, max = 1, step = .25, value = .5, ticks = F), style = "display:inline-block;width:40%"),
          div(style = "display:inline-block;width:5%"),
          div(sliderInput("confidence", "Confidence", min = 0, max = 1, step = .25, value = .5, ticks = F), style = "display:inline-block;width:40%"),
          style = ""
        ),
        style="padding:10px;background-color:#EEFFEE;border:1px solid green"
      )
    )
  })
  
  # observeEvent(input$flip, { 
  #   updateSliderInput(session, inputId = "strength", value = -input$strength)
  # })
  
  
  output$combo <- renderUI(if(T){
    if (length(valuesCoding$fromStack) > 1) {
      div(selectizeInput("combo", "Arrows interact?",
                     choices = c("", "AND", "OR","min","max","SAME","mean","sum"), width = "150px", selected = NULL, 
                     options =
                       list(create = T, placeholder = "", onInitialize = I('function() { this.setValue(""); }')),
      ),style="background-color:#EEFFEE;padding:10px;border:1px gray solid")
    }
  })
  
  # output$addNewNodeButton=renderUI({
  #  if(req(input$selectBoxValue)!=""){
  #  if(length(valuesCoding$foundIDs)==0){
  #  div(
  #   actionButton("addNewNode","Add")
  #   ,
  #   style="display:inline-block"
  # )
  # }}
  #   })
  
  
  output$selectBox2Buttons=renderUI({
    # div(tagList(
    #   if(!is.null(input$net_selected)) {
    #     div(actionButton("selectFrom","Start with"),style="display:inline-block")
    #   }
    #   ,
    #   
    #   # if(!is.null(input$net_selected)) {
    #   #   div(actionButton("selectFromTo","Start arrow(s), finish at next selection"),style="display:inline-block")
    #   # }
    #   #   ,
    #   
    #   if(!is.null(input$net_selected) && length(valuesCoding$fromStack)>0){
    #     div(actionButton("selectTo","Finish at"),style="display:inline-block")
    #     # p(as.character(unlist(valuesCoding$fromStack)))
    #   }
    # ),style="display:inline-block")
  })
  
  
  output$selectBoxButtons=renderUI({
      varlist <- values$graf %>% nodes_as_tibble() %>% pull(label) %>% unique() %>% as.character()
      varlist <- na.omit(varlist)
      
      tagList(
      # textInput("selectBoxValue","",placeholder="Type to select or add variables")
        div(selectizeInput("selectBoxValue",
          label = NULL, selected = NULL, multiple = F,
          options =
            list(create = T, placeholder = "Type to select or add variables", onInitialize = I('function() { this.setValue(""); }')),
          choices = varlist,width="400px"
        ),style="display:inline-block"),
        # div(disabled(actionButton("addNode",NULL,icon=icon("plus"))),style="display:inline-block"),
        div((actionButton("addFrom",NULL,icon=icon("arrow-alt-circle-right"))),style="display:inline-block"),
        div((actionButton("addTo",NULL,icon=icon("arrow-alt-circle-right"))),style="display:inline-block")
        

    )
  })
  
  
  

  
  
  observeEvent(c(input$addFrom),ignoreInit = TRUE,{
    
    ns <- input$net_selected
    if(is.null(ns))ns <- ""
    if(""!=(ns) | !is.null(input$selectBoxValue)){
    # browser()
      
    isb=input$selectBoxValue
    if(""==isb)isb <- NULL
    inpfrom <- NULL
    
    
    if(!is.null(isb)){
      vg <- values$graf  
      inpfrom <- vg %>% 
        mutate(id=row_number()) %>% 
        filter(label==isb) %>% 
        pull(id)
      
      if(length(inpfrom)==0){
        values$graf <- vg %>% 
          bind_nodes(tibble(label=isb))
        doNotification("Adding Node",2)
        inpfrom=vg %>% nodes_as_tibble() %>% nrow() %>% `+`(1)
        
        tmp <- req(values$graf)             # has to be agg2 because of statements, but shouldn't be because some missed out
        vpag <- values$pag
        iot <- input$onlyThisStatement
        delay(4000,refresh_and_filter_net(tmp,vpag,iot))
        
      }
    }

    # browser()
    
    valuesCoding$fromStack <- c(ns,valuesCoding$fromStack,inpfrom) %>% unique
    
    valuesCoding$fromStack <- valuesCoding$fromStack[valuesCoding$fromStack!=""]
    # doNotification("from stack ",99)
    
    }
    
    
    visNetworkProxy("net") %>% 
      visSetSelection(unselectAll = TRUE)
    
    updateSelectizeInput(session = session,inputId = "selectBoxValue",selected="")      
    
    session$sendCustomMessage("refocus",list(NULL)) 
    

  })
  
  
  observeEvent(c(input$selectBoxValue,input$net_selected,valuesCoding$fromStack),{
    # browser()
    ins=input$net_selected
    isb=input$selectBoxValue
    vcf=valuesCoding$fromStack
    
    if(is.null(ins))ins=""
    if(is.null(isb))isb=""
    if(is.null(vcf))vcf=""
    if(length(vcf)==0)vcf=""
    if((""!=(ins) | ""!=isb) & ""==(vcf)) enable("addFrom") else disable("addFrom")
    if((""!=(ins) | ""!=isb) & ""!=(vcf)) enable("addTo") else disable("addTo")
  })
  
  
  
  observeEvent(input$addTo,{
    # browser()
    # valuesCoding$readyForEndArrow <- F
    
    # hiddenStore <- values$net$x$nodes$hidden
    
    if (!is.null(input$quote)) {
      qq <- input$quote %>% as.character()
    } else qq=""
    
    # qq="" #FIXME
    # browser()
    
    inpfrom <- req(valuesCoding$fromStack)
    inpto <- NULL
    
        # browser()
      
    isb <- input$selectBoxValue
    if(isb=="") isb <- NULL
    
    if(!is.null(isb)){
      
      vg <- values$graf 
       
      inpto <- vg %>% 
        mutate(id=row_number()) %>% 
        filter(label==isb) %>% 
        pull(id)
      
      if(length(inpto)==0){
        values$graf <- vg %>% 
          bind_nodes(tibble(label=isb))
        doNotification("Adding Node",2)
        inpto=vg %>% nodes_as_tibble() %>% nrow() %>% `+`(1)
      }
      }
    if(is.null(inpto)){
    inpto <- req(input$net_selected)[1]
    }
    
    newEdges <- tibble(
      from = inpfrom %>% as.integer, 
      to = inpto %>% as.integer,
      trust = ifelse(input$crowd,.5,input$trust),
      strength = ifelse(input$crowd,.5,input$strength),
      label = ifelse(input$crowd,"",input$arrLabel),
      fun = "",
      combo.type = ifelse(input$crowd,"",ifelse(is.null(input$combo), "", input$combo)),
      definition.type = ifelse(input$crowd,"",input$definition.type),
      statement = ifelse(input$crowd,1,values$pag %>% as.integer()),
      quote = ifelse(input$crowd,"",qq),
      full.quote = ifelse(input$crowd,"",values$statements$text[values$statements$statement == values$pag])
    ) 
    
    values$graf <- values$graf %>% 
      bind_edges(newEdges) 
    
    if(!is.null(input$combo)) {
      if(input$combo!="") values$graf=values$graf %>% N_ %>%
          mutate(fun=ifelse(inpto==label,input$combo,fun))
    }
    
    valuesCoding$fromStack <- NULL
    # doNotification("from stack zero",99)
    # valuesCoding$foundIDs <- NULL
    # visNetworkProxy("net") %>%
    #   visSetSelection(unselectAll = TRUE)
    # doNotification(paste0("sel ",input$net_selected),99)
    updateTextInput(session=session,"selectBoxValue",value="")
    
    tmp <- req(values$graf)             # has to be agg2 because of statements, but shouldn't be because some missed out
    vpag <- values$pag
    iot <- input$onlyThisStatement
    delay(4000,refresh_and_filter_net(tmp,vpag,iot))   # TODO the 4 seconds is just a lucky guess
    
  })
  
  # textbox search nodes and highlights them --------------------------------
  

  observeEvent(c(input$selectBoxValue),{
    if(req(input$sides)=="Code"){
      
      # visNetworkProxy("net") %>%
      #   visGetNodes(input="net_nodes")
      
      # req(values$grafAgg2)
      # browser()
      if(input$selectBoxValue!=""  && nchar(input$selectBoxValue)>2){
        
        # browser()
        vag <- values$graf %>% nodes_as_tibble %>% 
          pull(label) 
        
        ids <- which(vag==input$selectBoxValue)
        
          # tolower %>% 
          # str_detect(input$selectBoxValue %>% tolower) %>% 
          # sapply(function(x)agrep(tolower(input$selectBoxValue),x) %>% length %>% `==`(1))  %>% 
          # which %>% 
          # unname
        # browser()
        
        # info <- data.frame(matrix(unlist(input$net_nodes), ncol = dim(vag)[1],byrow=T),stringsAsFactors=FALSE)
        # colnames(info) <- colnames(vag)
        # browser()
        wipe <- setdiff(valuesCoding$foundIDs,ids)
        
        if(length(ids) !=0 && length(ids)<length(vag)){
          visNetworkProxy("net") %>%
            visUpdateNodes(tibble(id=ids,hidden=F)) %>% 
            visSelectNodes(id = ids)
        }
        
        if(length(wipe)>0 ){
          # visNetworkProxy("net") %>%
          #   visUpdateNodes(tibble(id=wipe,hidden=T)) 
          }
        
        visNetworkProxy("net") %>%
          visFit(animation=list(duration=500))
        
        valuesCoding$foundIDs <- c(valuesCoding$foundIDs,ids)
      }
    } 
  })
  # 
  # observeEvent(input$addNewNode,{
  #   # browser()
  #   if(length(valuesCoding$foundIDs)==0 && (input$selectBoxValue!="")){
  #     values$graf <- values$graf %>% 
  #       bind_nodes(tibble(label=input$selectBoxValue,shadow.color="gold",shadow.x=0,shadow.y=0,shadow.size=50))
  #     
  #     valuesCoding$fromStack <- nrow(values$graf %>% nodes_as_tibble)
  #     doNotification("Added new node",2)
  #     
  #   }
  #   
  # })
  # 
  # 
  
  observeEvent(c(input$resetSelection,req(input$pager),input$onlyThisStatement),{
    
    
    
  # observeEvent(input$pager,{
    # browser()
    tmp <- req(values$graf)             # has to be agg2 because of statements, but shouldn't be because some missed out
    vpag <- input$pager[[1]]        # had to put this instead of values$pag, not sure why
    iot <- input$onlyThisStatement
    # browser()
    refresh_and_filter_net(tmp,vpag,iot)
    
    valuesCoding$fromStack <- NULL
    updateSelectizeInput(session = session,inputId = "selectBoxValue",selected="") 
    
  })
  
  
  # widgets to edit and delete selected nodes and edges. not complete ----
  # 
  
  output$varForm=renderUI({
    if (length(req(input$net_selected))>0) {
      # browser()
      df <- values$graf %>%
        nodes_as_tibble() %>%
        mutate(id = row_number()) %>%
        left_join(values$grafAgg2 %>% nodes_as_tibble() %>% select(new = id, id = origID)) # to provide a lo,1>9okup to find original ids if the nodes have been merged
      # df <- (values$graf %>% nodes_as_tibble %>% mutate(id=row_number()))
      
      rows <- df[input$net_selected,]
      div(tagList(
        if (length(input$net_selected)>0) {
          tagList(
            div(p("Edit: "),style="display:inline-block"),
            if (length(input$net_selected)==1) div(textInput("editVarFormText",NULL,placeholder="label",value=df[input$net_selected,"label"],width="100px"),style="display:inline-block;margin-right:5px"),
            div(textInput("editdetails",NULL,placeholder="details",value=ifelse(length(rows$details)>1,"",rows$details),width="100px"),style="display:inline-block;margin-right:5px"),
            div(textInput("editcluster",NULL,placeholder="cluster",value=rows$cluster %>% paste0(collapse=","),width="100px"),style="display:inline-block;margin-right:5px"),
            div(actionButton("editVarForm", "Save"),style = "display:inline-block;background-color:white;padding:10px;border-radius:5px"),
            div(actionLink("deleteVarForm", paste0("Delete: ",input$net_selected %>% paste0(collapse=";"),"?")),style = "padding:2px;display:inline-block;color:red")
          )      
        },
        hr(style="margin:2px")
      ),style="background-color:#EEFFEE;padding:10px;border:1px green solid"
        
      )
    }
    
  })
  observeEvent(input$editVarForm,{
    vg <- values$graf %>% 
      activate(nodes) 
    
     
    
    if(input$editdetails!=""){
      vg <- vg %>% 
      mutate(details=if_else(row_number() %in% input$net_selected,input$editdetails,details)) # hmm what if details form is empty 
    }
    
    if(length(input$net_selected)==1 && input$editVarFormText!=""){
      vg <- vg %>% 
        mutate(label=if_else(row_number() %in% input$net_selected,input$editVarFormText,label)) 
    }
    if(input$editcluster!=""){
      vg <- vg %>% 
        mutate(cluster=if_else(row_number() %in% input$net_selected,input$editcluster,cluster))  
        
    }
    
    values$graf <- vg
  })
  
  observeEvent(input$deleteVarForm, {
    # browser()
    whichtarg=values$grafAgg2 %>% nodes_as_tibble %>% 
      filter(row_number()==input$net_selected) %>% 
      pull(origID) 
    
    values$graf <- values$graf %>%
      activate(nodes) %>%
      filter(!(row_number() %in% whichtarg) )
  })
  
  # ++node/variables panel----
  
  
  # varSelectInput("variables", "Variable:", mtcars, multiple = TRUE) could be useful here
  
  output$combineVars <- renderUI({
    varlist <- values$nodes$label %>% unique() %>% as.character()
    varlist <- na.omit(varlist)
    
    tagList(
      # h5("Combine variables")
      # ,
      
      ### this will fail if duplicate labels TODO
      selectizeInput("combineSelect",
                     label = NULL, selected = NULL, multiple = T,
                     options =
                       list(create = T, placeholder = "start typing the name of the variables you want to combine", onInitialize = I('function() { this.setValue(""); }')),
                     choices = varlist
      ),
      textInput("newName", "Type a name for this new combined variable"),
      actionButton("combine", "Combine!"),
      hr()
    )
  })
  
  # observeEvent(input$combine, {
  #   if (!is.null(input$combineSelect)) {
  #     ic <- input$combineSelect
  #     if (length(ic) > 1) {
  #       one <- ic[1]
  #       rest <- ic[-1]
  #
  #       restids <- values$nodes %>%
  #         filter(label %in% rest) %>%
  #         pull(id)
  #
  #       oneid <- values$nodes %>%
  #         filter(label %in% one) %>%
  #         pull(id)
  #
  #       allids <- c(oneid, restids)
  #
  #       allLabs <- values$nodes %>%
  #         filter(id %in% allids) %>%
  #         pull(label) %>%
  #         paste0(collapse = "\n")
  #
  #       values$nodes <- values$nodes %>%
  #         mutate(cluster = ifelse(id %in% allids, oneid, cluster)) %>%
  #         mutate(clusterLabel = ifelse(id == oneid, ifelse(input$newName == "", allLabs, input$newName), clusterLabel))
  #
  #       doNotification("Added cluster information to variables")
  #     } else {
  #       doNotification("You need to put more than one variable to combine")
  #     }
  #   }
  # })
  #
  
  # automerge----
  
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
  
  output$combine_button=renderUI({
    if(!all(replace_na((nodes_as_tibble(values$graf))$cluster,"")=="")){
      div(actionButton("node_permanent_combine", "Combine clusters permanently!?"),style="margin-top:5px;")
    }
  })
  
  observeEvent(input$node_permanent_combine,{
    values$graf <- values$tmp.graf
  })
  
  output$nodeTable <- renderRHandsontable({
    # browser()
    arrows=values$graf %>% edges_as_tibble %>% select(xid=from,to) %>% unlist %>% unclass() %>% as.tibble
    
    vg=values$graf %>% nodes_as_tibble %>% 
      # mutate_all(replaceNA) %>% 
      mutate(xid=row_number()) %>% 
      left_join(arrows %>%  select(xid=value),by="xid") %>% 
      group_by(xid) %>% 
      mutate(frequency=n()) %>% 
      summarise_all(last)
    
    
    if(!is.null(input$net_selected)){
      whichtarg=values$grafAgg2 %>% nodes_as_tibble %>% 
        filter(row_number()==input$net_selected) %>% 
        pull(origID) 
      
    }
    else
      whichtarg=0
    
    
    
    doNotification("Creating nodes table")
    rhandsontable(vg, rowHeaders = FALSE, selectCallback = T,
                  row_highlight=as.numeric(whichtarg)-1, 
                  col_highlight=0, 
                  row_hide=(unite(vg, "xox") %>% pull(xox) %>% str_detect(input$nodeTableFilter) %>% `!`() %>% which)-1
                  ,usetypes = T) %>%
      hot_context_menu(allowRowEdit = F) %>%
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_cols(columnSorting = T) %>%
      hot_cols(manualColumnMove = T) %>%
      hot_col("type",source=xc("◨ ◪ ♛ ֍"),type="dropdown")%>% 
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
    }"
      )
    
    
  })
  
  output$nodeTableAddCol=renderUI({
    tagList(
      div(textInput("newNodeName","Name"),style="display:inline-block;width:30%"),
      div(selectInput("newNodeType","Type",choices=xc("text numeric logical")),style="display:inline-block;width:30%"),
      div(actionButton("newNodeGo","Add column"),style="display:inline-block;width:30%"),
      hr()
    )
  })
  
  observeEvent(input$newNodeGo,{
    # browser()
    if(!is.na(input$newNodeName)){
      values$graf=values$graf %>%
        activate(nodes) %>%
        mutate(!!input$newNodeName := ifelse(input$newNodeType=="numeric",as.numeric(NA),ifelse(input$newNodeType=="logical",as.logical(NA),"")))
      updateCheckboxInput(session,inputId = "nodeTableAddCol",value=F)
      # mutate(x=1)
    }})
  
  # observe node table----
  
  observeEvent(input$nodeTableUp, {
    # browser()
    doNotification("Creating nodes table")
    x <- hot_to_r(input$nodeTable) %>% 
      select(-frequency) %>% 
      arrange(xid) 
    
    values$graf <- tbl_graph(x, values$graf %>% edges_as_tibble()) 
    
  })
  
  # ++edge/arrows panel----
  
  output$test=renderUI({
    tagList(
      req(input$nodeTable_select$select$r) %>% as.character() %>% p()
    )
    
    # div(paste0(input$net_selected,"--",input$current_edge_id),style="background:white")
  })
  
  observeEvent(input$edgeTableUp, {
    # browser()
    doNotification("Updating edges table")
    x <- hot_to_r(input$edgeTable) %>%
      select(-fromLabel, -toLabel)
    
    node.ids <- values$graf %>% nodes_as_tibble() %>% mutate(id = row_number()) %>% pull(id)
    
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
      select(-from,-to,everything())
    
    # vn=values$graf %>% nodes_as_tibble 
    # 
    # # ve$from=factor(ve$from)
    # ve$from=factor(ve$from,labels=vn$label[ve$from])
    
    
    rhandsontable(ve, height = 600, rowHeaders = FALSE, row_highlight=as.numeric(input$current_edge_id)-1, usetypes = T) %>%
      hot_context_menu(allowRowEdit = F) %>%
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_cols(columnSorting = TRUE) %>%
      hot_cols(manualColumnMove = TRUE) %>%
      
      # hot_col() %>% 
      # hot_rows(fixedRowsTop = 1)%>%
      hot_cols(renderer = "function(instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 if (instance.params) {
                 mhrows = instance.params.row_highlight;
                 mhrows = mhrows instanceof Array ? mhrows : [mhrows];
                 }
                 if (instance.params && mhrows.includes(row)) td.style.background = 'coral';
    }"
      )
  })
  
  # output$overview <- renderPrint(if (!is.null(values$grafAgg2)) {
  #   pre(str(values$grafAgg2 %>% edges_as_tibble()))
  # })
  # 
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
  
  # ++Display / settings panel ----
  
  
  output$condFormattingOutput=renderUI({
    # browser()
    # if(is.null(values$grafAgg2)) gr <- values$graf else gr <- values$grafAgg2
    # gr <- gr %>% nodes_as_tibble() %>% 
    #   select(label,everything()) 
    # 
    # meaningful <- gr %>% apply(2,function(x)(length(unique(x)))>1)
    # 
    # gr <- colnames(gr)[meaningful]
    
    gr <- xc("label frequency sex mean_Positive_mean_mean notForwards mean_older_mean_mean mean_female_mean_mean female_mean older_mean ava_mean avp_mean")
    
    vals <- values$settingsConditional %>% 
      mutate(type=if_else(str_detect(attribute,"node"),"node","edge"))
    # browser()
      # attrs=c(node_names,edge_names)
      
    lapply(seq_along(all_attributes),function(n){
      thisAttribute <- all_attributes[n]
      thisType <- ifelse(n>length(node_names),"edge","node")
      if(is.na(thisAttribute))browser()
      vals2 = vals %>% filter(attribute==thisAttribute) 
    attribute_clean=str_replace_all(thisAttribute,"\\.","_") # because of js condition later
    
    div(
      
      div(
        p(thisAttribute,style="width:160px"),
        style="display:inline-block;vertical-align:top"
      ),
      
      div(
        selectInput(paste0('conditional_selector_', attribute_clean),label=NULL,choices = c("always","conditional on ..."), selected =vals2 %>% pull(selector),width="120px")
        ,
        style="display:inline-block;vertical-align:top"
      ),
      
      
      if(thisAttribute %>% str_detect("color")) {div(
        colourInput(paste0('conditional_value_', thisAttribute),
          label=NULL,
          palette = "limited",
          showColour ="background",
          value = vals2 %>% pull(value),
          allowedCols = allcols1
        ),
        style="display:inline-block;vertical-align:top"
        
      )} else div(
        textInput(paste0('conditional_value_', thisAttribute),NULL,value =  vals2 %>% pull(value),width="120px")
        ,style="display:inline-block;vertical-align:top"
      )
      ,
      conditionalPanel(paste0('input.conditional_selector_', attribute_clean,'=="conditional on ..."'),
        div(
          div(
          selectInput(paste0('conditional_var_', thisAttribute),label=NULL,choices = gr,selected = vals2 %>% pull(var),width="120px")
          # selectInput(paste0('conditional_var_', thisAttribute),label=NULL,choices = c("label","frequency"),selected = vals2 %>% pull(var),width="120px")
          ,
          style="display:inline-block;vertical-align:top"
        ),
        
        div(
          p("up to")
          ,
          style="display:inline-block;vertical-align:top"
        ),
        if(thisAttribute %>% str_detect("color")) div(
          colourInput(paste0('conditional_value2_', thisAttribute),
            label=NULL,
            palette = "limited",
            showColour ="background",
            value =  vals2 %>% pull(value2),
            allowedCols = allcols1
          ),
          style="display:inline-block;vertical-align:top"
          
        ) else div(
          textInput(paste0('conditional_value2_', thisAttribute),NULL,value =  vals2 %>% pull(value2),width="120px")
          ,style="display:inline-block;vertical-align:top"
        )  
          ,style="display:inline-block;background-color:#EEFFEE;margin-left:50px"
        )
      )
        ,
        hr(style="margin:5px")
    )
    })
    
    
    
  
  })
  
  
  
  output$filters=renderUI({
    clusters <- values$graf %>% nodes_as_tibble %>% pull(cluster) %>% unique

        tagList(
      lapply(colnames(values$statements %>% select(-text)),function(y){
      x=values$statements[[y]]
      u=unique(x) %>% na.omit()
      if(length(u)>1 & length(u)<12){
        div(checkboxGroupButtons(paste0("filters",y),y,choices=sort(u),selected=u),style="display:inline-block;vertical-align:top")
      }
    })
      ,
      
      if(!is.null(clusters) && length(clusters)>1)tagList(
        div(checkboxGroupButtons("filterscluster","cluster",choices=sort(values$graf %>% nodes_as_tibble %>% pull(cluster) %>% unique)),style="display:inline-block;vertical-align:top")
        
      )
    )
  })  
  
  
  ######## where is the observe event for filters, including for cluster filter?
  
  # produce user-friendlier settings widgets --------------------------------
  
  observeEvent(input$settingsTableGlobalUp,{
    # browser()
    vs <- values$settingsGlobal %>% mutate_all(as.character)
    output$inputs=renderUI({
      lapply(1:nrow(vs),function(x){
        row=vs[x,]
        rg=replace_na(row$widget,"")
        rt=paste0(row$type,row$setting,collapse="")
        rt=replace_na(rt,"")
        # if(rg=="color")  {checkboxInput("asdf","asdf")}
        div(
          if(rg=="color")  {
            # browser()
            colourInput(paste0('input', rt),rt,
                        palette = "limited",
                        showColour ="background",
                        value=if(is.null(findset(rt))) findset(rt) else findset(rt),
                        allowedCols = allcols1) 
          }
          else if(rg=="slider")  {
            sliderInput(paste0('input', rt),rt,min = 0,max=100,value = findset(rt)
            )
          }
          else if(rg=="checkbox")  {
            # browser()
            checkboxInput(paste0('input', rt),rt,value = as.logical(findset(rt))
            )
          }
          else 
            if(rg=="input") paste0(row$type,row$setting,collapse="")
          ,style="display:inline-block;")
      })
    })
  })
  
  ## observe changes in display widgets ----
  # observe(if(F){
  #   vs <- values$settingsGlobal %>% mutate_all(as.character)
  #   lapply(1:nrow(vs),function(x){
  #     row=vs[x,]
  #     rg=replace_na(row$widget,"")
  #     rt=paste0(row$type,row$setting,collapse="")
  #     rt=replace_na(rt,"")
  #     # if(rg=="color")  {checkboxInput("asdf","asdf")}
  #     inp=input[[paste0("input",rt)]]
  #     if(rg!="" & !is.null(inp))  {
  #       values$settingsGlobal$value[x]=inp
  #       doNotification(paste0("updated settings from widget: ",inp))
  #     }
  #   })
  # })
  
  output$settingsTableGlobal <- renderRHandsontable({
    vs <- values$settingsGlobal %>% mutate_all(as.character)
    
    ds <- defaultSettingsGlobal %>% mutate_all(as.character)
    
    vs <- bind_rows(vs, ds) %>%
      distinct(type, setting, .keep_all = T)
    
    
    rhandsontable(vs %>%
                    mutate(type = factor(type)), height = NULL, rowHeaders = FALSE, usetypes = T) %>%
      hot_context_menu(allowRowEdit = T) %>%
      hot_cols(colWidths = c(80,120,250,80))
  })
  
  # output$settingsTable <- renderRHandsontable({
  #   vs <- values$settings %>% mutate_all(as.character)
  #   
  #   ds <- defaultSettingsConditional %>% mutate_all(as.character)
  #   
  #   vs <- bind_rows(vs, ds) %>%
  #     distinct(type, setting, .keep_all = T)
  #   
  #   
  #   rhandsontable(vs %>%
  #                   mutate(type = factor(type), condition = factor(condition, levels = c("always", "if...", "conditional on:"))), height = NULL, rowHeaders = FALSE, usetypes = T) %>%
  #     hot_context_menu(allowRowEdit = T) %>%
  #     hot_cols(colWidths = c(80,120,250,80))
  #   
  # })
  
  # observeEvent(input$settingsTableUp, {
  #   doNotification("updating from settingsTable")
  #   values$settings <- hot_to_r(input$settingsTable)
  # })
  
  observeEvent(input$settingsTableGlobalUp, {
    doNotification("updating from settingsTableGlobal")
    values$settingsGlobal <- hot_to_r(input$settingsTableGlobal)
  })
  
  
  # ++ library/gallery panel ------------------------------------------------
  
  ## Gallery ----
  
  output$gallery <- renderUI({
    
    if(storage=="local"){
      details <- file.info(list.files("www", pattern = "-nodes.csv$", full.names = TRUE))
      
      details <- details[with(details, order(as.POSIXct(mtime))), ]
      files <- rownames(details) %>% gsub("www/", "", .) %>% gsub("\\-nodes.csv", "", .)
      
      lapply(rev(files), function(x) {
        sets=read_csv(paste0("www/",x,"-settingsGlobal.csv"))
        desc=sets$value[sets$setting=="description"][1]
        tagList(
          tags$a(x, href = paste0(".?permalink=", gsub("\\.-nodes.csv$", "", x))),
          "|",
          desc,
          hr()
        )
      })
    } else if(storage=="dropbox"){
      # browser()
      details <- drop_dir("www") %>% mutate(x=as.POSIXct(client_modified)) %>% filter(str_detect(name,"-settingsGlobal\\.csv$")) %>% arrange(x)
      
      files <- details %>% pull("path_display") 
      doNotification("Loading file information",2)      
      lapply(rev(files), function(x) {
        sets=read__csv(x)
        desc=sets$value[sets$setting=="description"][1]
        x=gsub("/www/","",x)
        x=x %>%  str_remove("-settingsGlobal\\.csv$")
        tagList(
          tags$a(x, href = paste0(".?permalink=", gsub("-settingsGlobal\\. csv$", "", x))),
          "|",
          desc,
          hr()
        )
      })
      
    }
    
  })
  
  # ++ charts panel  -----------------------------------------------------------
  
  output$pivot <- renderRpivotTable({
    # mtcars$car <- rownames(mtcars)
    doNotification("creating pivot")
    rpivotTable(nodes_as_tibble(values$grafAgg2)[,1:20])
  })  
  
  # ++ downloads panel  -----------------------------------------------------------
  
  output$downloads=renderUI({
    # browser()
    doNotification("Creating library list")
    if(!is.null(values$current)){
      name <- gsub("www/","",values$current)
      tagList(
        h4("Download your files"),
        h5("CSV files"),
        lapply(csvlist,function(x){
          tagList(a(href=paste0(name,"-",x,".csv"),x),hr())
        })
        ,
        h5("Graphic files"),
        div(
          
          actionButton("png", "Create new files", icon = icon("picture")),
          style = "display:inline-block;margin-right:50px;width:10px"
        ),
        hr(),
        tagList(
        a(href=paste0(name,"",".html"),"interactive html file"),hr(),
        a(href=paste0(name,"",".png"),"high-quality png file"),hr()
        )
      )}
  })
  
  # ++ main panel  -----------------------------------------------------------
  # description below graph
  output$description=renderUI({
    x=findset("diagramdescription")
    if(x!="")  div(p(x),style="padding:10px;background-color:whitesmoke;margin-top:10px;border-radius:5px")
    else ""
  })
  
  
  # colorLegend -------------------------------------------------------------
  # just a placeholder right now
  
  output$colourLegend <- renderPlot({
    emptyplot(main = "Percentage of women mentioning each factor and each link                 Percentage of younger people mentioning each factor",adj=0)
    colorlegend(posx = c(0, 0.1), 
                col = intpalette(c("blue", "red"), 100), 
                zlim = c(0, 100), zval = c(0,25,50,75,100))
    
    colorlegend(posx = c(0.5, 0.6), 
                col = intpalette(c("black", "white"), 100), 
                zlim = c(0, 100), zval = c(0,25,50,75,100))
    
  })
  
  # ++AGGREGATE ---------------------------------------------------------------------------
  # the long process of aggregating values$graf into values$grafAgg2, adding formatting etc

  
  observe({
    
    # make this code run whenever the tab$change reactive triggers
    tab$change
    # SP commented out above line
    
    # prevent this code running every time we update values
    
    
    # browser()
    vals <- isolate(values)
    
    # prevent this code running every time we change tab
    this_tab <- isolate(input$sides)
    
    edges_tbl <- edges_as_tibble(req(values$graf))
    
    if (nrow(edges_tbl) > 0) {
      
      # prepare statements, split columns ------------------------------------------------------
      
      doNotification("starting aggregation")    
      legend <- ""
      
      # post-process original version
      
      graph_values <- prepare_vg(values$graf) 
      
      # browser()
      
      # infer ----
      
      if(findset("variableinfer", v = vals) %>% as.logical()){
        graph_values=infer(graph_values)
        legend <- paste0(legend, "</br>Causal inference carried out")
      }
      
      vno <- graph_values %>% nodes_as_tibble() 
      ved <- graph_values %>% edges_as_tibble()
      
      
      if (findset("variablemerge") %>% as.logical() & input$sides!="Code") { # need to convert to and froms in edge df

      x <- merge_nodes(vno,ved)
      vno <- x[[1]]
      ved <- x[[2]]
      }
      
      
      
      # prepare ved
    
      ved <- prepare_ved(ved)
      # vno <- prepare_vno(vno)
      
      # ved rick inv_multi --------------------------------
      
      if(("from" %in% colnames(ved))  &&  as.logical(findset("arrowabsence"))){ #todo findset
        
        doNotification("rick aggregation")    
        
        if(all(is.na(ved$statement)))ved$statement=1
        # browser()
        ved <- ved %>%
          inv_multi()
      }
    
    
      # ved join statements--------------------------------
      # browser()
      values$statements <- values$statements %>% mutate(source__id=row_number())
      
      ved <- ved_join(ved, values$statements)
      
      
      saveRDS(ved,"ved")
      
      
      
      
      ved <- ved %>%
        mutate(statement=as.character(statement)) %>%
        mutate(wstrength = strength * trust) 
      # browser()
      
      
      
      # quip stats by question/domain---------------
      
      
      if("source" %in% colnames(ved) && "question" %in% colnames(ved)){
      
      # browser()
        ved <- ved %>%
        group_by(from, to, question) %>% 
        mutate(citation_count=length(unique(source))) %>% 
        ungroup() %>% 
        group_by(from, to) %>% 
        mutate(respondent_count=length(unique(source))) %>% 
        ungroup() %>% 
          mutate(citation_intensity=citation_count/respondent_count)
      
      
      }
        # browser()
      # ved edge merge TODO this messes up statements ---- 
      
      if (input$sides!="Code" & findset("arrowmerge", v = vals) %>% as.logical() ) {
        ved <- ved %>%
          group_by(from, to)
        
        legend <- paste0(legend, "</br>Multiple arrows between pairs of variables collapsed into one")
        
      } else {
# add different curve to each edge in coterminal sets of edges--------------
        ved <- ved %>%
          group_by(from, to) %>% 
          mutate(smooth.type="continuous") %>%
          mutate(smooth.roundness=seq(from=0,to=.8,length.out = n())) %>%
          mutate(smooth.enabled=TRUE) %>%
          ungroup() %>% 
          group_by(row_number())
      }
      
      doNotification("merge edge aggregation")    
      
      
      ved <- ved %>%
        mutate_if_sw(is.numeric, .funs = list(sum=sumfun,mean=meanfun)) %>%
        mutate_if_sw(is.character, .funs = catfun) %>%
        mutate(frequency = n()) 
      
      ved <- ved %>% 
        summarise_all(.funs = funs(first)) 

      
      
      
      ved <- ved %>% ungroup() %>%
        mutate(title = paste0(frequency, gsub("[^[:alnum:][:space:]]", "", label), separate = "- "))
      
      if("N" %in% colnames(ved)){
        ved <- ved %>% 
          mutate(frequency=N)
      }
      
      doNotification("min freq aggregation")    
      
      # edge minimum freq ----
      
      if(this_tab!="Code"){
        ved <- ved %>%
          filter(frequency > findset("arrowminimum.frequency", v = vals))
      }
      
      # join edges with nodes------------------------------------------------
      
      doNotification("join to edges aggregation")    
      
      
      if(findset("variablejoinedges", v = vals) %>% as.logical){ #todo, should list any functions. variablejoinedges is pointless
        
      vno <- join_nodes_and_edges(vno,ved)
      
          # browser()
        
        # doNotification("merging nodes and arrows")
        
        
      }
      
      # minimum freq for vars
      mf <- findset("variableminimum.frequency", v = vals) %>% as.numeric()
      # browser()
      if (this_tab!="Code" && mf > 0 ) {
        tmp <- tbl_graph(vno, ved) %>%
          N_() %>%
          filter(frequency > mf)
        
        vno <- tmp %>% nodes_as_tibble()
        ved <- tmp %>% edges_as_tibble()
      }
      
      
      doNotification("format aggregation")    
      

      
      tmp <- tbl_graph(vno, ved)
      # browser()
      
      # layout ----------------
      
      
      layout <- create_layout(tmp, layout = 'sugiyama') %>% 
        select(x,y,id)
      
      tmp <- tmp %>% activate(nodes) %>% 
        left_join(layout,by="id")
      
      tmp <- tmp %>% activate(edges) %>% 
        mutate(fromLevel=.N()$y[from],toLevel=.N()$y[to],notForwards=fromLevel>=toLevel) 
      
      # cond formatting------------
      
      # ved <- ved %>% 
      #   format_edges
      
      vno <- tmp %>% nodes_as_tibble()
      ved <- tmp %>% edges_as_tibble()
      
      
      if(input$sides!="Code"){
      vno <- vno %>% 
        format_nodes_and_edges(input,type="node")
      
      
      ved <- ved %>% 
        format_nodes_and_edges(input,type="edge")
      
      
      } else {
        vno$font.color="#333333"
        vno$font.size=66
        ved$width=4
      }
      ### make sure text is visibile when highlighted
      vno <- vno %>% 
        mutate(color.highlight.background=set_text_contrast_color(font.color))
      
      
      # margin--------
      # browser()
      vno <- vno %>% 
        mutate(margin=10)  # decent approximation
      
      
      
      # rationalise----
      
      doNotification("final aggregation")    
      
      ved=ved %>% 
        mutate(strength=strength_mean) %>% 
        mutate(wstrength=wstrength_mean) 
      # if("mean_key2_mean_mean" %in% colnames(vno))vno=vno %>% mutate(key2=mean_key2_mean_mean) 
      
      # labels----
      # browser()
      ved <- ved %>%
        mutate(label = replace_na(label,"")) %>% 
        mutate(label = ifelse(strength<0,paste0("🔀 ",label),label)) %>% 
        # mutate(combo.type <- replace_na(combo.type,"")) %>% 
        # mutate(arrows.middle.enabled = ifelse(combo.type == "", F, T)) %>%
        mutate(arrows.middle.enabled = F) %>%
        # mutate(label = paste0(label, ifelse(arrows.middle.enabled, paste0(" ", combo.type), ""))) %>%
        mutate(dashes = definition.type != "") %>% 
        mutate(arrows.to = definition.type != "Defined, undirected")
      
      if (!is.null(legend)) {
        if (legend != "") values$legend <- glue("</br><b style='font-color:red;'>Legend:</b>{legend}</br></br></br></br>")
      }
      
      vno <- vno %>%
        mutate(label = if_else(value>0,paste0(label," ♥"), label)) %>%
        mutate(label = if_else(value<0,paste0(label," ☹"), label)) %>%
        mutate(label = str_replace_all(label, "///", "<br>")) 
      
      
      vno <- vno %>% 
        mutate(label=make_labels(findset("variablelabel", v = vals),vno)
        )
      
      vno <- vno %>% 
        mutate(title=make_labels(findset("variabletooltip", v = vals),vno)
        )
      
      
      ved <- ved %>% 
        mutate(title=make_labels(findset("arrowtooltip", v = vals),ved),
               label=make_labels(findset("arrowlabel", v = vals),ved)
        )
      
      
      # fontsize ceiling so it doesn't crash. doesn't take into account if there are very long words
      # browser()
      vno <- vno %>% 
        mutate(font.size=round(pmin(as.numeric(font.size),as.numeric(findset("variablewidth"))/8)))
      
      
      # # wrapping ----
      # ved = ved %>%
      # 
      # 
      ved <- ved %>%
        mutate(label = str_replace_all(label, "///", "\n")) %>%
        mutate(label = str_wrap(label, findset("arrowwrap"))) %>%
        mutate(label = str_replace_all(label, "NA", "")) %>%
        mutate(label = str_trim(label))
      
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
  
  observe(if(!is.null(values$grafAgg2)){
    
    vals <- isolate(values)
    vga <- req(values$grafAgg2)

    if((input$crowd)){
      values$legend=""
    }
    
    doNotification("started viz")
    # browser()
    
    if(is.null(values$pag)){
      values$pag=1
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
            mutate(id = row_number()) 
          ,
          main =
            findset("diagramtitle"),
          submain =
            findset("diagramsubtitle"),
          # footer =
          #   values$legend,
          background = findset("diagrambackground", v = vals)
          ,
          height="2000px",
          width="3000px"
          # height=findset("diagramheight", v = vals) %>% paste0("px"),
          # width=findset("diagramwidth", v = vals) %>% paste0("px")
          
          # width="100%"
          # ,
          # height="2000px"
          # ,
          # width="2000px"
        ) 
      vn = vn1 %>%
        visInteraction(
          dragNodes = T,
          dragView = T,
          zoomView = T,
          navigationButtons = F,
          multiselect = T
        ) %>%
        visInteraction(
          tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;
                -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                 border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                 max-width:500px;word-break: break-all'
        ) %>%

        visOptions(
          manipulation = F,
          collapse = F,
          highlightNearest = list(
            enabled = T,
            degree = if(findset("diagramdownarrows") %>% as.logical) list(from=0,to=19) else list(from=19,to=0),
            # degree = ifelse(input$widgetDownArrows,list(from=0,to=19),list(from=19,to=0)),
            hover = T,
            labelOnly=F,
            algorithm = "hierarchical"
          ),
          nodesIdSelection = F
        ) %>%
        visConfigure(enabled = input$codeCollapse == "Advanced options",
                     container = "advancedAnchor") %>%
        # visEvents(select = "function(edges) {
        #         Shiny.onInputChange('current_edge_id', edges.edges);
        #         ;}") %>%
        visEvents(select = "function(data) {
                Shiny.onInputChange('net_selected', data.nodes);
                Shiny.onInputChange('net_selectedEdges', data.edges);
                ;}")        # visEvents(select = "function(nodes) {
        #         Shiny.onInputChange('net_selected', nodes.nodes);
        #         ;}")
      
      if (!all(na.omit(vga$group) == "")) {
      }
      
      
      #layout ----------------
        # browser()
      
      if (findset("diagramlayout", v = vals) == "layout_with_sugiyama") {
        # browser()
        nods <- vn$x$nodes  # saving them from the previous version, as the visigraphlayout in th next step, which shouldn't be necessary, is
        # browser()
        vn <- vn %>%
          visIgraphLayout(layout = "layout_with_sugiyama", randomSeed = 123, type = "full")
        
        if (findset("diagramorientation",v = vals) %in% xc("LR RL")) {
          # browser()
          vn$x$nodes <- nods # transferring from above
          tmp <- vn$x$nodes$x
          vn$x$nodes$x <- vn$x$nodes$y
          vn$x$nodes$y <- tmp
          vnxn <- vn$x$nodes
          levels=(length(unique(vnxn$x)))
          maxLen=vnxn %>% 
            group_by(x) %>% 
            summarise(len=n()) %>%
            max
          
          vnxn=vnxn %>% 
            group_by(x) %>% 
            mutate(ran=min_rank(y),len=n(),y=rescale(ran,to=c(-1,1))*sqrt(len/maxLen) + rnorm(1,0,.1/len)) %>% 
            ungroup()               #had to put in a tiny bit of rnorm to stop some artefacts in visnetwork when nodes have same y
          # mutate(len=n(),ran=min_rank(y)-.5,y=ran*levels/(len*3))
          vnxn=vnxn %>% 
            mutate(x=1-scale(x)*1,y=scale(y))  # 1 is the proportion. not sure why i added this line
          vn$x$nodes = vnxn
          
          
          
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
      vn <- vn %>%
        visNodes(
          shadow = list(enabled = T, size = 10),
          # color = list(highlight="black"),                   
          # color.highlight.background="white"
          widthConstraint=as.numeric(findset("variablewidth")) , #,300-(levels*10),#,(300*levels)-9,
          hidden = F,# findset("variablehidden",global=F) %>% as.logical(),
          scaling = list(label = list(enabled = F)),
          shape = findset("variableshape", v = vals),
          # shapeProperties = list("borderDashes=T"),
          group = T,#findset("variablegroup",global=F),
          # borderWidth = findset("variableborderWidth",global=F, v = vals),
          # widthConstraint=4,
          # widthConstraint = =4,
          # size =
          #   findset("variablesize",global=F),
          
          physics = findset("diagramphysics", v = vals)
          # ,
          # ,
          # widthConstraint=10
          
          # widthConstraint=findset("variablewidth") %>% as.numeric # %>% ifelse(.>0,.,NA)
        ) %>%
        visEdges(
          smooth = T,
          arrowStrikethrough = F,
          shadow =
            list(enabled = F, size = 5),
          # width =
          #   findset("arrowwidth"),
          # font =
          #   list(
          #     color =
          #       findset("arrowfont.color",global=F, v = vals),
          #     background =
          #       "#FFFFFF80",
          #     size = findset("arrowfont.size",global=F, v = vals)
          #   ),
          physics =
            F,
          arrows =
            list(middle = list(type = "circle", scaleFactor = .5))
          # ,
          # dashes = findset("arrowdashes") %>% as.logical()
        )
      
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
  if (T){
    visNetworkProxy("net") %>%
      visUpdateNodes(nodes = tibble(id=1:20,color="red"))
  }
  })
  
  # observeEvent(input$network_initialized,{
  #   doNotification("initial")
  #   if (T){
  #     visNetworkProxy("net") %>%
  #       visUpdateNodes(nodes = tibble(id=1:20,color="red"))
  #   }
  # })
  # 
  


  
  # ++ report -----------------------------------------------------
  
  output$reportTable=renderFormattable({
    values$net$x$nodes %>% 
      transmute(label,frequency=replace_na(frequency,0),from=replace_na(from.frequency_sum,0),to=replace_na(to.frequency_sum,0)) %>% 
      arrange(desc(frequency)) %>% 
      
      formattable(list(
        `frequency`= color_bar("#AAEEAA"),
        `from`= color_bar("#EEAAAA"),
        `to`= color_bar("#AAAAEE")
        ))
  })
  
  # ++ floating widgets -----------------------------------------------------
  output$floatingWidgets <- renderUI({
    div(
      
      # div(
      #   # icon("search-plus"),
      #   actionButton("bigpicture", label=icon("search-plus"), width = "30px"),
      #   # checkboxInput("bigpicture", label=icon("search-plus"), width = "30px"),
      #   style = "display:inline-block;margin-left:10px"
      # ),
      # actionButton("widgetsUP","Update")
      # div(
      #   
      #   checkboxInput("widgetDownArrows", "Hover arrows down"),
      #     style = "display:inline-block;margin-right:140px;width:10px"
      #   ),
      # div(

      # ,
      # div(checkboxInput("showStatements", "Statements"), style = "display:inline-block;margin-right:80px"),
      # div(actionButton("open", "Statements"),style = "display:inline-block;margin-right:80px"),
      
      div(actionButton("fitaction", "Fit"), style = "display:inline-block;margin-right:20px"),
      class = "bigpicbut" ,style="z-index:999 !important")
    # h1("asdfasdfasdf")
  })

    output$savebut <- renderUI(div(
    div(
      tagList(
        
        # div(actionButton("render_button","Render"),style="display:inline-block;vertical-align:top"),
        div(textInput(
          "titl", NULL,
          value = ifelse(is.null(values$current), "", values$current), placeholder = "Title", width = "100%"
        ), style = "display:inline-block;width:50%"),
        div(
          actionButton("saveb", "Save", icon = icon("save")),
          style ="display:inline-block;margin-left:5px;margin-right:5px;width:10%"
        )
      )
      ,
      style = "margin-bottom:-20px"
    )
  ))
  
  
  
  # observe save button ----
  # it saves the current project as csv files
  observeEvent(
      input$saveb , ignoreInit = TRUE, {
      # browser()
      # doNotification("observing save button",7)
      
      if ("" != input$titl) {
        inputtitl <<- gsub("[^[[:alnum:]|-]]*", "", input$titl)
        values$current <- inputtitl
        
        
        # saveRDS(values, paste0("www/", inputtitl, ".tm"))
        
        nodes=values$graf %>% nodes_as_tibble
        edges=values$graf %>% edges_as_tibble
        
        # have to check if the conditinal settings tab has ever  been visited
        if(is.null(input[[paste0('conditional_value_', all_attributes[[1]])]])) {
          if(is.null(values$settingsConditional)) settingsConditional <- defaultSettingsConditional 
          else 
          settingsConditional <- values$settingsConditional 
        }
          else 
          settingsConditional <- make_settingsConditional(input)
        
        
        # browser()
        # if(storage="local"){
        write__csv(settingsConditional, path=paste0("www/", inputtitl, "-settingsConditional.csv"))
        if(!is.null(values$settingsGlobal))write__csv(values$settingsGlobal, path=paste0("www/", inputtitl, "-settingsGlobal.csv"))
        if(!is.null(values$statements))write__csv(values$statements, path=paste0("www/", inputtitl, "-statements.csv"))
        if(!is.null(nodes))write__csv(nodes, path = paste0("www/", inputtitl, "-nodes.csv"))
        if(!is.null(edges))write__csv(edges, path = paste0("www/", inputtitl, "-edges.csv"))
        
        if(storage=="gsheets"){
          # browser()
          doNotification("Uploading to google drive",2)
          drive_upload(media=paste0("www/", inputtitl, "-settings.csv"),name=paste0(inputtitl,"-settings"),type="spreadsheet")
          drive_upload(media=paste0("www/", inputtitl, "-settingsGlobal.csv"),name=paste0(inputtitl,"-settingsGlobal"),type="spreadsheet")
          drive_upload(media=paste0("www/", inputtitl, "-statements.csv"),name=paste0(inputtitl,"-statements"),type="spreadsheet")
          drive_upload(media=paste0("www/", inputtitl, "-nodes.csv"),name=paste0(inputtitl,"-nodes"),type="spreadsheet")
          drive_upload(media=paste0("www/", inputtitl, "-edges.csv"),name=paste0(inputtitl,"-edges"),type="spreadsheet")
          doNotification("Finished uploading to google drive",2)
          
        }
        
        
        # file.copy(paste0("www/", inputtitl,".tm"),paste0("www/", inputtitl,".otm"),overwrite = T)
        doNotification("Saved")
        values$issaved = T
        # toggleClass("savemsg", "red")
        # delay(500, toggleClass("savemsg", "red"))
        
        
        
      }
    }
  )
  
  observeEvent(input$updateE_crowd, ignoreInit = TRUE, {
  # browser()
  # doNotification("observing save button",7)
  
  if ("" != input$titl) {
    inputtitl <<- gsub("[^[[:alnum:]|-]]*", "", input$titl)
    values$current <- inputtitl
    
    
    # saveRDS(values, paste0("www/", inputtitl, ".tm"))
    
    nodes=values$graf %>% nodes_as_tibble
    edges=values$graf %>% edges_as_tibble
    
    # browser()
    # if(storage="local"){
    if(!is.null(values$settings))write__csv(values$settings, path=paste0("www/", inputtitl, "-settings.csv"))
    if(!is.null(values$settingsGlobal))write__csv(values$settingsGlobal, path=paste0("www/", inputtitl, "-settingsGlobal.csv"))
    if(!is.null(values$statements))write__csv(values$statements, path=paste0("www/", inputtitl, "-statements.csv"))
    if(!is.null(nodes))write__csv(nodes, path = paste0("www/", inputtitl, "-nodes.csv"))
    if(!is.null(edges))write__csv(edges, path = paste0("www/", inputtitl, "-edges.csv"))
    
    if(storage=="gsheets"){
      # browser()
      doNotification("Uploading to google drive",2)
      drive_upload(media=paste0("www/", inputtitl, "-settings.csv"),name=paste0(inputtitl,"-settings"),type="spreadsheet")
      drive_upload(media=paste0("www/", inputtitl, "-settingsGlobal.csv"),name=paste0(inputtitl,"-settingsGlobal"),type="spreadsheet")
      drive_upload(media=paste0("www/", inputtitl, "-statements.csv"),name=paste0(inputtitl,"-statements"),type="spreadsheet")
      drive_upload(media=paste0("www/", inputtitl, "-nodes.csv"),name=paste0(inputtitl,"-nodes"),type="spreadsheet")
      drive_upload(media=paste0("www/", inputtitl, "-edges.csv"),name=paste0(inputtitl,"-edges"),type="spreadsheet")
      doNotification("Finished uploading to google drive",2)
      
    }
    
    
    # file.copy(paste0("www/", inputtitl,".tm"),paste0("www/", inputtitl,".otm"),overwrite = T)
    doNotification("Saved")
    values$issaved = T
    toggleClass("savemsg", "red")
    delay(500, toggleClass("savemsg", "red"))
    
    
    
  }
}
)
  
  # observe save png button -------------------------------------------------------------
  #  saves a png and html file which are not yet downloadable
  
  
  observeEvent(input$png,{
    # browser()
    doNotification("Saving file",2)
    fn <- paste0(input$titl,".html")
    visSave(values$net, fn, selfcontained = T)
    doNotification("Saved file",2)
    file.copy(fn,paste0("www/",fn),overwrite=T)             #because there is a bug with htmlwidgets saving to other directories
    file.remove(fn)
    webshot::webshot(file=paste0("www/",input$titl,".png"), url = paste0("www/",input$titl,".html"))
    doNotification("Saved png",2)
  })
  
  
  
  observeEvent(input$saveb, ignoreInit = TRUE, {
    output$savedMsg <- renderUI({
      if (!values$issaved) {
        div()
      } else {
        div(
          id = "savemsg",
          "Saved to this permanent link: ",
          tags$a(
            paste0(values$current),
            href = paste0("?permalink=", values$current)
          )
        )
      }
    })
  })
  
  
  
  
  # ++ crowsourcing / mobile interface --------------------------------------
  
  observe({
    if(input$crowd){output$net2 <- renderVisNetwork({
      doNotification("render viz",type="error")
      values$net
      # browser()
    })}
  })
  
  
  output$add_edges_widget2 <- renderUI({
    varlist=values$graf %>% nodes_as_tibble() %>% pull(label) %>% unique() %>% as.character()
    varlist <- na.omit(varlist)
    
    tagList(
      h3("What do people think about online social networking?",style="color:white"),
      h5("Thanks for taking part. I want to know what you think about the popuarity of online social networking.
          What drives it? What are the main things it will lead to? What in turn will those things lead to? 
          All you have to do is make links between different things - by typing the name of one thing in the top box,
          and then the name of the thing you think it leads to in the bottom box. 
          When typing, you can choose from the things people have already mentioned, or type your own. If there is already an arrow between the two things you select, the arrow will get fatter.
          You can add as many arrows as you want.
          
          ",style="color:white;margin:30px"),
      div(
        selectizeInput("new1_edge_crowd",
                       label = NULL, selected = if (values$clickArrow) input$net_selected else NULL, multiple = F,
                       options =
                         list(create = T, placeholder = "start typing the name of the thing at the start of the arrow(s)", onInitialize = I('function() { this.setValue(""); }')),
                       choices = varlist,
                       width = "100%"
        ),
        selectizeInput("new2_edge_crowd",
                       label = NULL, selected = NULL, multiple = F,
                       options =
                         list(create = T, placeholder = "start typing the name of the thing at the end of the arrow(s)", onInitialize = I('function() { this.setValue(""); }')),
                       choices = varlist,
                       width = "100%"
        )
        ,style="500px;margin-top:50px"),
      div(
        actionButton("updateE_crowd",
                     "Add an arrow between these two things",
                     style = "border:1px solid green;padding:15px;display:inline-block;width:350px"
        )
      )
    )
  })
  
  output$edgeInfo=renderUI({
    # input$current_edge_id %>% as.character()
    if(!is.null(input$net_selectedEdges))input$net_selectedEdges %>% 
      paste0(collapse="; ") %>% 
      as.character() %>% 
      paste0("Edit package containing arrow(s): ",.) %>%  
      tagList(actionLink("deletePackage"," (Delete?)"))
  })
  
  observeEvent(input$deletePackage,{
    # browser()
    if(!is.null(input$net_selectedEdges)) values$graf <- values$graf %>% activate(edges) %>% mutate(id=row_number()) %>% filter(!(id %in% input$net_selectedEdges)) %>% select(-id)
  })
  
  # fit-----------------
  
  observeEvent(input$fitaction, {               # restore network to normal zoom
    visNetworkProxy("net") %>%
      visFit() 
    
  })
  
  
  observe({
    
  output$testBut <- if(!is.null(input$net_selectedEdges)) renderUI({actionButton("testBut","Edit selected arrows")})  else renderUI({p()})
    
  })
  
  
  # session$onSessionEnded(stopApp) ## TODO take out
  
  # 
  # observeEvent(input$resetSelection,{
  #   browser()
  #   updatePageruiInput(session=session,"pager",page_current= values$pag+1)
  #   updatePageruiInput(session=session,"pager",page_current= values$pag)
  #   visNetworkProxy("net") %>%
  #         visSelectNodes(id=NULL)
  # })

  # focus -------------------------------------------------------------------
  
  # 
  # 
  # observe(if(req(input$sides)=="Code"){
  #   # vno <- req(values$grafAgg2) %>% nodes_as_tibble
  #   # if (as.logical(findset("diagramfocus")) & !is.null(values$pag) & nrow(vno)>0) {
  #   #   ids <- vno %>%
  #   #     mutate(sel=ifelse(str_detect(paste0(",|^", statement, ",|$"), as.character(values$pag)),T,F)) %>%
  #   #     pull(sel) %>%
  #   #     which
  #   # 
  #   # visNetworkProxy("net") %>%
  #   #   visSelectNodes(id = ids)
  #   # }
  # 
  #   
  # })   
  
  
  
  # observeEvent(req(input$pager),{
  #   ved <- req(values$grafAgg2) %>% edges_as_tibble %>% mutate(id=row_number())
  #   if (!is.null(values$pag)) {
  # 
  #     eids <- ved %>%
  #       mutate(sel=ifelse(str_detect(statement, paste0("(,|^)", as.character(values$pag), "(,|$)")),T,F)) %>%
  #       filter(sel) %>%
  #       pull(id) 
  #     # browser()
  #     
  #     visNetworkProxy("net") %>%
  #       visSelectEdges(id = eids) 
  #   }
  # })
  
  
  
  
  output$keypr = renderPrint({
    input$keypressed
  })  
  
  output$push=renderUI({
    actionButton("statementsTableUp", "Update")
    rHandsontableOutput("statements")
    
  })
  
  
  
  
  jqui_draggable("#sel,#chbox,#saveb")
  
  
  # makeReactiveBinding("inputtitl")
  # browser()
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")
}
