server <- function(input, output, session) {
  autoInvalidate <- reactiveTimer(2000)
  
  values <- reactiveValues()                         # nearly all reactive values are stored in values$...
  values$pag <- 1                         # stores value of pager in Code panel
  values$statements <- default.statements
  values$clickArrow <- F                         # no idea 
  values$crowd = F
  
  values$settings <- defaultSettings 
  values$settingsGlobal <- defaultSettingsGlobal
  
  values$highlightedText <- ""                         # part of a system to copy any text highlighted with mouse in browser i.e. from interview quotes and insert into the edge information
  
  observeEvent(input$highlightedText,{
    if (!is.null(input$highlightedText)) if ("" != (input$highlightedText)) values$highlightedText <- paste0(values$highlightedText," ... ",input$highlightedText)
  })
  
  loaded <- F                         # whether loaded from url permalink
  makeReactiveBinding("loaded")
  
  first <- T
  makeReactiveBinding("first")
  
  values$fromStack=NULL
  values$toStack=NULL
  values$issaved=F
  values$foundIDs=NULL
  
  # This keeps a record of the previous page, and so ensures that we only update
  # the visNetwork if we transition to/from the 'Code' tab
  page <- reactiveValues(old = "", change = 0)
  observeEvent(input$sides, {
    if(page$old == "Code"){
      page$change = page$change+1
    } else if(input$sides == "Code"){
      page$change = page$change+1
    }
    page$old = input$sides
    
  })
  
  inputtitl <- ""
  makeReactiveBinding("inputtitl")
  
  # ts <- reactiveValues(counter = 0)
  
  # findset function to transfer user settings to values$settings -----------
  # this is important and a bit clunky. I think it sucks in the two settings 
  # tables, combines them with defaults in case anything is missing, and then
  #  providess the value of the setting sought. probably most ripe for 
  #  rationalisation
  
  findset <- function(tex, which = "current",global=TRUE,v=values) {
    # if(tex=="diagrambackground") browser()
    if(global) {
      x <-  v$settingsGlobal
      d <-  defaultSettingsGlobal
    } else  {
      x <- v$settings
      d <- defaultSettings
    }
    if (which == "current") {
      # browser()
      x <- bind_rows(d ,x)
    } else if (which == "default") {x=d}   #WTF - its right but it is stupid
    
    inp=input[[paste0("input",tex,collapse = "")]]
    
    if(which!="original" & !is.null(inp)){
      x=inp
    } else {
      x <- x %>%
        mutate_all(replaceNA) %>%
        mutate(labs = paste0(type, setting)) %>%
        filter(labs == tex) %>%
        pull(value) %>%
        last()
    }
    
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
              bind_rows(defaultNodes %>% filter(F))
            
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
  
  defaultEdges <- data.frame(
    # id = "1",
    from =  1,
    to =  2,
    label = "",
    strength = .5,
    trust =  .5,
    sensitivity = .5,
    specificity =  .5,
    statement =  1,
    quote =  "",
    full.quote =  "",
    combo.type = "",
    definition.type =  "",
    # title="" ,
    # frequency="1",
    # width="" ,
    # color="" ,
    # color.opacity="",
    # blah=.5
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
    
    rhandsontable(
      values$statements[, ],
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
    df <- read_csv(input$up.nodes$datapath) %>%
      bind_rows(defaultNodes %>% filter(F))
    
    values$graf <- tbl_graph(df, values$graf %>% edges_as_tibble())
    doNotification("Updated variables")
  })
  
  id.finder <- function(label, node.df) {
    sapply(label, function(x) {
      (node.df$label == x) %>% which() %>% first()
    })
  }
  
  observeEvent(input$up.edges, {
    req(input$up.edges)
    max <- (values$graf %>% nodes_as_tibble() %>% nrow()) + 1
    
    df <- read_csv(input$up.edges$datapath)[, ]
    # browser()
    
    if (input$use.labels) {
      df <- df %>%
        mutate(
          from = id.finder(from, values$graf %>% nodes_as_tibble()),
          to = id.finder(to, values$graf %>% nodes_as_tibble())
        )
    }
    
    df <- df %>%
      mutate(from = as.numeric(from), to = as.numeric(to)) %>%
      select(one_of(xc("from to label strength trust statement"))) %>%
      filter(from < max & to < max) %>%
      bind_rows(defaultEdges %>% filter(F))
    
    
    
    if (select(df, from, to) %>% unlist() %>% max() > max) doNotification("You have edges which don't make sense",level=2)
    # browser()
    values$graf <- tbl_graph(values$graf %>% nodes_as_tibble(), df)
    doNotification("Updated arrows")
  })
  
  # observe import statements -----------------------------------------------
  
  observeEvent(input$up.statements, {
    req(input$up.statements)
    # browser()
    vstat <- read_csv(input$up.statements$datapath)[, ] 
    
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
        sliderInput(
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
        if("source" %in% colnames(values$statements))actionButton("overview_col", label = "Whole source"),                                  #   if one interview source has made more than one statement, show all of them
        style = "display:inline-block;margin-left:20px"
      )
    )
  })
  
  values$obsList <- list()                                                #   to show all the statemtns from one source
  
  observeEvent(input$overview_col,{
    vs=values$statements
    # col=findset("diagramoverview_column")
    if("source" %in% colnames(vs)){
      pointer=vs$source[values$pag]
      content=vs$statement[(vs$source==pointer)]
    } else content <- "No such column"
    showModal(modalDialog(
      title = "All the statements from this source",footer=NULL,easyClose = T,size="l",
      tagList(
        lapply(content,function(x){
          
          btName=paste0("gosource",x)
          
          if (T|is.null(values$obsList[[btName]])) {
            # make sure to use <<- to update global variable obsList
            values$obsList[[btName]] <- observeEvent(input[[btName]], {
              updatePageruiInput(session,"pager",page_current = as.numeric(x))
            })
          }
          
          tagList(
            # actionButton(paste0("gosource",x),"Go!"),
            if(str_detect(vs$text[x],"pdf$")) {
              tags$iframe(src="pdf.pdf",style="height:800px; width:100%;scrolling=yes")
            } else {
              actionLink(btName,label = vs$text[x])
            },
            br()
          )
        })
      )
    ))
  })
  
  # ** display statements one by one ----
  
  output$displayStatementPanel <- renderUI({
    
    tagList(
      icon("quote-left"),
      span(values$statements$text[values$statements$statement==values$pag], class = "textbanner", id = "textbanner"),
      # span(add_highlight((ve$quote[ve$statement==values$pag])[1],(values$statements$text[values$statements$statement==values$pag])), class = "textbanner", id = "textbanner"),
      hr()
    )
  })
  
  observeEvent({c(input$pager,input$timeslider)},{
      if (!is.null(input$pager)) {
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
  
  
  # OLD add edges observer ----
  # not using this any more but not prepared to delete it yet
  
  observeEvent({
    c(
      input$updateE3,
      input$updateE2,
      input$updateE,
      input$updateE_crowd
    )  }, {
      
      
      # browser()
      # browser()
      
      ### NEW
      qq <- if (!is.null(input$quote)) {
        input$quote %>% as.character()
      } else qq=""
      
      # qq="" #FIXME
      # browser()
      if(!is.null(input$new1_edge)) {inpfrom <- input$new1_edge} else if(!is.null(input$new1_edge_crowd)) {inpfrom <- input$new1_edge_crowd}
      if(!is.null(input$new2_edge)) {inpto <- input$new2_edge} else if(!is.null(input$new2_edge_crowd)) {inpto <- input$new2_edge_crowd}
      if (length(req(inpto)) > 0 & length(req(inpfrom)) > 0) {
        if (length(inpfrom) > 1 & length(inpto) > 1) {
          doNotification("Only using the first of the to arrows as there is more than one from arrow")
          inpto <- inpto[1]
        }
        # browser()
        if(input$crowd){
          trust =.5
          strength =.5
          label = ""
          combo.type=""
          definition.type=""
          statement = 1
          quote = qq
          full.quote =""
        }
        
        newEdges <- data.frame(
          from = inpfrom, 
          to = inpto,
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
        
        new.nodes <- data.frame(label = newEdges %>% select(from, to) %>% unlist() %>% unique())
        # if(!is.null(input$combo)) new.nodes$fun=input$combo
        new.graph <- tbl_graph(new.nodes, newEdges)
        old.graph <- values$graf
        # browser()
        if (is.null(values$graf)) values$graf <- new.graph else values$graf <- graph_join(old.graph , new.graph,by="label")
        
        if(!is.null(input$combo)) {if(input$combo!="") values$graf=values$graf %>% N_ %>%
            mutate(fun=ifelse(inpto==label,input$combo,fun))
        
        }
        updateSelectizeInput(session, "new2_edge", selected = "")
        # updateSelectizeInput(session,"new1_edge",selected="")
        
        doNotification(paste0("Creating arrows: ", paste0(input$new1_edge, collapse = ", "), " to ", paste0(input$new2_edge, collapse = " - ")))
      }
      else {
        doNotification("You need to put at least one variable in each box",level=2)
      }
    })
  
  observeEvent(input$updateE2, {
    updateSelectizeInput(session, "new1_edge", selected = input$new2_edge)
  })
  
  observeEvent(input$updateE3, {
    updatePageruiInput(session,inputId = "pager",page_current = as.numeric(values$pag)+1)
  })
  
  # Add edges widget----
  
  output$add_edges_widget <- renderUI({
    varlist=values$graf %>% nodes_as_tibble() %>% pull(label) %>% unique() %>% as.character()
    varlist <- na.omit(varlist)

    tagList(
      div(
        div(
          # actionButton("updateE",
          #              "Add",
          #              style = "border:1px solid green;padding:15px;display:inline-block;width:7%"
          # ),
          # actionButton("updateE3",
          #              "Add & next",
          #              style = "border:1px solid green;padding:15px;display:inline-block;width:15%"
          # ),
          # 
          # # actionButton("saveb2", "Save", icon = icon("save"),
          # #              style = "border:1px solid green;padding:15px;display:inline-block;width:15%"),
          # 
          # actionButton("updateE2",
          #              "Add & chain",
          #              style = "border:1px solid green;padding:15px;display:inline-block;width:15%"
          #),
          div(style = "display:inline-block;width:5%"),
          div(textInput("arrLabel", NULL, value = "", placeholder = "label"), style = "display:inline-block;height:10px;width:20%"),
          div(style = "display:inline-block;width:5%"),
          div(selectizeInput("definition.type", NULL, choices = c("", "Defined, directed", "Defined, undirected")), style = "display:inline-block;width:20%"),
          div(textAreaInput("quote", NULL, value = values$highlightedText, placeholder = "quote",rows=3,width="100%"), style = "")
        ),
        div(
          id = "sliders",
          div(actionLink("flip", "Flip"), style = "display:inline-block;"),
          div(style = "display:inline-block;width:5%"),
          div(sliderInput("strength", "Strength", min = -1, max = 1, step = .1, value = .5, ticks = F), style = "display:inline-block;width:40%"),
          div(style = "display:inline-block;width:5%"),
          div(sliderInput("trust", "Trust", min = 0, max = 1, step = .1, value = .5, ticks = F), style = "display:inline-block;width:40%"),
          style = "margin-top:20px"
        )
      )
    )
  })
  
  observeEvent(input$flip, { 
    updateSliderInput(session, inputId = "strength", value = -input$strength)
  })
  
  
  output$combo <- renderUI(if(T){
    div(if (length(input$new1_edge) > 1) {
      selectizeInput("combo", "Arrows interact?",
                     choices = c("", "AND", "OR","min","max","SAME","mean","sum"), width = "150px", selected = NULL, 
                     options =
                       list(create = T, placeholder = "", onInitialize = I('function() { this.setValue(""); }')),
      )
    })
  })
  
  output$addNewNodeButton=renderUI({
    if( !is.null(input$selectboxvalue)){
      if(length((values$foundIDs))==0 && input$selectboxvalue!=""){
        actionButton(inputId = "addNewNode","Add new node")
      }
    }
    
  })  
  
  output$selectbox=renderUI({
    tagList(
      textInput("selectboxvalue","Type to select",placeholder="Type to select variables in the diagram"),
      
      if(is.null(values$fromStack)) {
        actionButton("selectFrom","Start arrow(s) with selected variables(s)")
      } else if(!is.null(input$net_selected)){
        actionButton("selectTo","Finish arrow(s) with the selected variable")
        # p(as.character(unlist(values$fromStack)))
      }
    )
  })
  
  
  observeEvent(input$selectFrom,{
    values$fromStack <- c(input$net_selected,values$foundIDs) %>% unique
    visNetworkProxy("net") %>% 
      visUpdateNodes(tibble(id=values$fromStack,color.background="gold"))
    
    visNetworkProxy("net") %>% 
      visSelectNodes(id=NULL)
    # updateSelectizeInput("new1_edge","asdf")#input$selectboxvalue %>% str_trim)
  })
  
  # observeEvent(input$selectTo,{
  #   visNetworkProxy("net") %>% 
  #     visSelectNodes(id=NULL)
  #   
  # })
  observeEvent(input$selectTo,{
    # browser()
    qq <- if (!is.null(input$quote)) {
      input$quote %>% as.character()
    } else qq=""
    
    # qq="" #FIXME
    # browser()
    inpfrom <- req(values$fromStack)
    inpto <- req(input$net_selected)[1]
    
    newEdges <- tibble(
      from = inpfrom, 
      to = inpto,
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
    
    values$fromStack <- NULL
    values$foundIDs <- NULL
    visNetworkProxy("net") %>% 
      visSelectNodes(id=NULL)
  })
  
  # textbox search nodes and highlights them --------------------------------
  
  observeEvent(c(input$selectboxvalue),{
    if(req(input$sides)=="Code"){
      
      req(values$grafAgg2)
      
      if(input$selectboxvalue!=""){
        vag <- values$grafAgg2 %>% nodes_as_tibble
        
        ids=vag %>% 
          pull(label) %>% 
          tolower %>% 
          str_detect(input$selectboxvalue %>% tolower) %>% 
          which
        
        if(length(ids) !=0 && length(ids)<nrow(vag)){
          visNetworkProxy("net") %>%
            visSelectNodes(id=ids) 
          
        }
        values$foundIDs <- ids
      }
    } 
  })
  
  observeEvent(input$addNewNode,{
    # browser()
    if(length(values$foundIDs)==0 && (input$selectboxvalue!="")){
      values$graf <- values$graf %>% 
        bind_nodes(tibble(label=input$selectboxvalue))
      
      values$fromStack <- nrow(values$graf %>% nodes_as_tibble)
      doNotification("Added new node",2)
    }
    
  })
  
  
  observeEvent(req(input$pager),{
    vno <- req(values$grafAgg2) %>% nodes_as_tibble
    # browser()
    if (!is.null(values$pag) & nrow(vno)>0) {
      ids <- vno %>%
        mutate(sel=ifelse(str_detect(statement, paste0("(,|^)", as.character(values$pag), "(,|$)")),T,F)) %>%
        pull(sel) %>%
        which
      
      visNetworkProxy("net") %>%
        visSelectNodes(id = ids)
    }
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
      tagList(
        if (length(input$net_selected)>0) {
          tagList(
            div(
              span(
                paste0("Delete variable ", input$net_selected %>% paste0(collapse=";"),"? "),
                style="display:inline-block;margin-right:5px"
              ),
              actionButton("deleteVarForm", "Delete!"),
              style = "background-color:white;padding:10px;border-radius:5px"
            )
          )
        },
        hr(style="margin:2px")
      )
    }
    
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
      # clu=vg$cluster[whichtarg]
      # if(clu!=""){
      #   whichtarg=
      # } #could highlight all the rows in a cluster, but would have to allow for multiple selection already
      
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
  
  output$filters=renderUI({
    lapply(colnames(values$statements %>% select(-text)),function(y){
      x=values$statements[[y]]
      u=unique(x) %>% na.omit()
      if(length(u)>1 & length(u)<12){
        div(checkboxGroupButtons(paste0("filters",y),y,choices=sort(u),selected=u),style="display:inline-block")
      }
    })
  })  
  
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
                        value=if(is.null(findset(rt))) findset(rt,which="original") else findset(rt),
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
  
  output$settingsTable <- renderRHandsontable({
    vs <- values$settings %>% mutate_all(as.character)
    
    ds <- defaultSettings %>% mutate_all(as.character)
    
    vs <- bind_rows(vs, ds) %>%
      distinct(type, setting, .keep_all = T)
    
    
    rhandsontable(vs %>%
                    mutate(type = factor(type), condition = factor(condition, levels = c("always", "if...", "conditional on:"))), height = NULL, rowHeaders = FALSE, usetypes = T) %>%
      hot_context_menu(allowRowEdit = T) %>%
      hot_cols(colWidths = c(80,120,250,80))
    
  })
  
  observeEvent(input$settingsTableUp, {
    doNotification("updating from settingsTable")
    values$settings <- hot_to_r(input$settingsTable)
  })
  
  observeEvent(input$settingsTableGlobalUp, {
    doNotification("updating from settingsTableGlobal")
    values$settingsGlobal <- hot_to_r(input$settingsTableGlobal)
  })
  
  observeEvent(input$fitaction, {               # restore network to normal zoom
    visNetworkProxy("net") %>%
      visFit() 
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
      tagList(
        h4("Download your csv files"),
        hr(),
        lapply(csvlist,function(x){
          tagList(a(href=paste0(values$current,"-",x,".csv"),x),hr())
        })
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
    
    page$change
    vals <- isolate(values)
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
      
      # prepare ved
      
      
      
      # browser()
      ved <- prepare_ved(ved)
      
      # ved join statements--------------------------------
      
      ved <- ved_join(ved, values$statements)
      
      # ved edge merge ----
      
      if (findset("arrowmerge", v = vals) %>% as.logical() ) {
        ved <- ved %>%
          group_by(from, to)
        
        legend <- paste0(legend, "</br>Multiple arrows between pairs of variables collapsed into one")
        
      } else {
        ved <- ved %>%
          group_by(row_number())
      }
      
      doNotification("merge edge aggregation")    
      
      ved <- ved %>%
        mutate(statement=as.character(statement)) %>%
        mutate(wstrength = strength * trust) 
      # browser()
      
      ved <- ved %>%
        mutate_if_sw(is.numeric, .funs = list(sum=sumfun,mean=meanfun)) %>%
        mutate_if_sw(is.character, .funs = catfun) %>%
        mutate(frequency = n()) %>% 
        summarise_all(.funs = funs(first)) %>%
        ungroup() %>%
        mutate(title = paste0(frequency, gsub("[^[:alnum:][:space:]]", "", label), separate = "- "))
      
      doNotification("min freq aggregation")    
      
      # edge minimum freq ----
      
      if(this_tab!="Code"){
        ved <- ved %>%
          filter(frequency > findset("arrowminimum.frequency", v = vals))
      }
      # browser()
      # cat("merge")
      
      # join edges with nodes------------------------------------------------
      
      doNotification("join to edges aggregation")    
      
      if(findset("variablejoinedges", v = vals) %>% as.logical | 
         values$settings %>% filter((condition!="always")) %>% nrow() %>% `>`(0) | 
         values$settingsGlobal %>% pull(value) %>% unique %>% str_detect("mean|frequency|sum") %>% any(na.rm=TRUE)
      ){ #todo, should list any functions. variablejoinedges is pointless
        
        # browser()
        
        # doNotification("merging nodes and arrows")
        ved.from <- ved %>%
          mutate(id = from) %>%
          group_by(id) %>%
          mutate_if_sw(is.numeric, .funs = list(sum=sumfun,mean=meanfun)) %>%
          mutate_if_sw(is.character, .funs = catfun) %>%
          summarise_all(.funs = funs(first)) %>%
          ungroup() %>%
          rename_all(function(x)paste0("from.",x)) %>% 
          rename(id=from.id)
        
        vno <- vno %>%
          mutate(id = row_number()) %>%
          left_join(ved.from, by = "id")
        
        ved.to <- ved %>%
          mutate(id = to) %>%
          group_by(id) %>%
          mutate_if_sw(is.numeric, .funs = list(sum=sumfun,mean=meanfun)) %>%
          mutate_if_sw(is.character, .funs = catfun) %>%
          summarise_all(.funs = funs(first)) %>%
          ungroup() %>%
          rename_all(function(x)paste0("to.",x)) %>% 
          rename(id=to.id)
        
        vno <- vno %>%
          mutate(id = row_number()) %>%
          left_join(ved.to, by = "id")
        
        # add from and to scores for nodes
        
        vnofrom <- vno %>% 
          select(starts_with("from.")) %>% 
          select_if(is.numeric) %>% 
          as.matrix
        
        vnoto <- vno %>% 
          select(starts_with("to.")) %>% 
          select_if(is.numeric) %>% 
          as.matrix
        
        vnomean=apply(simplify2array(list(vnofrom,vnoto)),c(1,2),meanfun) %>% as.tibble()
        vnosum=apply(simplify2array(list(vnofrom,vnoto)),c(1,2),sumfun) %>% as.tibble()
        
        colnames(vnomean)=str_remove_all(colnames(vnoto),"^to.") %>% paste0("mean_",.)
        colnames(vnosum)=str_remove_all(colnames(vnoto),"^to.") %>% paste0("sum_",.)
        
        vno=bind_cols(vno,vnomean,vnosum)
        
        # browser()
        vno <- vno %>%
          ungroup() %>%
          mutate(
            frequency = sum_frequency_sum,
            trust = sum_trust_sum,
            strength = sum_strength_sum_sum,
            wstrength = sum_wstrength_sum_sum) %>% 
          mutate(
            edgelabels = paste0(from.label, to.label),
            statement = paste0(from.statement, to.statement),
            quote = paste0(from.quote, to.quote)
          ) 
        
        if("mean_key1_mean_mean" %in% colnames(vno))vno=vno %>% mutate(key1=mean_key1_mean_mean) 
        if("mean_key2_mean_mean" %in% colnames(vno))vno=vno %>% mutate(key2=mean_key2_mean_mean) 
        
        # nsets=defaultSettings %>%
        #   filter(type=="variable") %>%
        #   select(setting,value)
        #
        
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
        
      }
      
      doNotification("format aggregation")    
      
      # ...cond formatting----
      
      palettes=1
      conds <- values$settings %>% filter((type == "variable" | type == "arrow") )
      
      conds[is.na(conds)] <- ""
      
      if (nrow(conds) > 0) {
        
        # see if we need to have different palettes:
        
        conds <- conds %>%
          mutate(x = ifelse(str_detect(setting, "color") & condition == "conditional on:", 1, 0), palette = cumsum(x)) %>%
          select(-x) %>%
          filter(!is.na(condition))
        
        ## this is wasteful because could just do nodes once and edges once.
        
        for (i in 1:nrow(conds)) {
          row <- conds[i, ]
          if (row$type == "variable") df <- vno else df <- ved
          orig <- df
          
          typenow <- ifelse(row$type == "variable", "variable", "arrow")
          
          # browser()
          
          if (row$condition == "if...") {
            if(row$ifcolumn %in% colnames(df)){
              # browser()
              ifcol <- df[, row$ifcolumn] %>% unlist() %>% as.vector()
              comp <- row["comparison"] %>% unlist() %>% as.vector()
              
              ifcol[is.na(ifcol)] <- ""
              
              if (comp == "equal") {
                theseones <- ifcol == row$filter
              } else
                if (comp == "not equal") {
                  theseones <- ifcol != row$filter
                } else
                  if (comp == ">") {
                    theseones <- ifcol > row$filter
                  } else
                    if (comp == "<") {
                      theseones <- ifcol < row$filter
                    } else
                      if (comp == "contains") theseones <- str_detect(ifcol, row$filter)
              # browser()
              df[theseones, row$setting] <- row$value
              df[!theseones, row$setting] <- findset(paste0(typenow, row$setting), which = "default",global = F, v = vals)
              
              legend <- paste0(legend, "</br>", typenow, " ", row$setting, " set to ", row$value, " if ", row$ifcolumn, " ", row$comparison, " ", row$filter, " ")
            }
          }
          else if (row$condition == "conditional on:") {
            if (row$ifcolumn %in% colnames(df)) {
              
              # browser()
              # browser()
              
              rc <- df[, row$ifcolumn]
              if (row$setting %in% xc("label tooltip")) {
                df[, row$setting] <- rc
              }
              else if (row$setting %in% xc("frequency")) {
                df[, row$setting] <- as.numeric(rc)
              }
              else {
                rc[is.na(rc)] <- Inf
                # browser()
                mr <- dense_rank(unlist(rc))
                # mr[is.na(mr)]=""
                # mr=as.numeric(mr)
                nr <- nrow(df)
                if (str_detect(row$setting, "color") & !str_detect(row$setting, "opacity")) {
                  df[, row$setting] <- colorRampPalette((str_split(findset(paste0("diagrampalette",palettes), v = vals),","))[[1]] %>% str_trim)(max(as.numeric(mr)))[mr]
                  palettes=palettes+1
                }
                if (row$setting %in% xc("font.size")) {
                  df[, row$setting] <- (mr + findset("variablefont.size.floor",global = T, v = vals) %>% as.numeric) * findset("variablefont.size.scale",global = T, v = vals) %>% as.numeric / nr
                }
                if (row$setting %in% xc("size")) {
                  df[, row$setting] <- mr / nr
                }
                if (any(row$setting %in% xc("width"))) {
                  df[, row$setting] <- mr * findset("arrowconditionalwidthscaling", v = vals) %>% as.numeric() / max(mr)
                }
                if (any(row$setting %in% xc(" borderWidth"))) {
                  df[, row$setting] <- mr * findset("variableconditionalwidthscaling", v = vals) %>% as.numeric() / max(mr)
                }
                if (row$setting %in% xc("color.opacity")) {
                  df[, row$setting] <- mr / max(mr)
                }
                if (row$setting %in% xc("shape")) {
                  df[, row$setting] <- shapelist[mr]
                }
                if (row$setting %in% xc("style")) {
                  df[, row$setting] <- xc("dotted dashed solid")[mr]
                  if (length(unique(mr)) > 3) showNotification("I only have three styles")
                }
              }
              
              legend <- paste0(legend, "</br>", typenow, " ", row$setting, " shows ", row$ifcolumn, "")
            } else {
              doNotification(paste0("there is no such column ",row$ifcolumn),2)
            }
          } else  {
            # if (row$ifcolumn %in% colnames(df)) {
            # if(row$setting == "color.background") browser()
            rv=row$value
            if(allNum(rv))rv=as.numeric(rv)
            # df[, row$setting] <- rv
            
            # rc <- df[, row$ifcolumn]
            # }else {
            #   doNotification("there is no such column")
            # }
          }
          
          if (!identical(df, orig)) {
            if (row$type == "variable") vno <- df else ved <- df
          }
        }
        
        # browser()
      }
      
      # rationalise----
      
      doNotification("final aggregation")    
      
      ved=ved %>% 
        mutate(strength=strength_mean) %>% 
        mutate(wstrength=wstrength_mean) 
      # if("mean_key2_mean_mean" %in% colnames(vno))vno=vno %>% mutate(key2=mean_key2_mean_mean) 
      
      # iconify hardcoded----
      # browser()
      ved <- ved %>%
        mutate(label = replace_na(label,"")) %>% 
        mutate(label = ifelse(strength<0,paste0("🔀 ",label),label)) %>% 
        mutate(combo.type <- replace_na(combo.type,"")) %>% 
        mutate(arrows.middle.enabled = ifelse(combo.type == "", F, T)) %>%
        mutate(label = paste0(label, ifelse(arrows.middle.enabled, paste0(" ", combo.type), ""))) %>%
        mutate(dashes = definition.type != "") %>% 
        mutate(arrows.to = definition.type != "Defined, undirected")
      
      if (!is.null(legend)) {
        if (legend != "") values$legend <- glue("</br><b style='font-color:red;'>Legend:</b>{legend}</br></br></br></br>")
      }
      
      vno <- vno %>%
        mutate(label = if_else(value>0,paste0(label," ♥"), label)) %>%
        mutate(label = if_else(value<0,paste0(label," ☹"), label)) %>%
        mutate(label = str_replace_all(label, "///", "\n")) 
      
      labelmaker=function(tex,df,sep="<br>"){
        x=(str_split(tex,",")[[1]]) %>% 
          str_trim 
        lab=""
        for(i in x){
          if(i %in% colnames(df))  lab=paste(lab,ifelse(i=="label","",paste0(sep,"",i,": ",collapse="")),unlist(df[,i]))
        }
        lab %>% 
          str_remove_all("^  \\| details:  ")  %>% 
          str_replace_all("\\| frequency: ","") 
        # str_replace_all("\\| frequency: ([0-9]*)","\\(\\1\\)") 
        
      }
      
      vno <- vno %>% 
        mutate(title=labelmaker(findset("variabletooltip", v = vals),vno),
               label=labelmaker(findset("variablelabel", v = vals),vno,sep=" | ")
        )
      
      ved <- ved %>% 
        mutate(title=labelmaker(findset("arrowtooltip", v = vals),ved),
               label=labelmaker(findset("arrowlabel", v = vals),ved,sep="\n")
        )
      
      # # wrapping ----
      # vno = vno %>%
      #   mutate(label = str_wrap(label, findset("variablewrap")))
      # 
      # # ved tooltips
      # 
      # 
      # 
      ved <- ved %>%
        mutate(label = str_replace_all(label, "///", "\n")) %>%
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
  
  observe({
    
    vga <- (req(values$grafAgg2)) 
    
    if (T) {
      
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
          background = findset("diagrambackground")
          ,
          height=findset("diagramheight") %>% paste0("px"),
          width=findset("diagramwidth") %>% paste0("px")
          
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
          collapse = TRUE,
          highlightNearest = list(
            enabled = T,
            degree = 99,
            hover = FALSE,
            algorithm = "hierarchical"
          ),
          nodesIdSelection = F
        ) %>%
        visConfigure(enabled = input$codeCollapse == "Advanced options",
                     container = "advancedAnchor") %>%
        visEvents(select = "function(edges) {
                Shiny.onInputChange('current_edge_id', edges.edges);
                ;}") %>%
        visEvents(select = "function(nodes) {
                Shiny.onInputChange('net_selected', nodes.nodes);
                ;}")
      
      if (!all(na.omit(vga$group) == "")) {
        # vn <- vn %>%
        #   visGroups() %>%
        #   visClusteringByGroup(groups = groups, label = "Group: ")
      }
      
      if (findset("diagramlayout") == "layout_with_sugiyama") {
        vn <- vn %>%
          visIgraphLayout(layout = "layout_with_sugiyama", randomSeed = 123, smooth = T, type = "full")
        
        if (findset("diagramorientation") %in% xc("LR RL")) {
          # browser()
          tmp <- vn$x$nodes$x
          vn$x$nodes$x <- vn$x$nodes$y
          vn$x$nodes$y <- tmp
          vnxn <- vn$x$nodes
          levels=(length(unique(vnxn$x)))
          # b# rowser()
          vnxn=vnxn %>% 
            group_by(x) %>% 
            mutate(ran=min_rank(y),y=rescale(ran,to=c(-1,1))*3.5 + rnorm(1,0,.05)) %>% 
            ungroup()               #had to put in a tiny bit of rnorm to stop some artefacts in visnetwork when nodes have same y
          # mutate(len=n(),ran=min_rank(y)-.5,y=ran*levels/(len*3))
          vnxn=vnxn %>% 
            mutate(x=x*3)
          vn$x$nodes = vnxn
          
          
          
        } else if (findset("diagramorientation") %in% xc("DU RL")) {
          vn$x$nodes$x <- 1 - vn$x$nodes$x
          vn$x$nodes$y <- 1 - vn$x$nodes$y
        }
        
        
      } else {
        vn <- vn %>%
          visIgraphLayout(layout = "layout_in_circle", randomSeed = 123, smooth = T, type = "full", physics = T)
      }
      
      if (findset("diagramphysics") %>% as.logical()) {
        vn <- vn %>%
          visPhysics(barnesHut = list(avoidOverlap = .7))
      }
      
      
      
      vn <- vn %>%
        visNodes(
          shadow = list(enabled = T, size = 10),
          # widthConstraint=(5500/(levels+5))    , #  numeric(findset("variablewidth")) , #,300-(levels*10),#,(300*levels)-9,
          widthConstraint=as.numeric(findset("variablewidth")) , #,300-(levels*10),#,(300*levels)-9,
          color =
            list(
              background =
                findset("variablecolor.background",global=F) %>% toRGB(findset("variablecolor.opacity",global=F) %>% as.numeric()),
              border = findset("variablecolor.border",global=F)
              
              # ,
              # highlight=list(
              #   background=findset("variablecolor.background.highlight",global=F)
              #   ,
              #   border=findset("variablecolor.border.highlight",global=F)
              #
              # )
            ),
          font =
            list(
              color =
                findset("variablefont.color",global=F),
              size = findset("variablefont.size",global=F)
            ),
          hidden = F,# findset("variablehidden",global=F) %>% as.logical(),
          scaling = list(label = list(enabled = F)),
          shape = findset("variableshape",global=F),
          # shapeProperties = list("borderDashes=T"),
          group = T,#findset("variablegroup",global=F),
          borderWidth = findset("variableborderWidth",global=F),
          # widthConstraint=4,
          # widthConstraint = =4,
          # size =
          #   findset("variablesize",global=F),
          
          physics = findset("diagramphysics")
          # ,
          # ,
          # widthConstraint=10
          
          # widthConstraint=findset("variablewidth") %>% as.numeric # %>% ifelse(.>0,.,NA)
        ) %>%
        visEdges(
          smooth = F,
          arrowStrikethrough = F,
          shadow =
            list(enabled = F, size = 5),
          # width =
          #   findset("arrowwidth"),
          font =
            list(
              color =
                findset("arrowfont.color",global=F),
              background =
                "#FFFFFF80",
              size = findset("arrowfont.size",global=F)
            ),
          physics =
            F,
          arrows =
            list(to = list(enabled = TRUE), middle = list(type = "circle", scaleFactor = .5))
          # ,
          # dashes = findset("arrowdashes") %>% as.logical()
        )
      
      values$net <- vn
      
      doNotification("Produced viz")
      
      if (T) {
        
        

        
        # browser()
        
      }
    }
  })
  
  
  output$net <- renderVisNetwork({
    doNotification("render viz")
    # browser()
    # doNotification("rendered viz")
    values$net
    
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
      div(
        
        actionButton("fontminus", "fontscale-", icon = icon("picture")),
        style = "display:inline-block;margin-right:130px;width:10px"
      ),
      div(
        
        actionButton("fontplus", "fontscale+", icon = icon("picture")),
        style = "display:inline-block;margin-right:110px;width:10px"
      ),
      div(
        
        actionButton("fontscaleminus", "font-", icon = icon("picture")),
        style = "display:inline-block;margin-right:90px;width:10px"
      ),
      div(
        
        actionButton("fontscaleplus", "font+", icon = icon("picture")),
        style = "display:inline-block;margin-right:70px;width:10px"
      ),
      div(
        
        actionButton("png", "PNG", icon = icon("picture")),
        style = "display:inline-block;margin-right:50px;width:10px"
      ),
      # ,
      div(actionButton("fitaction", "Fit"), style = "display:inline-block;margin-right:20px"),
      class = "bigpicbut"
      ,style="z-index:999 !important")
    # h1("asdfasdfasdf")
  })
  # 
  # observe({
  # })
  
  observeEvent(input$fontplus,{
    values$settingsGlobal=values$settingsGlobal %>% 
      mutate(value=if_else(setting=="font.size.floor",as.character(as.numeric(value)*1.1),value)) 
  })
  
  observeEvent(input$fontminus,{
    values$settingsGlobal=values$settingsGlobal %>% 
      mutate(value=if_else(setting=="font.size.floor",as.character(as.numeric(value)*.9),value)) 
  })
  
  observeEvent(input$fontscaleplus,{
    values$settingsGlobal=values$settingsGlobal %>% 
      mutate(value=if_else(setting=="font.size.scale",as.character(as.numeric(value)*1.1),value)) 
  })
  
  observeEvent(input$fontscaleminus,{
    values$settingsGlobal=values$settingsGlobal %>% 
      mutate(value=if_else(setting=="font.size.scale",as.character(as.numeric(value)*.9),value)) 
  })
  
  output$savebut <- renderUI(div(
    div(
      list(
        
        # div(actionButton("render_button","Render"),style="display:inline-block"),
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
    {c(
      input$saveb ,
      input$updateE_crowd
    )}, {
      # browser()
      # doNotification("observing save button",7)
      
      if ("" != (input$titl)) {
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
    
    visSave(values$net, "www/export.html", selfcontained = T, background = "white")
    webshot::webshot("www/export.html", file = "www/export.png")
  })
  
  
  observeEvent(input$saveb,{
    output$savedMsg <- renderUI(if (!values$issaved) {
      div()
    } else {
      div(
        id = "savemsg",
        "Saved to this permanent link: ", tags$a(paste0(values$current), href = paste0("?permalink=", values$current)), style = "margin-bottom:10px"
      )
    })
  })
  
  
  
  
  # ++ crowsourcing / mobile interface --------------------------------------
  
  observe({
    if(input$crowd){output$net2 <- renderVisNetwork({
      doNotification("render viz")
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
    input$current_edge_id %>% as.character()
  })
  
  
  
  # session$onSessionEnded(stopApp) ## TODO take out
  
  
  
  
  jqui_draggable("#sel,#chbox,#saveb")
  
  
  # makeReactiveBinding("inputtitl")
  # browser()
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")
}
