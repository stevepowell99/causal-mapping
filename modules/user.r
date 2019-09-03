# sql ---------------------------------------------------------------------



if(T) {
  con <- isolate(dbConnect(RSQLite::SQLite(), "CMA"))
} else {
  con <-  isolate(DBI::dbConnect(RMariaDB::MariaDB(), user = "admin", password = "barnulf99",dbname = "CMA", host = "db1.c3sdt4rwfkjt.us-west-2.rds.amazonaws.com", port = 3306))
}



sess <- reactiveValues()

observe({
  doNotification("xx creating panel")
  
  sess$projectChoicesAll=tbl(con,"nodes") %>% select(project,user) %>% collect()
  
  query <- isolate(parseQueryString(session$clientData$url_search))
  ql <- query[["permalink"]]
  if(!is.null(ql)) {
    user <- (ql %>%  get_user_from_query)
    project <- (ql %>%  get_project_from_query)
    doNotification("update from query ..." %>% paste0(ql %>% get_project_from_query))
  } else {
    user <- "free"
    project <- "blank"
    
  }

    output$savebut <- renderUI({
  
      div(
        tagList(
          
          div(selectInput("userSelect",NULL,selected=user,choices = c(userlist),width="100%"), class="myelement",style = ";width:15%"),
          # div(actionButton("userSelectGo",NULL,icon=icon("star")), class="myelement",style = ""),
          div(selectizeInput("projectSelect",NULL,multiple=F,selected=project,choices = project,width="100%",
            options =
              list(create = T),
          ), class="myelement",style = ";width:30%"),
          # div(actionButton("projectSelectGo",NULL,icon=icon("star")), class="myelement",style = ""),
          div(actionButton("saveProject",NULL,icon=icon("save")), class="myelement",style = ""),
          div(
            actionButton("deleteProject", NULL, icon = icon("trash")),
            class="myelement",style = ";margin-left:5px;width:8%"
          )
        ),
        style = "margin-bottom:-20px"
      )
})
})

observeEvent(input$userSelect,{
  # query <- isolate(parseQueryString(session$clientData$url_search))
  req(input$projectSelect)
  proj <- isolate(input$projectSelect)
  
  isolate({
    
  # browser()
    sess$projectChoices <- sess$projectChoicesAll %>%  
      filter(user==req(input$userSelect)) %>% pull(project) %>% unique %>% c("blank",.)
    })
  
  # ql <- query[["permalink"]]
  # if(!is.null(ql)) {
  #   req(input$userSelect)
  #   updateSelectInput(session,"userSelect",selected=(ql %>%  get_project_from_query))
    updateSelectizeInput(session,"projectSelect",choices=sess$projectChoices,selected=proj)
  if((proj %in% sess$projectChoices))     updateSelectizeInput(session,"projectSelect",selected=proj)
  # doNotification("update from query ..." %>% paste0(ql %>% get_project_from_query))
  # } 
  # 
})




observeEvent({c(input$projectSelect)},{
  # req(input$projectS)
  # browser()
  if(input$projectSelect %in% c("blank",sess$projectChoices)){
  
  isolate({
      iu <- req(input$userSelect)
      ip <- req(input$projectSelect)
  })
    
    if(input$projectSelect=="blank"){
      
      disable("deleteProject")
      disable("saveProject")
      
      doNotification("Creating new project")
      
      values$nodes <- defaultNodes[1, ]
      values$edges <- defaultEdges[0, ]
      values$statements <- default.statements
      values$statements_extra <- default.statements_extra 
      values$settingsGlobal <- defaultSettingsGlobal
      values$settingsConditional <- defaultSettingsConditional
      # values$graf <- NULL
      values$grafAgg2 <- NULL
      values$graf <- tbl_graph(
        values$nodes,values$edges
      )
  } else {
    enable("deleteProject")
      enable("saveProject")
    
    for (fn in csvlist) {
    doNotification(glue("Loading {fn} from remote database for project: {ip}"))
      # fnn <- paste0(filename, "-", fn, ".csv")
      # browser()
      
      # browser()
      values[[fn]] <- tbl(con,fn) %>% 
        filter(project==ip, user==iu) %>% 
        select(-project,-user) %>% 
        collect() 
    }
    if(nrow(values[["nodes"]])>0)   {
      values$edges$quote <- values$edges$quote %>% cleanfun
      values$edges$label <- values$edges$label %>% cleanfun
      values$edges$domain <- values$edges$domain %>% cleanfun
      values$statements$text <- values$statements$text %>% cleanfun
      values$statements_extra$value <- values$statements_extra$value %>% cleanfun
      values$nodes$label <- values$nodes$label %>% cleanfun
      values$nodes$details <- values$nodes$details %>% cleanfun
    }
    }
      
  values$statements_extra <- values$statements_extra %>%
    spread(key,value) 
  
  values$graf <- tbl_graph(values$nodes, values$edges)
  
  
  
  doNotification(glue("Loaded{nrow(values$graf %>% nodes_as_tibble)} variables from permalink"))
    
  # browser()
  } else {
    doNotification("There is no such project, leaving old project")
  } 
  
})


  
# observe({
#   req(input$projectSaveAs)
#   if((input$projectSaveAs)=="")   {
#     
#     if(input$projectSelect=="blank") {
#       disable("saveProject")
#     } else {
#       enable("saveProject")
#       html("saveProject", "Save")
#     }
#   }
#   
# })

# user and save panel ----

# observeEvent(c(input$userSelectGo),{
#   # isolate({lu <- req(input$userSelect)})
#   # updateSelectizeInput(session,"projectSelect",choices=tbl(con,"nodes") %>% filter(user==lu) %>% pull(project) %>% unique() %>% c("blank",.))
# })




observeEvent(input$deleteProject,{
  delete_from_sql(con,input$userSelect,input$projectSelect)
  sess$projectChoicesAll=tbl(con,"nodes") %>% select(project,user) %>% collect()
  updateSelectInput(session,"userSelect",selected=input$userSelect) # to cascade changes
  updateSelectizeInput(session,"projectSelect",selected="blank")
  
  
  
})


# observe save button -----------------------------------------------------


observeEvent(
  c(input$saveProject),
  ignoreInit = TRUE, {
    
    # if(input$projectSelect %in% isolate(sess$projectChoices))
    req(values$nodes)
    req(values$edges)
    req(input$userSelect)
    
      # browser()
    
    if ((input$projectSelect!="blank" & input$projectSelect!="")) {
      # inputtitl <- gsub("[^[[:alnum:]|-]]*", "", input$projectSelect) %>% paste0(input$userSelect,"/",.)
      # projectFromURL <- reactiveVal(inputtitl)
      
      
      values$nodes <- values$graf %>% nodes_as_tibble()
      values$edges <- values$graf %>% edges_as_tibble()
      
      project <- input$projectSelect
      v=values
      ius=input$userSelect
      future({
      # browser()
      for(c in csvlist){
      send_to_sql(v,con,ius,project,c)
        
      }
      })
      
      doNotification("Saved")

            values$issaved <- T
      # updateSelectInput(session,"userSelect",selected = input$userSelect) # to cascade changes
      # updateTextInput(session,"projectSaveAs",value = "")
      
    } else doNotification("You have to give it a name")
  }
  
)





observeEvent(input$saveProject, ignoreInit = TRUE, {
  # browser()
  link <- paste0(input$userSelect,"/",input$projectSelect)
  output$savedMsg <- renderUI({
    if (F) {
    # if (!values$issaved) {
      div()
    } else {
      div(
        id = "savemsg",
        "Saved to this permanent link: ",
        tags$a(
          link,
          href = paste0("./?permalink=",link)
        )
      )
    }
  })
  
  
})


