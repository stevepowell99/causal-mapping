# sql ---------------------------------------------------------------------



if(T) {
  con <- isolate(dbConnect(RSQLite::SQLite(), "CMA"))
} else {
  con <-  isolate(DBI::dbConnect(RMariaDB::MariaDB(), user = "admin", password = "barnulf99",dbname = "CMA", host = "db1.c3sdt4rwfkjt.us-west-2.rds.amazonaws.com", port = 3306))
}

# browser()

# 
# loaded <- F # whether loaded from url permalink
# makeReactiveBinding("loaded")

projectFromURL <- reactiveVal("")



# logging on -----------
observeEvent(input$projectSelect,{
  
  req(input$userSelect)
  req((input$projectSelect))
  query <- isolate(parseQueryString(session$clientData$url_search))
  ql <- query[["permalink"]]
  projectFromURL <- reactiveVal(ql %>%  get_project_from_query())
  userFromURL <- reactiveVal(ql %>%  get_user_from_query())
  if(!is.null(ql)) {
    req(input$userSelect)
    req(input$projectSelect)
    # browser()
    updateSelectInput(session,"userSelect",selected=userFromURL())
    updateSelectInput(session,"projectSelect",selected=projectFromURL())
    }
  
  if (T) {
    for (fn in csvlist) {
    doNotification(glue("Loading {fn} from remote database"))
      # fnn <- paste0(filename, "-", fn, ".csv")
      # browser()
      
      # browser()
      iu <- req(input$userSelect)
      ip <- req(input$projectSelect)
      values[[fn]] <- tbl(con,fn) %>% 
        filter(project==ip, user==iu) %>% 
        select(-project,-user) %>% 
        collect() 
    }
    if(nrow(values[["nodes"]])>0)   {
      doNotification("Loaded project from database")
      values$edges$quote <- values$edges$quote %>% cleanfun
      values$edges$label <- values$edges$label %>% cleanfun
      values$edges$domain <- values$edges$domain %>% cleanfun
      values$statements$text <- values$statements$text %>% cleanfun
      values$statements_extra$value <- values$statements_extra$value %>% cleanfun
      values$nodes$label <- values$nodes$label %>% cleanfun
      values$nodes$details <- values$nodes$details %>% cleanfun
    }
    else {
      # browser()
      if(input$projectSelect!="blank"){
        doNotification("Saving new version")
        
      } else {
        doNotification("Creating new project")
        values$graf <- tbl_graph(
          defaultNodes[0, ],
          defaultEdges[0, ]
        )
        values$statements <- default.statements
        values$statements_extra <- default.statements_extra
        values$settingsGlobal <- defaultSettingsGlobal
        values$settingsConditional <- defaultSettingsConditional
        values$net <- NULL
        values$grafAgg2 <- NULL
      }
    }
    
  } 
  
  
  
  
  
  # browser()
  values$graf <- tbl_graph(values$nodes, values$edges)
  
  
  
  doNotification(glue("Loaded{nrow(values$graf %>% nodes_as_tibble)} variables from permalink"))
  # browser()
  
  
})


observeEvent(input$projectSaveAs,{
  req(input$projectSaveAs)
  if(input$projectSaveAs=="")  html("saveb", "Save")
})
  
observeEvent(input$projectSaveAs,{
  req(input$projectSaveAs)
  if(input$projectSaveAs!="")  html("saveb", "Save as") 
})
  
# observe({
#   req(input$projectSaveAs)
#   if((input$projectSaveAs)=="")   {
#     
#     if(input$projectSelect=="blank") {
#       disable("saveb")
#     } else {
#       enable("saveb")
#       html("saveb", "Save")
#     }
#   }
#   
# })

# user and save panel ----

observeEvent(c(input$userSelect),{
  lu <- req(input$userSelect)
  updateSelectInput(session,"projectSelect",choices=tbl(con,"nodes") %>% filter(user==lu) %>% pull(project) %>% unique() %>% c("blank",.))
})


observeEvent(input$projectSelectGo,{
  values <- req(values)
  # loggedProject(input$projectSelect)
})

observe({
  # lu <- req(input$userSelect)
  # projectChoices <- tbl(con,"nodes") %>% filter(user==lu) %>% pull(project) %>% unique()
  output$savebut <- renderUI(
  
  div(
    div(
      tagList(
        
        div(selectInput("userSelect",NULL,choices = userlist,width="100%"), class="myelement",style = ";width:15%"),
        div(selectInput("projectSelect",NULL,choices = "",width="100%"), class="myelement",style = ";width:30%"),
        div(
          actionButton("deleteProject", NULL, icon = icon("trash")),
          class="myelement",style = ";margin-left:5px;width:8%"
        ),
        div(textInput("projectSaveAs",NULL,placeholder = "Save as",width="100%"), class="myelement",style = ";width:30%"),
        div(
          actionButton("saveb", "Save", icon = icon("save")),
          class="myelement",style = ";margin-left:5px;margin-right:5px;width:10%"
        )
      ),
      style = "margin-bottom:-20px"
    )
  ))
})
observeEvent(input$deleteProject,{
  delete_from_sql(con,input$userSelect,input$projectSelect)
  
})


# observe save button -----------------------------------------------------


observeEvent(
  input$saveb,
  ignoreInit = TRUE, {
    
      # browser()
    
    if (!(input$projectSaveAs=="" & input$projectSelect=="blank")) {
      inputtitl <- gsub("[^[[:alnum:]|-]]*", "", input$projectSelect) %>% paste0(input$userSelect,"/",.)
      projectFromURL <- reactiveVal(inputtitl)
      
      
      values$nodes <- values$graf %>% nodes_as_tibble()
      values$edges <- values$graf %>% edges_as_tibble()
      
      project <- ifelse(input$projectSaveAs=="",input$projectSelect,input$projectSaveAs)
      
      for(c in csvlist){
      send_to_sql(values,con,input$userSelect,project,c)
        
      }

      
      doNotification("Saved")

            values$issaved <- T
      updateSelectInput(session,"userSelect",selected = input$userSelect) # to cascade changes
      updateTextInput(session,"projectSaveAs",value = "")
      
    } else doNotification("You have to give it a name")
  }
  
)





observeEvent(input$saveb, ignoreInit = TRUE, {
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
  
  
  # toggleClass("savemsg", "red")
  # delay(500, toggleClass("savemsg", "red"))
})


