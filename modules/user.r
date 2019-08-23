# sql ---------------------------------------------------------------------



# s <- paste0("CREATE DATABASE CMA ")
mydb <-  DBI::dbConnect(RMariaDB::MariaDB(), user = "admin", password = "barnulf99",dbname = "CMA", host = "db1.c3sdt4rwfkjt.us-west-2.rds.amazonaws.com", port = 3306)

# DBI::dbExecute(mydb, as.character('set character set "utf8"'))
# s="ALTER DATABASE CMA CHARACTER SET utf8 COLLATE utf8_general_ci;"



# s="SHOW TABLES;"
# rs <- dbSendQuery(mydb, s)
# df <-  fetch(rs, n = -1)
# 
# dbDisconnect(mydb)


# tbl(mydb, 'iris') %>% 
#   select(cyl,mpg)
# 
# 
# iris[1,6]="Jel čuješ samo što hoćeš"
# iris4=iris[1:3,]
# copy_to(mydb, iris4)
# 
# 
# 
# dbSendQuery(mydb,"INSERT INTO iris4 (V6)
#     VALUES ('čuješ✅')")

loaded <- F # whether loaded from url permalink
makeReactiveBinding("loaded")
# 
# first <- T
# makeReactiveBinding("first")
# 
# 
# reactiveKeepRecovery <- reactiveVal(F)

if(!exists("loggedUser") %>% is.null)showModal(query_modal)
fileFromURL <- reactiveVal("")

# loggedUser <- reactiveVal()
loggedUser <- reactiveVal("free")

observeEvent(input$logon, {
  removeModal()
  
  loggedUser(input$input_user)
  
})

# logging on -----------
observe(({
  req(loggedUser())
  
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
    
    
    fileFromURL <- reactiveVal(ql %>%  get_title_from_query())
    userFromURL <- reactiveVal(ql %>%  get_user_from_query())
    # browser()
    
    if(req(userFromURL())!=req(loggedUser())) {
      doNotification("You do not have access",9)
      return()
    }
    if (!is.null(ql)) {
      if (!loaded) {
        doNotification("Loading files", 2)
        
        values$crowd <- str_detect(ql, "-crowd")
        if (values$crowd) updateCheckboxInput(session, "crowd", value = T)
        
        filename <- paste0("www/", ql)
        
        # browser()
        if (filename != "" & !file.exists(paste0(filename, "-nodes.csv"))) {
          createAlert(
            session,
            "notfound",
            title = "Sorry, couldn't find that link.",
            content = "Let me know if you need help: steve@pogol.net",
            append = FALSE
          )
        }
        
        
        for (fn in csvlist) {
          fnn <- paste0(filename, "-", fn, ".csv")
          
          if (file.exists(fnn)) {
            values[[fn]] <- read_csv((fnn))
          }
        }
        
        
        # need to add any new columns TODO
        
        nodes <- values$nodes %>%
          bind_rows(defaultNodes %>% filter(F)) %>%
          mutate(cluster = replace_na(cluster, "")) %>%
          mutate(clusterLabel = replace_na(clusterLabel, ""))
        
        edges <- values$edges %>%
          bind_rows(defaultEdges %>% filter(F)) %>%
          mutate(trust = as.numeric(trust)) %>%
          mutate(strength = as.numeric(strength)) %>%
          mutate(statement = as.numeric(statement)) %>% 
          mutate(quote=stringi::stri_trans_general(quote,"latin-ascii"))
        
        values$graf <- tbl_graph(nodes, edges)
        
        
        
        doNotification(glue("Loaded{nrow(values$graf %>% nodes_as_tibble)} variables from permalink"))
        # browser()
        updateTextInput(session, "titl", value = fileFromURL())#
      }
    }
    
    loaded <<- T
    # }
    
    
    qt <- query[["text"]]
  }
}))


# save button ----

output$savebut <- renderUI(
  
  div(
    div(
      tagList(
        
        # div(actionButton("render_button","Render"),style="display:inline-block;vertical-align:top"),
        div(if(!is.null(loggedUser()))tagList(span("Logged in as: " %>% paste0( loggedUser())) , a("| Log out",href=".")), style = "color:white;height:20px;font-size:14px;margin-bottom:0"),
        div(textInput(
          "titl", NULL,
          value = ifelse(is.null(fileFromURL()), "", fileFromURL()), placeholder = "Title", width = "100%"
        ), style = "display:inline-block;width:50%"),
        div(
          actionButton("saveb", "Save", icon = icon("save")),
          style = "display:inline-block;margin-left:5px;margin-right:5px;width:10%"
        )
      ),
      style = "margin-bottom:-20px"
    )
  ))

observeEvent(
  input$saveb,
  ignoreInit = TRUE, {
    
    
    
    if ("" != input$titl) {
      inputtitl <<- gsub("[^[[:alnum:]|-]]*", "", input$titl) %>% paste0(loggedUser(),"/",.)
      fileFromURL <- reactiveVal(inputtitl)
      
      
      
      
      values$nodes <- values$graf %>% nodes_as_tibble()
      values$edges <- values$graf %>% edges_as_tibble()
      
      for (c in csvlist) {
        write_csv(values[[c]], path = paste0("www/", inputtitl, "-", c, ".csv"))
      }
      
      
      
      upload_to_gdrive(gdriveRoot,fileFromURL(),values,input$titl)
      
      # file.copy(paste0("www/", inputtitl,".tm"),paste0("www/", inputtitl,".otm"),overwrite = T)
      doNotification("Saved")
      values$issaved <- T
      
    }
  }
  
)





observeEvent(input$saveb, ignoreInit = TRUE, {
  # browser()
  output$savedMsg <- renderUI({
    if (!values$issaved) {
      div()
    } else {
      div(
        id = "savemsg",
        "Saved to this permanent link: ",
        tags$a(
          paste0(fileFromURL()),
          href = paste0("?permalink=", fileFromURL())
        )
      )
    }
  })
  
  
  # toggleClass("savemsg", "red")
  # delay(500, toggleClass("savemsg", "red"))
})


