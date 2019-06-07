# options -----------------------------------------------------------------
options(shiny.port = 1111)
doNotificationLevel=1     #notification popups. level=1 is debugging and level=2 is user.
storage <- "dropbox"
storage <- "gsheets"
storage <- "local"



options(stringsAsFactors = F)

# source ------------------------------------------------------------------


source("combo_functions.r")

# libs --------------------------------------------------------------------




library(shinythemes)
library(shinyWidgets)
library(webshot)
library(readxl)
library(scales)
library(shinyjqui)
library(tidyr)
library(stringr)
library("shinyBS")
library(shinyjs)
library(glue)
library(r2d3)
library(tidyverse)
library(rhandsontable)
library(shinyPagerUI)
library(networkD3)
library(tidygraph)
library(googledrive)
library(googlesheets4)

library(rdrop2)

require(visNetwork)
require(plotly) # rgba
library(colourpicker)
library(ggraph)
library(DiagrammeR)
library(rpivotTable)  
library(shape)

if(storage=="local" | storage=="gsheets"){
  file__exists <- file.exists
  read__csv <- read_csv
  write__csv <- write_csv
} else if(storage=="dropbox"){
  file__exists <- drop_exists
  read__csv <- drop_read_csv
  write__csv <- function(obj,path){
    # browser()
    write_csv(obj,path = path)
    drop_upload(path,"www")      # note folder is hard-coded TODO
    file.remove(path)
    
  }
}



# functions ----------------------------------------------------------------


mutate_if_sw=function(...)suppressMessages(mutate_if(...))

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

widthScalr <- function(x) x / 10

doNotification <- function(text,level=1) {
  # set=findset("diagramnotification")
  # set=replace_na(set,"")
  # if(""==(set))set=F
  if(level>doNotificationLevel)showNotification(text)
  # x=readLines("log.txt")
  # if(exists("values")) {
  # x="log"#values$current
  write(paste0(text), paste0("","log.txt"), append = T)
  # }
}


addtran <- function(col, alpha) { # it is vectorised but assume both are same length
  sapply(1:length(col), function(i){
    col2rgb(col[i]) %>% paste0(collapse = ",") %>% paste0('rgba(', ., ',', alpha[i], ')')  # I couldn't work out any other way to make transparency work with edges with visnetwork
  }
  )
}  # maybe the standard way works if you use html colors not red etc


add_highlight=function(tex,background){
  
  if(!is.na(tex)[1]){
    loc=sapply(tex,function(x)str_locate(background,x)) %>% data.frame
    tagList(
      tags$span(background %>% str_sub(1,loc[y,1]-1)),
      lapply(1:ncol(loc),function(y){
        tags$b(background %>% str_sub(loc[1,y],loc[2,y]))
      }
      ),
      tags$span(background %>% str_sub(loc[2,ncol(loc)]+1,nchar(background)))
    )
  }
  else
    background
}

stripper <- function(vec) vec %>% str_remove_all("\\n|\\r") %>% str_replace_all("\\s+", " ") %>% str_replace_all("\\'", "")



allNum <- function(vec) {
  vec <- replaceNA(vec)
  s <- str_detect(vec, "^[-*\\.0-9]*$")
  all(s, na.rm = T) & !all(vec == "")
}

xc <- function(x, sep = " ") {
  str_split(x, sep)[[1]]
}

replaceNA <- function(df) {
  df[is.na(df)] <- ""
  df
}

inv=function(ed){   #inverts an edgelist
  ed %>% 
    select(from,to) %>% 
    unlist %>% 
    unique %>% 
    expand.grid(from=.,to=.,stringsAsFactors = F) %>% 
    bind_rows(ed) %>% 
    select(from,to) %>% 
    group_by(from,to) %>% 
    mutate(n=paste0(from,to)) %>% 
    group_by(from,to) %>% 
    mutate(n=n()) %>% 
    mutate(present=ifelse(n<2,0,1)) %>% 
    select(-n)
  
}


inv_multi=function(df){
  # browser()
  stats <- df %>%
    split(.$statement) %>%
    purrr::map_dfr(inv) %>% 
    group_by(from,to) %>% 
    mutate(absent=sum(1-present),present=sum(present),num=n(),avp=round(present/num,2),ava=round(absent/num,2)) %>% 
    summarise_all(dplyr::first)
  
  df %>% left_join(stats,by=c("from","to"))
  
}



infer=function(gr){                         # sets levels of downstream variables
  gr=gr %>% activate(nodes) %>% 
    mutate(source=node_is_source()) %>% 
    mutate(priorLevel=level) %>% 
    mutate(xid=row_number())
  
  
  
  ids=gr %>% 
    activate(nodes) %>% 
    pull(source) %>% 
    which
  
  
  
  empties=gr %>% 
    activate(nodes) %>% 
    mutate(empty=source & is.na(level)) %>% 
    pull(empty) %>% 
    which
  
  
  
  # ridiculous palaver cause can't call bfs directly  
  ranks=vector(length=0)
  
  for(y in ids){
    # browser()
    gr %>% 
      activate(nodes) %>% 
      mutate(rank=bfs_rank(root=y)) %>% 
      filter(!is.na(rank)) %>% 
      arrange(rank) %>% 
      pull(xid) -> rk
    ranks=c(ranks,rk[-1])
    
  }
  
  
  # gridd=list(length=0)
  
  combs=lapply(empties,function(x)0:1) %>% expand.grid()
  
  colnames(combs)=empties
  
  gr1=gr 
  
  levs=NULL#vector(length=length(combs))
  
  for(e in 1:nrow(combs)){
    if(length(empties)>0){
      # browser()
      row=combs[e,]
      gr1 %>% NN ->jj
      jj$level[as.numeric(names(combs))]=row
      gr1=gr1 %>% activate(nodes) %>% 
        mutate(level=unlist(jj$level))
      
    } 
    for(r in ranks){
      gr1=  gr1 %>% 
        activate(nodes) %>% 
        mutate(level = ifelse(r!=xid,level,
                              do.call(.N()$fun[r],list(.N()$level[.E()$from[.E()$to==r]]))
        )
        )
      
    }
    levs=cbind(levs,(gr1 %>% NN %>% pull(level)))
  }  # browser()
  
  
  
  if(length(empties)>0){
    means=rowMeans(levs,na.rm=T)
    gr = gr %>% activate(nodes) %>% 
      mutate(level=means)
  } else gr = gr1
  
  
  
  
  gr
}

# helpers for tidygraph
N_ <- function(gr) gr %>% activate(nodes)
E_ <- function(gr) gr %>% activate(edges)
NN <- function(gr) gr %>% activate(nodes) %>% as_tibble()
EE <- function(gr) gr %>% activate(edges) %>% as_tibble()






findfirst <- function(vec, vec2) {
  sapply(vec, function(x) {
    which(x == vec2) %>% first()
  })
}









# constants ----

writeLines("", "log.txt") # just to open up a fresh file


csvlist <- xc("nodes edges settings statements settingsGlobal")

blanks <- (c("", "", ""))

allcols1 <- lapply(xc("Greys Set1 Pastel1 Reds Oranges Blues Greens Purples"), function(x) RColorBrewer::brewer.pal(8, x)) %>%
  # c(RColorBrewer::brewer.pal(8,"Pastel1"),.) %>%
  # c(RColorBrewer::brewer.pal(8,"Set1"),.) %>%
  c(xc("#FFFFFF black")) %>%
  unlist()

allcols2 <- lapply(xc("Greys Reds Blues Greens Oranges Purples"), function(x) RColorBrewer::brewer.pal(9, x)[c(2, 4, 6, 8)]) %>%
  c(xc("#FFFFFF"), .) %>%
  unlist()

allcols <- allcols2

allcols <- c(allcols, paste0(allcols, "55"))

colnams <- lapply(xc("Grey Red Blue Green Orange Purple"), function(x) xc("light mid1 mid2 dark") %>% paste0(., " ", x)) %>%
  c(xc("White"), .) %>%
  unlist()

names(allcols) <- c(colnams, paste0(colnams, " hazy"))

shapelist <- c("box", "circle", "square", "triangle", "dot", "star", "ellipse", "database", "text", "diamond")

default.statements <- data.frame(
  "text" =
    rep("Some text", 1),
  "key1" =
    rep("", 1),
  "key2" =
    rep("", 1),
  "key3" =
    rep("", 1),
  "key4" =
    rep("", 1),
  "key5" =
    rep("", 1),
  "key6" =
    rep("", 1),
  "key7" =
    rep("", 1),
  "statement" =
    rep(1, 1),
  stringsAsFactors = FALSE
)



conditionlist <-
  c("always", "if...", "conditional on:")



comparisonlist <-
  c("equal", "not equal", ">", "<", "contains")

ft <- c(F, T)
op <- (0:10) / 10

valuelist <- list(
  "-" =
    "-",
  "direction" =
    c("Left-to-right" = "LR", "Right-to-left" = "RL", "Top-to-bottom" = "TB", "Bottom-to-top" = "BT"),
  "dir" =
    xc("forward back both"),
  "ranksep" =
    ((1:12) / 6)^2 %>% round(1),
  "nodesep" =
    xc("1 2 3 4"),
  "borderWidth" =
    1:9,
  "rank" =
    1:15,
  "penwidth" =
    1:15,
  "layout" =
    xc("dot neato twopi circo"),
  "fixedsize" =
    xc("true false"),
  "fixedsize" =
    xc("true false"),
  "shape" =
    shapelist,
  "hidden" =
    ft,
  "dashes" =
    ft,
  "opacity" =
    op,
  "color.opacity" =
    op,
  "group" =
    1:20,
  "bgcolor" =
    allcols,
  "background" =
    allcols,
  "color" =
    allcols,
  "border" =
    allcols,
  "color.background" =
    allcols,
  "color.border" =
    allcols,
  "color.background.highlight" =
    allcols,
  "color.border.highlight" =
    allcols,
  "background" =
    allcols,
  "color" =
    allcols,
  "font.color" =
    allcols,
  "fillcolor" =
    allcols,
  "font.size" =
    ((1:20) / 2)^2,
  "fontname" =
    xc("Helvetica Times Arial Courier"),
  "font.name" =
    xc("Helvetica Times Arial Courier"),
  "wrap" =
    (1:20) * 5,
  "combo.type" =
    xc("AND OR"),
  "style" =
    c("solid", "dotted", "dashed", "invisible" = "invis"),
  "height" =
    (1:9) / 10,
  "size" =
    (1:6)^2,
  "width" =
    1:9,
  "font.size" =
    1:9,
  "label" =
    "Placeholder for legend",
  "labelloc" =
    xc("b t"),
  "labeljust" =
    xc("l r c"),
  "nodeSpacing" =
    ((1:20) / 4)^2,
  "levelSeparation" = ((1:20) / 4)^2
)


arrcollist <- c(
  "quote",
  "label",
  "strength",
  "trust",
  "dir",
  #  "color",
  #  "style",
  "statement"
)



defaultSettings <- read_csv("defaultSettings.csv")
defaultSettingsGlobal <- read_csv("defaultSettingsGlobal.csv")




# UI ----------------------------------------------------------------------




ui <- tagList(
  useShinyjs(),
  inlineCSS(list(.red = "background: snow")),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/styles2.css")
  ),
  tags$script('
              $(document).on("keydown", function (e) {
              Shiny.onInputChange("keypressed", e.which);
              });
              '),
  

# get selected text for Code tab ------------------------------------------


  tags$script('
                function getSelectionText() {
                    var text = "";
                    if (window.getSelection) {
                        text = window.getSelection().toString();
                    } else if (document.selection) {
                        text = document.selection.createRange().text;
                    }
                    return text;
                }

        document.onmouseup = document.onkeyup = document.onselectionchange = function() {
            var selection = getSelectionText();
            Shiny.onInputChange("highlightedText", selection);
        };
        '),
  

# fluid page --------------------------------------------------------------



  fluidPage(
    theme = shinytheme("flatly"), style = "background-color:#445544 !important;",
    
    
    div(
      id = "loading-content",
      h2("Getting ready for causal mapping...")
    ),
    conditionalPanel("!input.crowd",
                     
                     div(
                       div(
                         
                         
                         sidebarLayout(
                           jqui_resizable(sidebarPanel(
                             id="resizablePanel",
                             width = 4, style =
                               "padding:0",
                             
                             id = "app-content",
                             a(h4("Causal Mapping ", style = "display:inline-block;color:white;margin-right:8px"), href = "."),
                             img(src = "img/logo.gif", height = "20px", style = "display:inline-block;"),
                             a(icon("question-circle"),href="http://www.pogol.net/_help/index.html", target="_blank",height = "20px", style = "display:inline-block;margin-left:20px"),
                             
                             hr(style = "margin-top:5px"),
                             
                             # uiOutput("keyp")
                             # ,
                             
                             
                             bsAlert("bsmsg"),
                             bsAlert("found"),
                             bsAlert("notfound"),
                             
                             
                             uiOutput("test"),
                             
                             # selectInput(inputId = "selnodes", label = "Nodes selection", choices = 1:15, multiple = TRUE)
                             # ui sidepanels----
                             conditionalPanel(
                               condition = "1==1",
                               style = "padding:0",
                               # tabsetPanel(type = "tabs",selected="statements",
                               
                               uiOutput("savebut"),
                               hr(),
                               tabsetPanel(
                                 id = "sides", type = "tabs", selected = "Code",
                                 
                                 # ui statements----
                                 
                                 tabPanel("",icon=icon("upload"),
                                          style = glue(";border-radius:10px"),
                                          # h3("uploads"),
                                          
                                          bsCollapse(
                                            id = "uploads", open = "Upload statements",
                                            bsCollapsePanel(
                                              title = "Upload statements",
                                              # uiOutput("statementsButton"),
                                              fileInput("up.statements", NULL,
                                                        multiple = FALSE, width = NULL,
                                                        accept = c(
                                                          "text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv"
                                                        )
                                              )
                                            ),
                                            bsCollapsePanel(
                                              title = "View and edit statements",
                                              p("Paste your statements into the space below. The table will expand if you paste more rows."),
                                              # p("If you want more columns, right-click to create them first.")
                                              # ,
                                              p("The first column is the text. You can use other columns for attributes like age or gender."),
                                              # p("You can change the column names."),
                                              actionButton("statementsTableUp", "Update"),
                                              rHandsontableOutput("statements")
                                            ),
                                            bsCollapsePanel(
                                              "Upload variables and arrows",
                                              fileInput("up.nodes", "Upload variables",
                                                        multiple = FALSE, width = NULL,
                                                        accept = c(
                                                          "text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv"
                                                        )
                                              ),
                                              fileInput("up.edges", "Upload arrows",
                                                        multiple = FALSE, width = NULL,
                                                        accept = c(
                                                          "text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv"
                                                        )
                                              ),
                                              checkboxInput("use.labels", "Use labels instead of row numbers")
                                            )
                                          )
                                 ),
                                 
                                 # ui coding----
                                 tabPanel("",icon=icon("highlighter"),
                                          value = "Code", style = glue("background-color:{rgb(0.99,1,0.97)};;border-radius:10px"), 
                                          
                                          # checkboxInput("showPage","Code one sentence")
                                          # ,
                                          uiOutput("pagerBig"),
                                          uiOutput("pagePanel"),
                                          
                                          
                                            uiOutput("varForm"),
                                          
                                          bsCollapse(
                                            id = "codeCollapse", open =
                                              "Add arrows",
                                            
                                            bsCollapsePanel(
                                              "Add arrows",
                                              div(
                                                uiOutput("selectbox"),
                                                uiOutput("addNewNodeButton"),
                                                uiOutput("add_edges_widget"),
                                                uiOutput("combo")
                                              )
                                            ),
                                            
                                            
                                            bsCollapsePanel(
                                              "Advanced options",
                                              p("Development only"),
                                              icon("exclamation-triangle"),
                                              p(id = "advancedAnchor")
                                            )
                                          )
                                 ),
                                 tabPanel(value="Variables","",
                                          style = glue("background-color:{rgb(0.99,1,0.97)};;border-radius:10px"), icon = icon("boxes"),
                                          # h3("View and edit variables"),
                                          div(actionButton("nodeTableUp", "Update"),style="display:inline-block"),
                                          div(textInput("nodeTableFilter", NULL,width = "160px",placeholder="Filter..."),style="display:inline-block;margin-left:20px"),
                                          div(checkboxInput("nodeTableAddCol", "Add columns"),style="display:inline-block;margin-left:20px"),
                                          div(checkboxInput("nodeTableColE", "Edit columns"),style="display:inline-block;margin-left:20px"),
                                          conditionalPanel("input.nodeTableAddCol",
                                                           uiOutput("nodeTableAddCol")),
                                          rHandsontableOutput("nodeTable"),
                                          uiOutput("combine_button")
                                   # ,
                                   #        uiOutput("formMakr")
                                 ),
                                 tabPanel("",
                                          value = "Arrows", style = glue("background-color:{rgb(0.99,1,0.97)};;border-radius:10px"), icon = icon("arrow-right"),
                                          # h3("View and edit arrows"),
                                          actionButton("edgeTableUp", "Update"),
                                          rHandsontableOutput("edgeTable"),
                                          div(
                                            style = "display:inline-block;padding-left:20px;font-size:4px;width:150px",
                                            checkboxInput("addColBut", "Edit columns")
                                          ),
                                          uiOutput("edgeInfo")
                                 ),
                                 
                                 
                                 # ui settings----
                                 
                                 tabPanel("",value="Display",
                                          style = glue("background-color:{rgb(0.97,1,0.97)};;border-radius:10px"), icon = icon("palette"),
                                          
                                          # selectInput("layout","layout",choices=c("Sugiyama"="layout_with_sugiyama", "circle"="layout_in_circle"),selected = "layout_with_sugiyama")
                                          # ,
                                          uiOutput("filters"),
                                          uiOutput("inputs"),
                                          bsCollapse(
                                            id = "display", open =
                                              "Advanced",
                                            bsCollapsePanel(
                                              "Advanced",
                                              actionButton("bigTableGUp","Update"),
                                              rHandsontableOutput("bigTableG"),
                                              hr(),
                                              actionButton("bigTableUp","Update"),
                                              rHandsontableOutput("bigTable"),
                                              hr()
                                              # htmlOutput("overview"),
                                              # hr()
                                            )
                                          ),
                                          bsCollapsePanel(
                                            "Easy",
                                            uiOutput("setwig"),
                                            actionButton("autoMerge", "Auto-suggest clusters")
                                          )
                                   # ,
                                          # bsCollapsePanel(
                                          #   "Final / merged variables",
                                          #   rHandsontableOutput("node2")
                                          # ),
                                          # 
                                          # bsCollapsePanel(
                                          #   "Final / merged arrows",
                                          #   rHandsontableOutput("edge2")
                                          # )
                                 ),
                                 tabPanel("",icon=icon("chart-pie"),
                                   style = "padding:20px;background-color:white;z-index:99",
                                   # d3Output("d3")
                                   
                                   bsCollapse(
                                     bsCollapsePanel(title = "Sankey",
                                       plotlyOutput("charts")
                                     ),
                                     bsCollapsePanel(title = "pivot",
                                       rpivotTableOutput("pivot", width = "100%", height = "300px")
                                     )
                                   )
                                   
                                 ),
                                 
                                 tabPanel("", icon=icon("stack-overflow"),div(style = ""),
                                   style = "",
                                   uiOutput("gallery")
                                 ),
                                 tabPanel("", icon=icon("download"),div(style = ""),
                                   style = "",
                                   uiOutput("downloads")
                                 )
                               )
                             )
                           ), options = list(minHeight = 100, maxHeight = 2000,
                                             minWidth = 200, maxWidth = 3000)),
                           
                           # main panel----
                           mainPanel(
                             
                             width = 8, style = "border-left:2px dotted black",
                             # actionButton("aggbut","agg"),
                             conditionalPanel("!input.crowd",style="background-color:white;border-radius:5px",
                               
                               uiOutput("showbigpicture"),
                               
                               
                                              # tabsetPanel(
                                              #   type = "tabs", selected =
                                              #     "Diagram",
                                              #   
                                              #   tabPanel("Diagram", div(style = ""),
                                              #            style = "padding:20px;",
                                              # 
                                              #            
                                              #            # uiOutput("titlvalid"),
                                                         
                                                         conditionalPanel("1==1",visNetworkOutput("net", height = "950px", width = "100%")),
                                                        # conditionalPanel("input.diagType!='Interactive'",grVizOutput("dot")),                                                         
                                                         uiOutput("versions"),
                                                         
                                                         
                                                         # textInput("urll","lab"),
                                                         # uiOutput("propslider"),
                                                         tags$hr(),
                                                         # textOutput("itmess"),
                                                         uiOutput("blog"), 
                                                         textOutput("info")
                                              #   )
                                              # )
                             )
                             ,
                             # div(actionButton("Interrupt", "Interrupt",
                             #                  icon = icon("exclamation-triangle"),
                             #                  style = "background-color:pink;"
                             # ), style = "display:inline-block;margin-left:10px"),
                             uiOutput("description"),
                             plotOutput("colourLegend")
                           )
                         )
                       )
                     ),
                     div(actionButton("Interrupt", "Interrupt",
                     ), style = "position:fixed;bottom:0;right:10px")
    ),
    conditionalPanel("input.crowd",
                     uiOutput("add_edges_widget2"),
                     visNetworkOutput("net2", height = "75vh", width = "auto")
                     
                     # p("lkj")
    ),
    div(checkboxInput("crowd","Simplified view for crowdsourcing?"),style="color:#EEFFEE;position:fixed;bottom:0")
  )
)


#server----

server <- function(input, output, session) {
  autoInvalidate <- reactiveTimer(2000)
  
  values <- reactiveValues()
  # values$isBig <- F
  values$pag <- 1
  values$statements <- default.statements
  values$clickArrow <- F
  values$crowd = F
  # values$graph <- NULL
  
  values$settings <- defaultSettings #%>% mutate_all(as.character)
  values$settingsGlobal <- defaultSettingsGlobal# %>% mutate_all(as.character)
  
  values$highlightedText <- ""
  
  observeEvent(input$highlightedText,{
    if (!is.null(input$highlightedText)) if ("" != (input$highlightedText)) values$highlightedText <- paste0(values$highlightedText," ... ",input$highlightedText)
  })
  
  loaded <- F
  makeReactiveBinding("loaded")
  
  first <- T
  makeReactiveBinding("first")
  
  values$fromStack=NULL
  values$toStack=NULL
  values$issaved=F
  values$foundIDs=NULL
  
  
  inputtitl <- ""
  makeReactiveBinding("inputtitl")
  
  ts <- reactiveValues(counter = 0)
  
  
  

# findset function to transfer user settings to values$settings -----------

  
  findset <- function(tex, which = "current",global=T,v=values) {
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
  
  
  
  observeEvent(input$Interrupt, {
    browser()
  })
  
  
  observeEvent(input$statementsTableUp, {
    
    # vs=values$statements
    # # browser()
    # if (nrow(vs) == 1 & vs$text[1] == "") {
    #   vs$text[1] <- "Some statement made by one of the sources ..."
    #   vs$statement[1] <- "1"
    # }
    
    values$statements <-  hot_to_r(input$statements) %>%
      mutate(text = str_replace(text, "\'", "")) %>% 
      select(1:(ncol(default.statements))) %>%
      mutate(statement = row_number())
    # filter(text != "") %>%
    bind_rows(default.statements[1, ])
    
  })
  
  
  
  # update from permalink or example url; also set buttons and messages -----------
  
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
              createAlert(session, "notfound", title = "Sorry, couldn't find that link.", content = "Let me know if you need help: steve@pogol.net", append = FALSE)
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
            
            
            
            
            doNotification(glue("Loaded{nrow(values$graf %>% NN)} variables from permalink"))
            
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
    # id =
    #   1:2,
    # color.background=c("","")
    # ,
    # color.border=c("","")
    # ,
    # color.highlight.border = c("","")
    # ,
    # color.highlight.background = c("","")
    # ,
    label = xc("one two"),
    details = xc("one two")
    # ,
    # font.size=1:20
    ,
    group =
      c("", ""),
    col1 =
      c("", ""),
    cluster =
      c("", ""),
    # frequency="1"
    # ,
    value =
      0,
    level =
      0,
    fun = 
      "sumfun",
    type =
      ("◨"),
    is_context = 
      c(F,F),
    clusterLabel =
      c("", ""),
    stringsAsFactors = F
    # ,
    # borderWidth=1:20
    # ,
    # shape=rep(20,"box",20)
  )
  
  defaultEdges <- data.frame(
    # id =
    #   "1",
    from =
      1,
    to =
      2,
    label =
      "",
    strength =
      .5,
    trust =
      .5,
    sensitivity =
      .5,
    specificity =
      .5,
    statement =
      1,
    quote =
      "",
    full.quote =
      "",
    combo.type =
      "",
    definition.type =
      "",
    # title=""
    # ,
    # frequency="1"
    # ,
    
    # width=""
    # ,
    # color=""
    # ,
    # color.opacity=""
    # ,
    # blah=.5
    stringsAsFactors = F
  )
  
  # if(is.null(values$graf))  
  values$graf <- tbl_graph(defaultNodes%>% filter(F), defaultEdges %>% filter(F))
  
  
  # observe({
  #   # if (is.null(values$nodes)) values$nodes <- defaultNodes
  #   if (is.null(values$graf) & is.null(values$current)) {
  #     doNotification("Creating default graph")
  #     # if (""==(values$current))
  #     nodes <- defaultNodes
  #     edges <- defaultEdges
  # 
  # 
  #     # browser()
  #     # # values$graf=tbl_graph(nodes,edges)
  #     # values$graf=NULL
  #     # values$graf=NULL
  #     values$graf <- tbl_graph(nodes, edges)
  #     doNotification("Created default graph")
  #   }
  # })
  
  
  

# create values$tot -------------------------------------------------------

  
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
        sliderInput("timeslider", NULL, min = 0, max = values$tot, value = 0, animate = animationOptions(loop = T), ticks = F, width = "50px"),
        style = "display:inline-block;"
      ),
      div(if("source" %in% colnames(values$statements))actionButton("overview_col", label = "Whole source"),
          style = "display:inline-block;margin-left:20px"
      )
    )
  })
  
  
  values$obsList <- list()
  
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
            } else
            {
              actionLink(btName,label = vs$text[x])
            }
            ,
            br()
          )
        }
        )
        
        
      )
    ))
  })
  
  
  
  
  
  # 
  # observe({
  #   if (!is.null(values$statements)) {
  #     # tot=nrow(values$statements)
  #     tot <- values$statements %>%
  #       filter(text != "") %>%
  #       pull(statement) %>%
  #       na.omit() %>%
  #       as.numeric() %>%
  #       max()
  #     values$tot <- tot
  #     # browser()
  #   } # if(!is.na(tot)) if(tot>1) browser()
  #   else {
  #     tot <- 9
  #   }
  #   updatePageruiInput(session, "pager", pages_total = as.numeric(tot))
  # })
  
  # ** create page panel ----
  
  # observeEvent(req(input$pager),{
  #   if (!is.null(input$pager)) {
  #     pag <- input$pager[[1]]
      # vs <- values$statements %>% filter(text != "")
      
      
        # browser()
        # ve=values$graf %>% EE

        
      output$pagePanel <- renderUI({
          
        tagList(
          icon("quote-left"),
          span(values$statements$text[values$statements$statement==values$pag], class = "textbanner", id = "textbanner"),
          # span(add_highlight((ve$quote[ve$statement==values$pag])[1],(values$statements$text[values$statements$statement==values$pag])), class = "textbanner", id = "textbanner"),
          hr()
        )
        
        })
        
        
  #   }
  # })

  
  
  
  observeEvent(
    {
      c(input$pager,input$timeslider)
    },{
      if (!is.null(input$pager)) {
        values$pag <- input$pager[[1]]
        
        if (input$timeslider > 0 & nrow(values$statements) > 2) updatePageruiInput(session, "pager", page_current = input$timeslider)
        if(!is.null(input$pager) && !is.null(input$quote))updateTextAreaInput(session = session,inputId = "quote",value="",placeholder="quote")
      }
    })
  
  
  # observeEvent(input$pager,{
  #   # browser()
  # })
  
  # Import statements----
  
  output$statements <- renderRHandsontable({
    
    # if(is.null(values$statements))values$statements=default.statements
    # browser()
    rhandsontable(values$statements[,], height = 700, rowHeaders = FALSE, usetypes = T) %>%
      hot_context_menu(allowRowEdit = T, allowColEdit = T) %>%
      hot_cols(colWidths = c(400, rep(70, ncol(values$statements) - 1))) %>%
      hot_cols(fixedColumnsLeft = 1)
  })
  
  
  # # **create edit columns statements ----
  # 
  # output$statementsButton <- renderUI({
  #   tab <- (values$statements)
  #   
  #   tagList(
  #     checkboxInput("editColumns", "Edit column names"),
  #     # if(!is.null(input$editColumns))
  #     conditionalPanel(
  #       "input.editColumns",
  #       
  #       # for(x in 1:7){
  #       lapply(2:(ncol(tab)), function(x) {
  #         textInput(paste0("statementscol", x), NULL, value = colnames(tab)[x])
  #       })
  #     )
  #   )
  # })
  # 
  # 
  # # ** observe form for changing statements colnames  ----
  # observe({
  #   lapply(1:ncol(values$statements), function(x) {
  #     # browser()
  #     inx <- input[[paste0("statementscol", x)]]
  #     if (!is.null(inx)) colnames(values$statements)[x] <- inx
  #     if (!is.null(inx)) colnames(default.statements)[x] <- inx
  #   })
  # })
  # 
  # 
  
  
  
  
  # Code----
  
  
  
  
  # automerge----
  
  observeEvent(input$autoMerge, { # notice this jumpt so edgesAGG, is that good?
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
    
    
    # values$nodes=values$nodes %>%
    #   mutate(cluster=ifelse(as.numeric(id)<3,"1",""))
  })
  
  
  
  observeEvent(input$firstuncoded, {
    # browser()
    slist <- values$statements %>%
      filter(text != "") %>%
      pull(statement) %>%
      na.omit() %>%
      as.numeric() %>%
      sort()
    
    elist <- EE(values$graf)$statement %>% na.omit() %>% as.numeric()
    
    min <- slist[sapply(slist, function(y) !(y %in% elist))] %>% min()
    
    if (is.infinite(min)) {
      doNotification("You have coded everything!",level=2)
    } else {
      updatePageruiInput(session, "pager", page_current = min)
    }
  })
  
  
  
  
  
  
  
  # Display----
  
  # ....bigtable----
  
  
  output$filters=renderUI({
    lapply(colnames(values$statements %>% select(-text)),function(y){
      x=values$statements[[y]]
      u=unique(x) %>% na.omit()
      if(length(u)>1 & length(u)<12){
        div(checkboxGroupButtons(paste0("filters",y),y,choices=sort(u),selected=u),style="display:inline-block")
      }
    })
  })  
  
  
  observeEvent(input$bigTableGUp,{
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
  observe(if(F){
    vs <- values$settingsGlobal %>% mutate_all(as.character)
    lapply(1:nrow(vs),function(x){
      row=vs[x,]
      rg=replace_na(row$widget,"")
      rt=paste0(row$type,row$setting,collapse="")
      rt=replace_na(rt,"")
      # if(rg=="color")  {checkboxInput("asdf","asdf")}
      inp=input[[paste0("input",rt)]]
      if(rg!="" & !is.null(inp))  {
        values$settingsGlobal$value[x]=inp
        doNotification(paste0("updated settings from widget: ",inp))
      }
      # else if(rg=="slider")  {
      #   div(sliderInput(paste0('input', rt),rt,min = 0,max=1000,value = 50
      #   ) ,
      #   style="display:inline-block;margin:0;padding:0;height=8px")
      # }
      # else 
      #   if(rg=="input") paste0(row$type,row$setting,collapse="")
    })
  })
  
  output$bigTableG <- renderRHandsontable({
    vs <- values$settingsGlobal %>% mutate_all(as.character)
    
    ds <- defaultSettingsGlobal %>% mutate_all(as.character)
    
    vs <- bind_rows(vs, ds) %>%
      distinct(type, setting, .keep_all = T)
    
    
    rhandsontable(vs %>%
                    mutate(type = factor(type)), height = NULL, rowHeaders = FALSE, usetypes = T) %>%
      hot_context_menu(allowRowEdit = T) %>%
      hot_cols(colWidths = c(80,120,250,80))
  })
  
  output$bigTable <- renderRHandsontable({
    vs <- values$settings %>% mutate_all(as.character)
    
    ds <- defaultSettings %>% mutate_all(as.character)
    
    vs <- bind_rows(vs, ds) %>%
      distinct(type, setting, .keep_all = T)
    
    
    rhandsontable(vs %>%
                    mutate(type = factor(type), condition = factor(condition, levels = c("always", "if...", "conditional on:"))), height = NULL, rowHeaders = FALSE, usetypes = T) %>%
      hot_context_menu(allowRowEdit = T) %>%
      hot_cols(colWidths = c(80,120,250,80))
    
  })
  
  observeEvent(input$bigTableUp, {
    doNotification("updating from bigtable")
    values$settings <- hot_to_r(input$bigTable)
  })
  
  observeEvent(input$bigTableGUp, {
    doNotification("updating from bigtableG")
    values$settingsGlobal <- hot_to_r(input$bigTableG)
  })
  
  
  
  
  
  
  observeEvent(input$fitaction, {
    visNetworkProxy("net") %>%
      visFit() 
  })
  
  
  
  
  mutfun <- function(vec) {
    if (allNum(vec)) {
      sum(as.numeric(vec), na.rm = T)
    } else {
      first(vec)
    }
  }
  
  catfun <- function(x) {
    x <- na.omit(x)
    x <- x[x != ""]
    paste0(x, collapse = ",")
  }
  
  andfun <- function(x) {
    x <- na.omit(x)
    x <- x[x != ""]
    all(as.logical(x)) %>% as.numeric
  }
  
  orfun <- function(x) {
    x <- na.omit(x)
    x <- x[x != ""]
    any(as.logical(x)) %>% as.numeric
  }
  
  sumfun <- function(x) {
    # x[is.na(x)]=0
    # x <- sum(as.numeric(x), na.rm = T) %>% round(2)
    # x
    # x[1]
    # 
    sum(x,na.rm=T) 
  }
  
  meanfun <- function(x) {
    # x[is.na(x)]=0
    # x <- mean(as.numeric(x), na.rm = T) %>% round(2)
    # x
    # x[1]
    mean(x,na.rm=T) 
  }
  
  # catfun=function(x)paste0(x,collapse=",")
  
  # group_if=function(df,cond,vars1,vars2){
  # dots = sapply(if(cond)vars1 else vars2, . %>% {as.formula(paste0('~', .))})
  # group_by_(df,.dots=dots)
  # }
  #
  #
  
  # AGGREGATE ---------------------------------------------------------------------------
  
  observe(
    if ((req(values$graf)) %>% EE %>% nrow %>% `>`(0)) {
  
      

# prepare statements, split columns ------------------------------------------------------

   

      
          
      # if(input$crowd)browser
  doNotification("starting aggregation")    
      legend <- ""
      
      # post-process original version
      
      vs <- (values$settings) %>% replaceNA()
      
      vg=(values$graf) %>% 
        activate(nodes) %>% 
        mutate(id = row_number(), origID = id) %>% 
        mutate(value=as.numeric(value)) %>% 
        mutate(value=if_else(value=="",0,value)) %>% 
        mutate(value=replace_na(value,0)) 
      
      # browser()
      
      # infer ----
      
      if(findset("variableinfer") %>% as.logical()){
        vg=infer(vg)
        legend <- paste0(legend, "</br>Causal inference carried out")
      }
      
      vno <- vg %>% NN() 
      
      ved <- vg %>% EE()
      
      
      # prepare ved
      
      # browser()
      ved <- ved %>%
        mutate_at(vars(strength, trust), funs(as.numeric)) %>%
        mutate(combo.type = ifelse(is.na(combo.type), "", combo.type)) %>%
        mutate(label = ifelse(is.na(label), "", label)) %>%
        mutate(definition.type = ifelse(is.na(definition.type), "", definition.type))
      
      
      
      
      # ved join statements--------------------------------
      
      ved <- ved %>%
        left_join(values$statements, by = "statement")
      
      
      # cat("refreshing")
      if(req(input$sides)!="Code"){
        
        doNotification("cluster aggregation")    
        # merge nodes by cluster -- note we don't merge arrows first ----
        
        if (findset("variablemerge") %>% as.logical() & input$sides!="Code") { # need to convert to and froms in edge df
          # browser()
          vno <- vno %>%
            mutate(id = row_number()) %>%
            mutate(cluster = ifelse(cluster == "", NA, cluster)) %>%
            group_by(cluster) %>%
            mutate(clusterid = first(id)) %>%
            mutate(clusterid = ifelse(is.na(cluster), id, clusterid)) %>%
            ungroup() %>%
            group_by(clusterid) %>%
            mutate(clusterLength = n()) %>%
            mutate(clusterLabel = clusterLabel %>% replaceNA()) %>%
            mutate(label = ifelse(clusterLength < 2, label, ifelse(clusterLabel != "", clusterLabel, paste0("Cluster: ", label)))) %>%
            # mutate(value=mean(value,na.rm=T)) %>%    
            # mutate(valueSum=sum(value,na.rm=T)) %>%    
            mutate_if_sw(is.numeric, .funs = list(sum=sumfun,mean=meanfun)) %>%
            ungroup()
          
          ved <- ved %>%
            mutate(from = vno$clusterid[findfirst(from, vno$id)]) %>%
            mutate(to = vno$clusterid[findfirst(to, vno$id)])
          # browser()
          tmp.graf <- tbl_graph(vno, ved) %>%
            N_() %>%
            filter(id == clusterid)
          
          vno <- tmp.graf %>% NN()
          ved <- tmp.graf %>% EE()
          
          values$tmp.graf <- tmp.graf   #for permanent cluster reduction button
          
          legend <- paste0(legend, "</br>Variables merged according to user-defined clusters")
          
        }
        
        
        
        
        
        # filter out some sources ----
        
        doNotification("filter aggregation")    
        
        for(i in names(input))if(str_detect(i,"filters")){
          ii=str_remove(i,"filters")
          fil=input[[i]]
          # browser()
          if(length(fil)>0){
            ved <- ved %>% 
              filter(UQ(sym(ii)) %in% fil)
            # legend <- paste0(legend, "</br>Edges filtered to show specific sources only")
          }
          tmp=tbl_graph(vno,ved) %>% 
            N_() %>% 
            filter(row_number() %in% unique(c(ved$from,ved$to)))
          
          vno <- tmp %>% NN()
          ved <- tmp %>% EE()
          
          # inefficient TODO we could collect them all and filter only once
          
        }
        
        
        
        # rick --------------------------------------------------------------------
        
        
        if(("from" %in% colnames(ved))  &&  as.logical(findset("arrowabsence")) && input$sides!="Code"){ #todo findset
          
        doNotification("rick aggregation")    
          
          if(all(is.na(ved$statement)))ved$statement=1
          # browser()
          ved <- ved %>%
            inv_multi()
        }
        }
        
        
        # browser()
        
        # ved edge merge ----
        
        if (findset("arrowmerge") %>% as.logical() && input$sides!="Code") {
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
        
        if(input$sides!="Code"){
        ved <- ved %>%
          filter(frequency > findset("arrowminimum.frequency"))
        }
        # browser()
        # cat("merge")
        
        # join edges with nodes------------------------------------------------
        
        doNotification("join to edges aggregation")    
        
        if(findset("variablejoinedges") %>% as.logical | 
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
          mf <- findset("variableminimum.frequency") %>% as.numeric()
          # browser()
          if (input$sides!="Code" && mf > 0 ) {
            tmp <- tbl_graph(vno, ved) %>%
              N_() %>%
              filter(frequency > mf)
            
            vno <- tmp %>% NN()
            ved <- tmp %>% EE()
            # #
            #
            # vno=vno %>%
            #   filter(frequency>mf)
            #
            #
            # ved=ved %>%
            #   filter(from %in% vno$id & to %in% vno$id)
            #
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
                df[!theseones, row$setting] <- findset(paste0(typenow, row$setting), which = "default",global = F)
                
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
                    df[, row$setting] <- colorRampPalette((str_split(findset(paste0("diagrampalette",palettes)),","))[[1]] %>% str_trim)(max(as.numeric(mr)))[mr]
                    palettes=palettes+1
                  }
                  if (row$setting %in% xc("font.size")) {
                    df[, row$setting] <- (mr + findset("variablefont.size.floor",global = T) %>% as.numeric) * findset("variablefont.size.scale",global = T) %>% as.numeric / nr
                  }
                  if (row$setting %in% xc("size")) {
                    df[, row$setting] <- mr / nr
                  }
                  if (any(row$setting %in% xc("width"))) {
                    df[, row$setting] <- mr * findset("arrowconditionalwidthscaling") %>% as.numeric() / max(mr)
                  }
                  if (any(row$setting %in% xc(" borderWidth"))) {
                    df[, row$setting] <- mr * findset("variableconditionalwidthscaling") %>% as.numeric() / max(mr)
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
          mutate(title=labelmaker(findset("variabletooltip"),vno),
                 label=labelmaker(findset("variablelabel"),vno,sep=" | ")
          )
        
        
        ved <- ved %>% 
          mutate(title=labelmaker(findset("arrowtooltip"),ved),
                 label=labelmaker(findset("arrowlabel"),ved,sep="\n")
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
        
        if (findset("variableautogroup")) {
          tmp <- tmp %>%
            N_() %>%
            mutate(group = group_walktrap())
        }
        
        
        
        values$grafAgg2 <- tmp # %>% debounce(4000)
        
       
      
      
      # browser()
      
      doNotification("Aggregated")
    })
  
  # values$grafAgg2 <-values$grafAgg2 %>% debounce(4000)
  
  
  # RENDER visnetwork----
  # observeEvent(c(input$render_button, input$updateE3,
  #                input$updateE2,
  #                input$updateE,
  #                input$updateE_crowd,
  #                input$timeslider,
  #                input$pager
  # ),{

  observe({
    
    vga <- (req(values$grafAgg2)) #%>% shiny::debounce(4000)
    
    if (T) {
      
      if((input$crowd)){
        values$legend=""
      }
      
      doNotification("started viz")
      # browser()
      
      if(is.null(values$pag))values$pag=1
      if (T) {
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
        
        # visSave(vn1,"vn1.html",selfcontained = F,background = "white")
        
        vn= vn1 %>%
          visInteraction(dragNodes = T, dragView = T, zoomView = T, navigationButtons = F, multiselect = T) %>%
          visInteraction(tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;
                -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                 border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                 max-width:500px;word-break: break-all')%>%
          visOptions(manipulation = F, collapse = TRUE, highlightNearest = list(enabled=T,degree=99,hover=T,algorithm="hierarchical"), nodesIdSelection = F) %>%
          visConfigure(enabled = input$codeCollapse == "Advanced options", container = "advancedAnchor") %>%
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
        
        
        # browser()
        
        if (findset("diagramlayout") == "layout_with_sugiyama") {
          vn <- vn %>%
            visIgraphLayout(layout = "layout_with_sugiyama", randomSeed = 123, smooth = T, type = "full")
          
          if (T) {
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
            
            # browser()
            # vn$x$nodes$x <- vn$x$nodes$x * 1
            # vn$x$nodes$y <- vn$x$nodes$y * 2.5
          }
        } else {
          vn <- vn %>%
            visIgraphLayout(layout = "layout_in_circle", randomSeed = 123, smooth = T, type = "full", physics = T)
        }
        
        
        
        if (findset("diagramphysics") %>% as.logical()) {
          vn <- vn %>%
            visPhysics(barnesHut = list(avoidOverlap = .7))
        }
    #    browser()
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
        
        x <- function(fghj){vn}
        
        values$net <- x#throttle(x,6000)
        
        doNotification("Produced viz")
        # browser()
        
      }
      # else if (!is.null(values$nodesAgg)) doNotification(paste0("You have arrows which mention non-existent variables"))
      
      
      
    }
    
    
    
    })
  
  
  output$net <- renderVisNetwork({
    doNotification("render viz")
    # browser()
    # doNotification("rendered viz")
    if("net" %in% names(values)){
    if(is.function(values$net))values$net() 
      }
    
  })
  
  

# focus -------------------------------------------------------------------

  
  
  
  observe(if(req(input$sides)=="Code"){
    # vno <- req(values$grafAgg2) %>% NN
    # if (as.logical(findset("diagramfocus")) & !is.null(values$pag) & nrow(vno)>0) {
    #   ids <- vno %>%
    #     mutate(sel=ifelse(str_detect(paste0(",|^", statement, ",|$"), as.character(values$pag)),T,F)) %>%
    #     pull(sel) %>%
    #     which
    # 
    # visNetworkProxy("net") %>%
    #   visSelectNodes(id = ids)
    # }

    
  })   # took this out because
  
  # observe(if(req(input$nodeTable_select$select$c)==1){
  #   visNetworkProxy("net") %>%
  #     visSelectNodes(id = input$nodeTable_select$select$r)
  #   #
  # 
  # })
  
  
  
  # add edges observer ----
  
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
  
  
  # **node table----
  
  output$combine_button=renderUI({
    if(!all(replace_na((NN(values$graf))$cluster,"")=="")){
      div(actionButton("node_permanent_combine", "Combine clusters permanently!?"),style="margin-top:5px;")
    }
  })
  
  
  observeEvent(input$node_permanent_combine,{
    values$graf <- values$tmp.graf
  })
  
  output$nodeTable <- renderRHandsontable({
    # browser()
    arrows=values$graf %>% EE %>% select(xid=from,to) %>% unlist %>% unclass() %>% as.tibble
    
    vg=values$graf %>% NN %>% 
      # mutate_all(replaceNA) %>% 
      mutate(xid=row_number()) %>% 
      left_join(arrows %>%  select(xid=value),by="xid") %>% 
      group_by(xid) %>% 
      mutate(frequency=n()) %>% 
      summarise_all(last)
    
    
    if(!is.null(input$net_selected)){
    whichtarg=values$grafAgg2 %>% NN %>% 
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
    
    values$graf <- tbl_graph(x, values$graf %>% EE()) 
    
  })
  
  
  
  # **edge table----
  
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
    
    node.ids <- values$graf %>% NN() %>% mutate(id = row_number()) %>% pull(id)
    
    x <- x %>%
      filter(from %in% node.ids & to %in% node.ids)
    
    values$graf <- tbl_graph(values$graf %>% NN(), x)
  })
  
  
  
  
  output$edgeTable <- renderRHandsontable({
    # browser()
    doNotification("Creating edges table")
    # values$edges=values$edges
    ve <- values$graf %>%
      E_() %>%
      mutate(fromLabel = .N()$label[from], toLabel = .N()$label[to]) %>%
      select(fromLabel, toLabel, -full.quote, everything(), full.quote) %>%
      EE() %>% 
      select(-from,-to,everything())
    
    # vn=values$graf %>% NN 
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
  #   pre(str(values$grafAgg2 %>% EE()))
  # })
  # 
  output$edge2 <- renderRHandsontable(if (!is.null(values$grafAgg2)) {
    doNotification("Creating edges2 table")
    rhandsontable(values$grafAgg2 %>% EE(), height = 700, rowHeaders = FALSE, usetypes = T) %>%
      hot_context_menu(allowRowEdit = T) %>%
      hot_cols(columnSorting = TRUE) %>%
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_rows(fixedRowsTop = 1)
  })
  
  output$node2 <- renderRHandsontable(if (!is.null(values$grafAgg2)) {
    doNotification("Creating nodes2 table")
    rhandsontable(values$grafAgg2 %>% NN(), height = 700, rowHeaders = FALSE, usetypes = T) %>%
      hot_context_menu(allowRowEdit = T) %>%
      hot_cols(columnSorting = TRUE) %>%
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_rows(fixedRowsTop = 1)
  })
  
  
  # Add edges widget----
  
  output$add_edges_widget <- renderUI({
    # varlist <- values$graf %>% NN() %>% pull(label) %>% unique() %>% as.character()
    # vn=values$graf %>% NN()
    # if(nrow(vn)>0){
    # browser()
    #     varlist=
    #       vn %>% 
    #     mutate(id=row_number()) %>% 
    #     left_join(values$graf %>% EE %>% mutate(id=from) %>% select(id),by="id") %>% 
    #     left_join(values$graf %>% EE %>% mutate(id=to) %>% select(id),by="id") %>% 
    #     group_by(id) %>% 
    #     mutate(n=n()) %>% 
    #       summarise(label=first(label)) %>% 
    #     unite("label",sep=": ") %>% 
    #     pull(label) %>% 
    #       as.character()
    #     
    #     
    # } else 
    varlist=values$graf %>% NN() %>% pull(label) %>% unique() %>% as.character()
    varlist <- na.omit(varlist)
    
    ### this will fail if dupliate labels TODO
    # doNotification(paste0("Refreshing add edges widget", input$net_selected))
    
    tagList(
      div(
        # selectizeInput("new1_edge",
        #                label = NULL, selected = if (values$clickArrow) input$net_selected else NULL, multiple = T,
        #                options =
        #                  list(create = T, placeholder = "start typing the name of the variable(s) at the start of the arrow(s)", onInitialize = I('function() { this.setValue(""); }')),
        #                choices = varlist
        # ),
        # selectizeInput("new2_edge",
        #                label = NULL, selected = NULL, multiple = T,
        #                options =
        #                  list(create = T, placeholder = "start typing the name of the variable(s) at the end of the arrow(s)", onInitialize = I('function() { this.setValue(""); }')),
        #                choices = varlist
        # ),
        div(
          actionButton("updateE",
                       "Add",
                       style = "border:1px solid green;padding:15px;display:inline-block;width:7%"
          ),
          actionButton("updateE3",
                       "Add & next",
                       style = "border:1px solid green;padding:15px;display:inline-block;width:15%"
          ),
          
          # actionButton("saveb2", "Save", icon = icon("save"),
          #              style = "border:1px solid green;padding:15px;display:inline-block;width:15%"),
          
          actionButton("updateE2",
                       "Add & chain",
                       style = "border:1px solid green;padding:15px;display:inline-block;width:15%"
          ),
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
  
  # observeEvent(input$clickArrow, { # doesn't work, don't know why
  #   updateSelectizeInput(session, inputId = "new1_edge", selected = "input$net_selected")
  #   values$clickArrow <- T
  #   # doNotification(paste0("Updating first node", input$net_selected))
  # })
  
  
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
      
      if(is.null(values$fromStack) && !is.null(input$net_selected)){
      actionButton("selectFrom","Start arrow(s) with selected variables(s)")
        } else if(!is.null(input$net_selected)){
            actionButton("selectTo","Finish arrow(s) with the selected variable")
          # p(as.character(unlist(values$fromStack)))
        }
    )
  })
  
  
  observeEvent(input$selectFrom,{
    values$fromStack <- input$net_selected
    visNetworkProxy("net") %>% 
      visUpdateNodes(tibble(id=input$net_selected,color.background="gold"))
    
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
      visNetworkProxy("net") %>% 
        visSelectNodes(id=NULL)
  })

# textbox search nodes and highlights them --------------------------------

    observeEvent(c(input$selectboxvalue),{
    if(req(input$sides)=="Code"){

          req(values$grafAgg2)
    
      if(input$selectboxvalue!=""){
    vag <- values$grafAgg2 %>% NN
    
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
      
      values$fromStack <- nrow(values$graf %>% NN)
      doNotification("Added new node",2)
    }
    
  })
  
  
  observeEvent(req(input$pager),{
      vno <- req(values$grafAgg2) %>% NN
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
  
  
  
  
  # output$reportTable1 <- renderTable({
  #   table(mtcars$cyl)
  # })
  
  # output$report <- renderUI({
  #   van=values$grafAgg2 %>% NN %>% 
  #     arrange(desc(frequency))
  #   vae=values$grafAgg2 %>% EE
  #   
  #   tagList(
  #     h3("combined variables, with the most mentioned first")
  #     ,
  #     lapply(1:nrow(van),function(i){
  #       n=van[i,]
  #       tagList(
  #         hr(),
  #         h4(n$label),
  #         paste0("Incoming arrows -- frequency: ",n$to.frequency,"; Outgoing arrows -- frequency: ",n$from.frequency),
  #         paste0("Quotes: ",n$quote) %>% div(style="background-color:whitesmoke") #%>% paste0("Quotes: ",.)
  #       )
  #     })
  #   )
  #   
  # })
  
  
  
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
  
  output$showbigpicture <- renderUI({
    div(
      # div(
      #   # icon("search-plus"),
      #   actionButton("bigpicture", label=icon("search-plus"), width = "30px"),
      #   # checkboxInput("bigpicture", label=icon("search-plus"), width = "30px"),
      #   style = "display:inline-block;margin-left:10px"
      # ),
      # actionButton("widgetsUP","Update")
      div(
        
        actionButton("fontminus", "font-", icon = icon("picture")),
        style = "display:inline-block;margin-right:110px;width:10px"
      ),
      div(
        
        actionButton("fontplus", "font+", icon = icon("picture")),
        style = "display:inline-block;margin-right:80px;width:10px"
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
  
  observe({
    visNetworkProxy("net") %>% 
    visGetNodes(input = "net_nodes",addCoordinates = T)
  })
  
  # observeEvent(input$fontplus,{
  #   browser()
  #   visNetworkProxy("net")$x %>% 
  #     visGetNodes(input = "net_nodes",addCoordinates = T)
  #   
  #   input$net_nodes
  #   visNetworkProxy("net") %>% 
  #     visUpdateNodes(tibble(id=1:20,font.size=22))
  # })
  
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
        # ,
        # 
        # div(
        #   
        #   actionButton("png", "PNG", icon = icon("picture")),
        #   style = "display:inline-block;margin-left:5px;width:10px"
        # ),
        # div(
        #   
        #   actionButton("proxy", "proxy", icon = icon("picture")),
        #   style = "display:inline-block;margin-left:30px;width:10px"
        #)
        
        
        
        # div(radioGroupButtons(inputId = "diagType",status="success",size="xs",choices = xc("Interactive HiQ"),selected = "Interactive",),style="display:inline-block"),
        # uiOutput("filters")
        
        # div(checkboxInput("publicc", "Public", value = T),
        #   style =
        #     "display:inline-block;margin-left:15px;width:10%"
        # ),
      )
      ,
      # downloadButton(outputId = "downloadGraph", label = "Download image"),
      # downloadButton(outputId = "downloadHTML", label = "Download HTML"),
      # downloadButton(outputId = "downloadData", label = "Download data"),
      style = "margin-bottom:-20px"
    )
  ))
  
  cleanr <- function(tex) {
    str_replace(as.character(tex), "\'", "")
  }
  
  
  
  # Edit and observe Node form ----
  # 
  
  output$varForm=renderUI({
    if (length(req(input$net_selected))>0) {
      # browser()
      df <- values$graf %>%
        NN() %>%
        mutate(id = row_number()) %>%
        left_join(values$grafAgg2 %>% NN() %>% select(new = id, id = origID)) # to provide a lo,1>9okup to find original ids if the nodes have been merged
      # df <- (values$graf %>% NN %>% mutate(id=row_number()))
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
  
  # output$formMakr <- renderUI({
  #   browser()
  # 
  # })

  
  observeEvent(input$deleteVarForm, {
    # browser()
    whichtarg=values$grafAgg2 %>% NN %>% 
    filter(row_number()==input$net_selected) %>% 
      pull(origID) 
    
    values$graf <- values$graf %>%
      activate(nodes) %>%
      filter(!(row_number() %in% whichtarg) )
  })
  # 
  # 
  #   edge.id.split <- function(x) x %>% str_split(":") %>% `[[`(1) %>% as.numeric()
  # 
  #   # edit and observe edge form----
  # 
  # output$formMakrE <- renderUI({
  #   if (!is.null(input$current_edge_id)) {
  #     if (length(input$current_edge_id) == 1) {
  #       # browser()
  #       p(input$current_edge_id %>% paste0(collapse=";"))
  #       
  #       df <- values$graf %>%
  #         EE() %>%
  #         mutate(id = row_number())
  #       tagList(
  #         if (!is.null(input$current_edge_id)) {
  #           tagList(
  #             div(
  #               h4(
  #                 paste0("Edit arrow ", input$current_edge_id)
  #               ),
  #               hr(),
  #               lapply(colnames(df %>% select(-id)), function(v) {
  #                 ids <- input$current_edge_id
  #                 div(
  #                   textInput(paste0("updateEdgeForm.", v), v, value = df[df$id == ids, v], width = ifelse(str_detect(v, "label"), 250, 50)),
  #                   style = "display:inline-block"
  #                 )
  #               }),
  #               actionButton("updateEdgeForm", "Update!"),
  #               actionButton("deleteEdgeForm", "Delete!"),
  #               style = "background-color:white;padding:10px;border-radius:5px"
  #             )
  #           )
  #         },
  #         hr()
  #       )
  #     }
  #   }
  # })
  # 
  
  # it would be better to write a general replace_block function for dfs
  replace_block <- function(df, rows, repl) {
    if (sum(rows) > 1) {
      if (!is.data.frame(repl)) stop("repl has to be a df")
      if (length(rows) != nrow(repl)) stop("wrong shape")
      if (ncol(df) != ncol(repl)) stop("wrong shape")
    }
    df[rows, ] <- repl
    df
  }
  
  # # switch to variables----
  # observe({
  #   
  #   if(length(input$net_selected)>0)
  #     updateTabsetPanel(session,inputId = "sides",selected = "Variables")
  # })
  # 
  # # switch to variables----
  # observe({
  #   
  #   if(!is.null(input$current_edge_id))
  #     updateTabsetPanel(session,inputId = "sides",selected = "Arrows")
  # })
  # 
  # observe save button ----
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
        
        nodes=values$graf %>% NN
        edges=values$graf %>% EE
        
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
        
        # saveRDS(values$graf)
        
        # file.copy(paste0("www/", inputtitl,".tm"),paste0("www/", inputtitl,".otm"),overwrite = T)
        doNotification("Saved")
        values$issaved = T
        toggleClass("savemsg", "red")
        delay(500, toggleClass("savemsg", "red"))
        
        
        
      }
    }
  )
  

# observe png -------------------------------------------------------------

  
  
  observeEvent(input$png,{
    
    visSave(values$net(), "www/export.html", selfcontained = T, background = "white")
    webshot::webshot("www/export.html", file = "www/export.png")
  })
  
  # output$downloadGraph <- downloadHandler(
  #   filename = function() {
  #     paste0("Theory Maker - ", values$current, ".html")
  #   },
  #   # make a copy of the file on server to download
  #   content = function(file) {
  #     writeLog(c("Save html: ", session$token, paste0("<a href='", values$current, ".html'>", values$current, "</a> downloaded")))
  #     # makeToC2(tex = input$myText, sess=session$token, save = "png")
  #     # browser()
  #     fn <- paste0(session$token, ".", "html")
  #     visSave(graph = values$net, file = paste0("", inputtitl, ".html"))
  #     
  #     
  #     file.copy(paste0(fn), file)
  #   }
  # )
  # output$HTML <- downloadHandler(
  #   filename = function() {
  #     paste0("Theory Maker - ", values$current, ".png")
  #   },
  #   # make a copy of the file on server to download
  #   content = function(file) {
  #     writeLog(c("Save png: ", session$token, paste0("<a href='", values$current, ".png'>", values$current, "</a> downloaded")))
  #     # makeToC2(tex = input$myText, sess=session$token, save = "png")
  #     # browser()
  #     fn <- paste0(session$token, ".", "png")
  #     
  #     export_graph(values$graph$graph, file_name = fn, file_type = "png", width = 1800)
  #     
  #     file.copy(paste0(fn), file)
  #   }
  # )
  
  observeEvent(input$saveb,{
    output$versions <- renderUI(if (!values$issaved) {
      div()
    } else {
      div(
        id = "savemsg",
        "Saved to this permanent link: ", tags$a(paste0(values$current), href = paste0("?permalink=", values$current)), style = "margin-bottom:10px"
      )
    })
  })
  
  
  
  # file upload ----
  
  
  observeEvent(input$up.nodes, {
    
    # browser()
    req(input$up.nodes)
    df <- read_csv(input$up.nodes$datapath) %>%
      bind_rows(defaultNodes %>% filter(F))
    
    
    values$graf <- tbl_graph(df, values$graf %>% EE())
    doNotification("Updated variables")
  })
  
  id.finder <- function(label, node.df) {
    sapply(label, function(x) {
      (node.df$label == x) %>% which() %>% first()
    })
  }
  
  observeEvent(input$up.edges, {
    req(input$up.edges)
    max <- (values$graf %>% NN() %>% nrow()) + 1
    
    df <- read_csv(input$up.edges$datapath)[, ]
    # browser()
    
    if (input$use.labels) {
      df <- df %>%
        mutate(
          from = id.finder(from, values$graf %>% NN()),
          to = id.finder(to, values$graf %>% NN())
        )
    }
    
    df <- df %>%
      mutate(from = as.numeric(from), to = as.numeric(to)) %>%
      select(one_of(xc("from to label strength trust statement"))) %>%
      filter(from < max & to < max) %>%
      bind_rows(defaultEdges %>% filter(F))
    
    
    
    if (select(df, from, to) %>% unlist() %>% max() > max) doNotification("You have edges which don't make sense",level=2)
    # browser()
    values$graf <- tbl_graph(values$graf %>% NN(), df)
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
  
  
  session$onSessionEnded(function() {
    
  })
  # session$onSessionEnded(stopApp)  ##remember to change this for productionnnnnnnnnnnnnnnnnnnnnnnproductionnnnnnnnnnnnnnnnnnnnnnnproductionnnnnnnnnnnnnnnnnnnnnnnproductionnnnnnnnnnnnnnnnnnnnnnnproductionnnnnnnnnnnnnnnnnnnnnnnproductionnnnnnnnnnnnnnnnnnnnnnn
  
  

# colorLegend -------------------------------------------------------------

  
  output$colourLegend <- renderPlot({
    emptyplot(main = "Percentage of women mentioning each factor and each link                 Percentage of younger people mentioning each factor",adj=0)
    colorlegend(posx = c(0, 0.1), 
      col = intpalette(c("blue", "red"), 100), 
      zlim = c(0, 100), zval = c(0,25,50,75,100))
    
    colorlegend(posx = c(0.5, 0.6), 
      col = intpalette(c("black", "white"), 100), 
      zlim = c(0, 100), zval = c(0,25,50,75,100))
    
  })
  
  
  
  
  ## Gallery ----
  
  # what----
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
  
  output$description=renderUI({
    x=findset("diagramdescription")
    if(x!="")  div(p(x),style="padding:10px;background-color:whitesmoke;margin-top:10px;border-radius:5px")
    else ""
  })
  
  # iframe----
  # output$frame <- renderUI({
  #   tags$iframe(src = "http://www.pogol.net/_help/index.html", height = "900px", width = "100%")
  # })
  # 
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
  
  
  # output$d3 <- renderD3({
  #   r2d3(
  #     runif(5, 0, .9),
  #     script = "js/kill.js"
  #   )
  # })
  
  
  # observeEvent(c(input$bigpicture),{
  #   toggleClass(id="bigpicture",class = "bigpicture2")
  #   toggleClass(id="maindivnet",class = "maindivnet2")
  # })
  
  observe({
    if(input$crowd){output$net2 <- renderVisNetwork({
      doNotification("render viz")
      values$net()
      # browser()
    })}
  })
  
  # output$charts=renderPlotly({
  #   
  #   
  #   nodes=values$grafAgg2 %>% NN
  #   edges=values$grafAgg2 %>% EE
  #   
  #   p <- plot_ly(
  #     type = "sankey",
  #     orientation = "h",
  #     
  #     node = list(
  #       # label = c("A1", "A2", "B1", "B2", "C1", "C2"),
  #       # color = c("blue", "blue", "blue", "blue", "blue", "blue"),
  #       label=nodes$label,
  #       pad = 15,
  #       thickness = 20,
  #       line = list(
  #         color = "black",
  #         width = 0.5
  #       )
  #     ),
  #     
  #     link = list(
  #       source = edges$from,
  #       target = edges$to,
  #       value =  edges$frequency
  #     )
  #   ) %>% 
  #     layout(
  #       title = "Basic Sankey Diagram",
  #       font = list(
  #         size = 10
  #       )
  #     )
  #   p
  # }
  # )
  # 
  # output$chartgg=renderPlot({
  #   values$grafAgg2 %>% 
  #     ggraph(layout = 'sugiyama') + 
  #     geom_edge_link(arrow=arrow(ends="last")) + 
  #     geom_node_point(size = 8, colour = 'steelblue') +
  #     geom_node_text(aes(label = label), colour = 'black', vjust = 0.4) + 
  #     # ggtitle('Joining graphs') + 
  #     theme_graph()
  # })
  # 
  # output$ggplotly=renderPlotly({
  #   ggp=values$grafAgg2 %>% 
  #     ggraph(layout = 'sugiyama') + 
  #     geom_edge_link(arrow=arrow(ends="last"),alpha=.5) + 
  #     geom_node_point(size = 8, colour = 'steelblue',alpha=.5) +
  #     geom_node_text(aes(label = label), colour = 'black', vjust = 0.4) + 
  #     # ggtitle('Joining graphs') + 
  #     theme_graph() 
  #   ggplotly(ggp)
  # })
  # 
  # 
  # output$bar=renderPlotly({
  #   values$grafAgg2 %>% NN %>% 
  #     arrange(frequency) %>% 
  #     plot_ly(x = ~label, y = ~frequency, type = 'bar', name = 'frequency') %>%
  #     layout(yaxis = list(title = 'Count'), barmode = 'group')
  #   
  #   
  #   
  # })
  
  output$pivot <- renderRpivotTable({
    # mtcars$car <- rownames(mtcars)
    doNotification("creating pivot")
    rpivotTable(NN(values$grafAgg2)[,1:20])
  })  
  
  # output$d3 <- renderForceNetwork({
  #   # mtcar
  #   data(MisLinks, MisNodes)
  #   forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
  #                Target = "target", Value = "value", NodeID = "name",
  #                Group = "group", opacity = 0.4)
  # })  
  
  
  # output$dot=renderGrViz({
  #   gr=create_graph(nodes_df=values$grafAgg2 %>% NN %>% 
  #                     mutate(id=row_number()) %>% 
  #                     mutate(fontsize=font.size*6) %>% 
  #                     # mutate(fontcolor=font.color) %>%
  #                     mutate(tooltip=title) %>% 
  #                     select(-cluster),  # note clusters just break the layout
  #                   edges_df=values$grafAgg2 %>% EE %>% 
  #                     # mutate(fontsize=font.size*3) %>% 
  #                     # mutate(fontcolor=font.color) %>% 
  #                     mutate(tooltip=title) %>% 
  #                     select(from,to)) %>% 
  #     add_global_graph_attrs("rankdir", "LR", "graph") %>% 
  #     add_global_graph_attrs("layout","dot","graph") %>% 
  #     add_global_graph_attrs("shape","rectangle","node") %>% 
  #     add_global_graph_attrs("height","0","node") %>% 
  #     add_global_graph_attrs("ratio",as.numeric(findset("diagramHiQratio")),"graph") %>% 
  #     add_global_graph_attrs("fontsize",as.numeric(findset("variablefont.size",global = F)),"node") %>% 
  #     # add_global_graph_attrs("ratio",as.numeric(findset("diagramverticalSpacing"))/as.numeric(findset("diagramhorizontalSpacing")),"graph") %>% 
  #     add_global_graph_attrs("fixedsize","false","node") 
  #   
  #   gr %>% generate_dot %>% grViz()
  #   
  #   
  # })
  
  output$add_edges_widget2 <- renderUI({
    varlist=values$graf %>% NN() %>% pull(label) %>% unique() %>% as.character()
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


# Run the application
shinyApp(ui = ui, server = server)
